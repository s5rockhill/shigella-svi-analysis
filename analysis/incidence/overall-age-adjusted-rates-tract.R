library('dplyr')
library('ggplot2')
#library('ggthemes')
#library('data.table')
library('tidyverse')
#library('kableExtra')
#library('ggsci')

#--------------------------------------------------------------------------------
#Import census tract population data
#--------------------------------------------------------------------------------
folder<-'//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/FoodNet Population Data/'

#2004-2005 population by 2000 census tracts
fnpop00<-read.csv(
            paste0(folder, 'foodnet_2000tract_population_2004_2005.csv'), 
                  stringsAsFactors = F, colClasses = c("GEOID"='character'))

#2006-2019 population by 2010 census tract
fnpop10<-read.csv(
            paste0(folder, 'foodnet_2010tract_population_2006_2019.csv'), 
                   stringsAsFactors = F, colClasses = c("GEOID"='character'))

#Combine population data 
pop<- fnpop10 %>%
  select(-moe) %>%
  filter(Ethnicity != 'All' | Race != 'All') %>%
  rbind(filter(fnpop00, Ethnicity != 'All' | Race != 'All'))

rm(fnpop00, fnpop10)

#--------------------------------------------------------------------------------
# Sum population data by GEOID, AgeStd, Race, Ethnicity, and the year we want 
# to join the SVI dataset by
#--------------------------------------------------------------------------------
pop <- pop %>%
  mutate(sviyear = ifelse(Year<2006, 2000, 
                      ifelse(Year>2005 & Year<2011, 2010,
                        ifelse(Year>2010 & Year<2015, 2014,
                           ifelse(Year>2014 & Year<2017, 2016, 
                             ifelse(Year>2016, 2018, 0))))) ) %>%
  group_by(GEOID, AgeStd, Race, Ethnicity, sviyear) %>%
  summarise(Pop=sum(estimate))

#--------------------------------------------------------------------------------
# Import Shigella Cases 
#--------------------------------------------------------------------------------
folder<-"//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/Final Datasets/"
dat<-read.csv(paste0(folder, 'analytic_file_final_06162023.csv'), stringsAsFactors = F,
              colClasses = c('FIPS'='character'))

#--------------------------------------------------------------------------------
# Categorize age by standard age categories
#--------------------------------------------------------------------------------
AgeLabs<-c('0-4', '5-14', '15-24', '25-34', '35-44', '45-54', '55-64', '65-74','75-84', '85+')
dat <- dat %>%
  mutate(AgeStd=cut(Age, 
                    breaks=c(0, 5, seq(15, 85, by=10), 999), 
                    right=F, 
                    labels=AgeLabs))

#--------------------------------------------------------------------------------
# Sum Shigella cases by census tract, standard age categories, and period of 
# diagnosis (sviyear) for each race and for each ethnicity
#--------------------------------------------------------------------------------
t<-dat %>%
  filter(!is.na(FIPS) & !is.na(AgeStd)) %>%
  group_by(FIPS, AgeStd, sviyear, Race) %>%
  summarise(Ethnicity='All', Cases=n()) %>%
    rbind(dat %>%
            filter(!is.na(FIPS) & !is.na(AgeStd)) %>%
            group_by(FIPS, AgeStd, sviyear, Ethnicity) %>%
            summarise(Race='All', Cases=n()) )

#--------------------------------------------------------------------------------
# Merge tabulated cases with population totals 
#--------------------------------------------------------------------------------
pop <- pop %>%
  left_join(t, join_by(GEOID==FIPS, AgeStd, Race, Ethnicity, sviyear)) %>%
  mutate(Cases = ifelse(is.na(Cases), 0, Cases))

#--------------------------------------------------------------------------------
# Import SVI data, calculate region-specific scores, and link to pop/case data
#--------------------------------------------------------------------------------
source('helper funcs/calc-region-specific-svi-score-tract.R')

pop <- pop %>%
  left_join(select(svi_all, sviyear, FIPS, t1qrtile:qrtile),
            join_by(GEOID==FIPS, sviyear))

#--------------------------------------------------------------------------------
# Calculate age-adjusted rate for each race and ethnicity and SVI Theme Quartile
# Uses a modification of the ageadjust.direct function from the epiTools package
# that also outputs the variance of the direct standardized rate using the 
#--------------------------------------------------------------------------------
# Load direct age adjustment helper function
source("./helper funcs/ageadjust rate direct method with variance.R.")

# Source U.S. Standard Population (2000) from NCI SEER
stdpop<-read.fwf(url('https://seer.cancer.gov/stdpopulations/stdpop.18ages.txt'),
                 widths = c(3, 3, 8), col.names = c('V1', 'AgeGrp', 'Pop')) %>%
        filter(V1==204) %>%
        mutate(AgeStd=cut(AgeGrp, breaks=(c(0, seq(from=1, to=19, by=2))), labels=AgeLabs)) %>%
        group_by(AgeStd) %>%
        summarise(Pop=sum(Pop))

#Calculate age-adjusted rates, confidence intervals and variance of rates by race
race <- pop %>%
  filter(Race != 'All' & !is.na(qrtile)) %>%
  group_by(qrtile, Category=Race, AgeStd) %>%
  summarise(Pop=sum(Pop), Cases=sum(Cases)) %>%
  group_by(qrtile, Category) %>%
  reframe(Adj.Rate=age.adjust.direct.var(Cases, Pop, stdpop =stdpop$Pop)['adj.rate'],
          lci=age.adjust.direct.var(Cases, Pop, stdpop =stdpop$Pop)['lci'],
          uci=age.adjust.direct.var(Cases, Pop, stdpop =stdpop$Pop)['uci'],
          var=age.adjust.direct.var(Cases, Pop, stdpop =stdpop$Pop)['variance'])

#Calculate age-adjusted rates, confidence intervals and variance of rates by ethnicity
ethn <- pop %>%
  filter(Ethnicity != 'All' & !is.na(qrtile)) %>%
  group_by(qrtile, Category=Ethnicity, AgeStd) %>%
  summarise(Pop=sum(Pop), Cases=sum(Cases)) %>%
  group_by(qrtile, Category) %>%
  reframe(Adj.Rate=age.adjust.direct.var(Cases, Pop, stdpop =stdpop$Pop)['adj.rate'],
          lci=age.adjust.direct.var(Cases, Pop, stdpop =stdpop$Pop)['lci'],
          uci=age.adjust.direct.var(Cases, Pop, stdpop =stdpop$Pop)['uci'],
          var=age.adjust.direct.var(Cases, Pop, stdpop =stdpop$Pop)['variance'])

#--------------------------------------------------------------------------------
# Calculate rate ratios and 95% confidence intervals using white and 
# non-Hispanic as reference categories for race/ethnicity with each quartile
#--------------------------------------------------------------------------------
race <- race %>%
  group_by(qrtile) %>%
  mutate(RR = Adj.Rate/Adj.Rate[Category=='W'], 
         RRvar = ((var/Adj.Rate^2)+(var[Category=='W']/Adj.Rate[Category=='W']^2)),
         RRlci=exp(log(RR)-1.96*sqrt(RRvar)),
         RRuci=exp(log(RR)+1.96*sqrt(RRvar)))

ethn <- ethn %>%
  group_by(qrtile) %>%
  mutate(RR = Adj.Rate/Adj.Rate[Category=='N'], 
         RRvar = ((var/Adj.Rate^2)+(var[Category=='N']/Adj.Rate[Category=='N']^2)),
         RRlci=exp(log(RR)-1.96*sqrt(RRvar)),
         RRuci=exp(log(RR)+1.96*sqrt(RRvar)))

#--------------------------------------------------------------------------------
# Combine race and ethnicity tables and format Category
#--------------------------------------------------------------------------------
# Format race categories and sort
racelabels<-c('White', 'American Indian, AK Native', 'Asian', 'Black', 
              'Multiracial', 'Native Hawaiian, Pacific Islander', 'Other race', 
              'Non-Hispanic', 'Hispanic')

result<-rbind(race, ethn) %>%
      mutate(Category=factor(Category, 
                      levels = c('W', 'I', 'A', 'B', 'M', 'P', 'O', 'N', 'H'),
                      labels = racelabels)) %>%
  arrange(Category)
#--------------------------------------------------------------------------------
# Format and output table
#--------------------------------------------------------------------------------

# Format rates, rate ratios, and confidence intervals for tables
OutTable <- result %>%
      mutate(
        across(Adj.Rate:uci, ~sprintf("%1.2f", round(.x*100000, 3))),
        across(RR:RRuci, ~sprintf( "%1.2f", .x)),
        Rate.CI = paste0("(", lci, '-',  uci, ")"),
        RR.CI = paste0("(",  RRlci, '-', RRuci, ")")
        ) %>%
      select(Quartile=qrtile, Category, Rate=Adj.Rate, Rate.CI, RR, RR.CI) %>%
      pivot_wider(names_from = Quartile,
                  values_from = c(Rate, Rate.CI, RR, RR.CI),
                  names_vary = 'slowest')
write.csv(OutTable, 'charts/Incidence/Age-adjusted/age_adj_rate_aggregate.csv', row.names = F)
#--------------------------------------------------------------------------------
# Charts of rates 
#--------------------------------------------------------------------------------
result %>%
  arrange(desc(Category)) %>%
  mutate(
    across(Adj.Rate:uci, ~round(.x*100000, 3)),
    qrtile = factor(qrtile, levels=1:4, labels=paste('Quartile', 1:4))) %>%
  ggplot(aes(x=Adj.Rate, y=Category)) +
    geom_point(shape=15, size=3) +
    geom_linerange(aes(xmin=lci, xmax=uci)) +
    facet_wrap(~qrtile) +
  labs(title='Age-Adjusted Rate of Shigellosis by Race, Ethnicity and SVI Quartile',
       subtitle = 'FoodNet Data, 2004-2019',
       x = 'Rate per 100K residents')+
  theme_linedraw() +
  theme(plot.title = element_text(hjust=5.7), 
        plot.subtitle = element_text(hjust = -0.5),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(color='gray', linewidth = 0.5),
        panel.grid.minor = element_blank())
  
#--------------------------------------------------------------------------------
# Forest plot of rate ratios
#--------------------------------------------------------------------------------
result %>%
  filter(! Category %in% c('White', 'Non-Hispanic')) %>%
  arrange(desc(Category)) %>%
  mutate(
    qrtile = factor(qrtile, levels=1:4, labels=paste('Quartile', 1:4))) %>%
  ggplot(aes(x=RR, y=Category)) +
  geom_point(shape=15, size=3) +
  geom_linerange(aes(xmin=RRlci, xmax=RRuci)) +
  geom_vline(xintercept = 1, linetype="dashed") +
  facet_wrap(~qrtile) +
  labs(title='Shigellosis Rate Ratio by Race, Ethnicity and SVI Quartile',
       subtitle = 'FoodNet Data, 2004-2019',
       x = 'Rate Ratio')+
  theme_classic() +
  theme(plot.title = element_text(hjust=-3.25), 
        plot.subtitle = element_text(hjust = -0.5),
        axis.title.y = element_blank(),
        strip.background = element_rect('black'),
        strip.text = element_text(color='white'))


