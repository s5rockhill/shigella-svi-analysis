library('dplyr')
library('epitools')
library('ggthemes')
library('data.table')
library('tidyverse')
library('kableExtra')
library('ggsci')

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
  mutate(AgeStd=cut(Age, breaks=c(0, 5, seq(15, 85, by=10), 999), right=F, labels=AgeLabs))

#--------------------------------------------------------------------------------
# Sum Shigella cases by census tract, standard age categories, and period of 
# diagnosis (sviyear) for each race and for each ethnicity
#--------------------------------------------------------------------------------
t<-dat %>%
  group_by(FIPS, AgeStd, sviyear, Race) %>%
  summarise(Ethnicity='All', Cases=n()) %>%
  rbind(dat %>%
          group_by(FIPS, AgeStd, sviyear, Ethnicity) %>%
          summarise(Race='All', Cases=n()) )

#--------------------------------------------------------------------------------
# Merge tabulated cases with population totals 
#--------------------------------------------------------------------------------
pop <- pop %>%
  left_join(t, join_by(GEOID==FIPS, AgeStd, Race, Ethnicity, sviyear)) %>%
  mutate(Cases = ifelse(is.na(Cases), 0, Cases))

#--------------------------------------------------------------------------------
# Import SVI data and calculate region scores
#--------------------------------------------------------------------------------

source('helper funcs/calc-region-specific-svi-score-tract.R')

#--------------------------------------------------------------------------------
# Link population/case data to SVI data
#--------------------------------------------------------------------------------
pop <- pop %>%
  left_join(select(svi_all, sviyear, FIPS, t1qrtile:qrtile),
            join_by(GEOID==FIPS, sviyear))

#--------------------------------------------------------------------------------
# Calculate age-adjusted rate for each race and ethnicity and SVI Theme Quartile
#--------------------------------------------------------------------------------
pop<-pop %>% mutate(AgeStd=factor(AgeStd), levels=AgeLabs, ordered=T)

std.pop<-data.frame(AgeStd= as.factor(AgeLabs), 
                    Population=c(18986520, 39976619, 38076743, 37233437, 44659185, 
                                 37030152, 23961506, 18135514,  12314793, 4259173))
race <- pop %>%
  filter(!Race %in% c('All', 'U') & !is.na(qrtile)) %>%
  group_by(qrtile, Race, AgeStd) %>%
  summarise(Pop=sum(Pop), Cases=sum(Cases)) %>%
  group_by(qrtile, Race) %>%
  mutate()
  summarise(ageadjust.direct(count=Cases, pop=Pop, stdpop = std.pop))

pop<-pop[, list(pop=sum(estimate), moe=tidycensus::moe_sum(moe, estimate=estimate, na.rm=T)),
         by=c('Year', 'State', 'GEOID', 'AgeStd')]

folder<-"//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/Final Datasets/"
cntydat<-read.csv(paste0(folder, 'Final_County_AgeAdjusted_ByRaceEth.csv'), 
                  colClasses = c('County'='character'))

#formatting variables
racelabels<-c('White, NH','American Indian\n or AK Native', 'Asian or\n Pacific Islander, NH',
              'Black, NH', 'Hispanic*')

setDT(cntydat)[, c('sviyear','LabelRaceEth', 'UrCode')  := 
                 list(factor(sviyear),
                      
                      factor(raceeth, 
                             levels=c('White-NH', 'AmInd-AKNat', 'Asian, NH and PI', 
                                      'Black-NH', 'Hispanic'),
                             labels=racelabels),
                      
                      factor(UrCode, levels=c(1:6), 
                             labels=c(rep('Large metro', 2), 'Medium metro', 'Small metro', rep('Rural',2))))]




#--------------------------------------------------------------------------------
#Table: Summary statistics of rates/proportions by race/svi quartile
#--------------------------------------------------------------------------------
varlist<- c('qrtile', 't1qrtile', 't2qrtile', 't3qrtile', 't4qrtile')
sumstats<-data.frame()
for (i in varlist) {
  temp<- cntydat %>%
    filter(Pop>0) %>%
    group_by(get(i), LabelRaceEth) %>%
    summarise(
          `1+ cases (%)`= sum(AdjCases>0)/n()*100,
           Mean=mean(AdjCases/Pop*100000, na.rm=T),
           Median=quantile(AdjCases/Pop*100000, 0.50),
           Min=min(AdjCases/Pop*100000),
           Max=max(AdjCases/Pop*100000)) %>%
    mutate(Category=i)
  
  names(temp)[1]<-'Group'
  sumstats<-rbind(sumstats, temp)
}

#Format result table
sumtab<-sumstats %>%
  mutate(across(c(`1+ cases (%)`:Max), ~ sprintf("%0.1f", .x)),
    Category=factor(Category,levels=varlist, labels=c('Overall', paste('Theme',1:4))),
    Range = paste(Min, "-", Max)) %>%
  select(Category, LabelRaceEth, Group, `1+ cases (%)`,  Mean, Median, Range) %>%
  arrange(Category, LabelRaceEth, Group) %>%
  filter(!is.na(Group)) %>%
  pivot_wider(id_cols = c(Category, LabelRaceEth), names_from = Group, 
              values_from = c(`1+ cases (%)`, Mean, Median),
              names_vary = 'slowest') 

names(sumtab)<-gsub("_[1-4]|LabelRaceEth", "", names(sumtab))

#Print results table
kbl(sumtab[, 2:14], align =c("l",  rep('r', 13))) %>%
  kable_classic(full_width=T, font_size = 14) %>%
  add_header_above(c(" "=1, rep(c(" "=1, "Rate per 100,000" = 2), 4))) %>%
  add_header_above(c(" "=1, "Quartile 1"=3, "Quartile 2"=3, "Quartile 3"=3, "Quartile 4"=3), bold=T) %>%
  pack_rows('Overall Score', 1, 5) %>%
  pack_rows('Theme 1 Score', 6, 10) %>%
  pack_rows('Theme 2 Score', 11, 15) %>%
  pack_rows('Theme 3 Score', 16, 20) %>%
  pack_rows('Theme 4 Score', 21, 25) %>%
  footnote(symbol = 'Excludes Hispanic American Indian and Alaska Native individuals') %>%
  save_kable('charts/summary_stat_table_bysvitheme.png')

#--------------------------------------------------------------------------------
#Chart: Summary statistics of rates/proportions by race/svi quartile
#--------------------------------------------------------------------------------
sumstats<-data.frame()
for (i in varlist) {
  temp<- cntydat %>%
    filter(Pop>0) %>%
    group_by(get(i), LabelRaceEth, County) %>%
    summarise( Rate = sum(AdjCases)/sum(Pop)*100000) %>%
    mutate(Category=i)
  names(temp)[1]<-'Quartile'
  sumstats<-rbind(sumstats, temp)
}

sumstats$Category<-factor(sumstats$Category,levels=varlist, 
                          labels=c('Overall', 'Theme 1 (Socioeconomic Status)',
                                   'Theme 2 (Household Characteristics)', 
                                   'Theme 3 (Racial and Ethnic Minority Status)', 
                                   'Theme 4 (Housing Type and Transportation)'))
themelist<-unique(sumstats$Category)

plots<-list()
for (i in 1:length(themelist)) {
 plots[[i]]<- sumstats %>%
    filter(Category==themelist[i]) %>%
    ggplot(aes(x=Quartile, y=Rate))+
      facet_grid(cols=vars(LabelRaceEth),  scales='free')+
      stat_summary(fun.data=mean_cl_boot,size=.7)+
      coord_trans(y="log10")+
      theme_light()+
      scale_color_nejm()+
      labs(title = paste0('Average Shigella Incidence Rate by Race and Ethnicity and\n',
                        'Social Vulnerability Index ', themelist[i], ' Quartile'))+ 
           #,subtitle= 'among counties with one or more cases') +
      ylab('Log(Rate per 100K Residents)')+
      xlab('Social Vulnerability Index Quartile')+
      labs(caption = "*Excludes Hispanic American Indian and Alaska Native individuals") +
      theme(legend.position = 'none', 
            plot.caption = element_text(hjust = 0),
            plot.title = element_text(size=14, face='bold', color='#002E40'),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            strip.text=element_text(color='black', size=10, face="bold"))
}  

ggsave('charts/Incidence/Age-adjusted/Incidence_by_Race_and_OverallQuartile.png', plot=plots[[1]], width=8, height=5, units='in')
ggsave('charts/Incidence/Age-adjusted/Incidence_by_Race_and_Theme1Quartile.png', plot=plots[[2]], width=8, height=5, units='in')
ggsave('charts/Incidence/Age-adjusted/Incidence_by_Race_and_Theme2Quartile.png', plot=plots[[3]], width=8, height=5, units='in')
ggsave('charts/Incidence/Age-adjusted/Incidence_by_Race_and_Theme3Quartile.png', plot=plots[[4]], width=8, height=5, units='in')
ggsave('charts/Incidence/Age-adjusted/Incidence_by_Race_and_Theme4Quartile.png', plot=plots[[5]], width=8, height=5, units='in')
