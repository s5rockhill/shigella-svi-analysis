library('stringr')
library('openxlsx')
library('ggplot2')
library('ggthemes')
library('kableExtra')
library('dplyr')
library('tidycensus')
library('tidyr')
library('colorspace')

col_palette<-c('black', '#D55E00', '#0072B4', '#009E73', '#E69F00', '#56B4E9')
#--------------------------------------------------------------------------------
#Import case-level dataset 
#--------------------------------------------------------------------------------
folder<-"//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/Final Datasets/"
dat<-read.csv(paste0(folder, 'analytic_file_final_06062023.csv'), stringsAsFactors = F,
              colClasses = c('CNTYFIPS'='character'))

#--------------------------------------------------------------------------------
# Factor categorical variables
#--------------------------------------------------------------------------------
dat <- dat %>%
  mutate(Race = factor(Race, levels=c('W', 'I', 'A', 'B', 'M', 'P', 'O', 'U' ),
                       labels=c('White', 'American Indian and AK Native', 
                                'Asian and Pacific Islander', 'Black', 'Multiracial',
                                'Pacific Islander', 'Other Race', 'Unknown Race')),
         LabelRace = factor(RacEthGroupA, 
                levels=c('White-NH', 'AmInd-AKNat', 'Asian-NH', 'Black-NH', 'Hispanic', 
                         'Multiracial-NH', 'NatHwn-PI',   'Other-NH',  'Unknown'),
                labels=c('White, non-Hispanic', 
                         'American Indian and AK Native', 
                         'Asian, non-Hispanic', 
                         'Black, non-Hispanic', 
                         'Hispanic',
                         'Multiracial, non-Hispanic', 
                         'Native Hawaiian and Pacific Islander',
                         'Other, non-Hispanic',
                         'Unknown')),
         Sex = factor(Sex, levels=c('F', 'M'), labels=c('Female', 'Male'))) 

#-------------------------------------------------------------------------------
# Update CNTYFIPS Code based on reported County if missing
#-------------------------------------------------------------------------------

data(fips_codes)
fips_codes$county<-toupper(gsub(' County',"", fips_codes$county))
dat$County[dat$County=='PRINCE GEORGES']<-"PRINCE GEORGE'S"

dat <- dat %>%
  left_join(fips_codes, join_by(State==state, County==county)) %>%
  mutate(CNTYFIPS=ifelse(is.na(CNTYFIPS), paste0(state_code, county_code), CNTYFIPS))

#-------------------------------------------------------------------------------
#Import NCHS Urban-Rural classification and merge to data by STCNTY
#-------------------------------------------------------------------------------
folder<-'//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/NCHS/'
nchs<-read.xlsx(paste0(folder, 'NCHSURCodes2013.xlsx'))
nchs$CNTYFIPS<-str_pad(nchs$FIPS.code, 5, "left", "0")
nchs$UrCode<-nchs$`2013.code`

dat <- dat  %>%
      left_join(nchs[, c('CNTYFIPS', 'UrCode')], join_by(CNTYFIPS)) %>%
      mutate(UrCode = factor(UrCode, levels=c(1:6), 
                          labels=c(rep('Large metro', 2), 'Medium metro', 'Small metro', rep('Rural',2))))
      
#-------------------------------------------------------------------------------
# Demographics of cases by severity and by antimicrobial resistance (any/none) 
#-------------------------------------------------------------------------------
dat <-dat %>%  #Group years into periods
  mutate(Period = cut(Year, breaks=seq(2004, 2020, by=4), right=F, 
                    labels=c('2004-2007', '2008-2011', '2012-2015', '2016-2019')))

#Frequencies of categorical fields and chi-square test
dem.vars<-c('Sex', 'LabelRace', 'Period', 'UrCode')

d<-data.frame()

for (i in dem.vars){
  d<-rbind(d, cbind(Group=i, 
                       with(dat, table(get(i), Severe)),
                        p_sev=with(dat, chisq.test(table(get(i), Severe))$p.value),
                       with(dat, table(get(i), AbxResSum>0)),
                        p_amr=with(dat, chisq.test(table(get(i), AbxResSum>0))$p.value))
      )
} 

#-------------------------------------------------------------------------------  
#Format and output categorical results table
#-------------------------------------------------------------------------------
d[, 2:7]<-lapply(d[, 2:7], as.numeric)
names(d)[2:7]<-c("NS", 'S', 'p_sev',  "NR", "R", 'p_amr')

plabels<-c("<0.001", "<0.01", '<0.05', ' ')

d<-d %>%
  mutate(
        across(c(p_sev, p_amr), \(x) cut(as.numeric(x), breaks=c(0, 0.001, 0.01, 0.05, 1), 
                                         right=F, labels=plabels)),
         Values=row.names(.),
         Severe = paste0(S, " (", sprintf("%0.1f", S/(S+NS)*100), ")"),
         NonSevere= paste0(NS, " (", sprintf("%0.1f", NS/(S+NS)*100), ")"),
         Resistant= paste0(R, " (", sprintf("%0.1f", R/(R+NR)*100), ")"),
         NotResistant= paste0(NR, " (", sprintf("%0.1f", NR/(R+NR)*100), ")")) %>%
  select(Group, Values, Severe, NonSevere, p_sev, Resistant, NotResistant, p_amr )

row.names(d)<-NULL

d<- d %>%
  group_by(Group) %>%
  mutate(across(c(p_sev, p_amr), \(x) ifelse(row_number()==1, as.character(x), "")))%>%
  ungroup()%>%
  select(-Group)

names(d)<-c(' ', rep(c('N (Percent)', 'N (Percent)', 'p'), 2))

d %>%
  kable(align='lrrrrrr') %>%
    kable_classic(full_width=F, font_size = 14) %>%
    add_header_above(c(' '=1, 'Yes'=1, 'No'=1, ' '=1, 'Yes'=1, 'No'=1, ' '=1), 
                     bold=T, font_size = 14) %>%
  add_header_above(c(' '=1, 'Severe Shigella\n(N=38930)'=3,  
                     'Reistant to One or More Antimicrobials\n(N=1252)'=3), bold=T, font_size = 16) %>%
    column_spec(1, '6cm') %>%
    column_spec(2:3, '3cm') %>%
    column_spec(5:6, '3cm') %>%
    pack_rows('Sex', 1, 2) %>%
    pack_rows('Race and Ethnicity', 3, 11) %>%
    pack_rows('Year of Diagnosis', 12, 15) %>%
    pack_rows('Urban-Rural Designation', 16, 19) %>%
    save_kable('charts/case_level_demographics_table3_4.png')


#-------------------------------------------------------------------------------
# Continuous covariates (SVI scores) by severity and ABX resistance
#-------------------------------------------------------------------------------

dat %>%
  select(Severe, rpl, t1rpl, t2rpl, t3rpl, t4rpl) %>%
  gather(key="SVI Theme", value="Score",  -Severe) %>%
  mutate(`SVI Theme` = recode(`SVI Theme`,  'rpl' = 'Overall','t1rpl' = 'Theme 1',
                                            't2rpl' = 'Theme 2','t3rpl' = 'Theme 3',
                                            't4rpl' = 'Theme 4')) %>%
  ggplot(aes(factor(`SVI Theme`), Score, Group=Severe)) +
  geom_boxplot(aes(fill=Severe))+
  #geom_violin(draw_quantiles = 0.50, bw=0.05, aes(fill=Severe)) +
  scale_fill_discrete_qualitative(palette = "Dark 3")+
  labs(title='Social Vulnerability Index Score by Shigella Severity',
       caption='Theme 1 = socioeconomic status \nTheme 2 = household composition \nTheme 3 = minority status and language \nTheme 4 = housing and transportation')+
  theme_light()+
  theme(axis.title.x = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption = element_text(hjust = 0, face = 'italic', size=9))
ggsave('charts/SVI_Score_by_Severity_3A.png', width = 8, height = 5, units='in')


dat %>%
  filter(SubPop==T) %>%
  mutate(`Antimicrobial Resistance` = ifelse(AbxResSum>0, T, F)) %>%
  select(`Antimicrobial Resistance`, rpl, t1rpl, t2rpl, t3rpl, t4rpl) %>%
  gather(key="SVI Theme", value="Score",  -`Antimicrobial Resistance`) %>%
  mutate(`SVI Theme` = recode(`SVI Theme`,  'rpl' = 'Overall','t1rpl' = 'Theme 1',
                              't2rpl' = 'Theme 2','t3rpl' = 'Theme 3',
                              't4rpl' = 'Theme 4')) %>%
  ggplot(aes(factor(`SVI Theme`), Score, Group=`Antimicrobial Resistance`)) +
  geom_boxplot(aes(fill=`Antimicrobial Resistance`))+
  scale_fill_discrete_qualitative(palette = "Dark 3")+
  labs(title='Social Vulnerability Index Score by Antimicrobial Resistance',
       caption='Theme 1 = socioeconomic status \nTheme 2 = household composition \nTheme 3 = minority status and language \nTheme 4 = housing and transportation')+
  theme_light()+
  theme(axis.title.x = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption = element_text(hjust = 0, face = 'italic', size=9))
ggsave('charts/SVI_Score_by_AbxRes_3A.png', width = 8, height = 5, units='in')


#-------------------------------------------------------------------------------
# Proportion of cases having severe shigella by race/ethnicity and SVI Quartile
#-------------------------------------------------------------------------------
#Figure 3.4A
quartile.labs <- c('1: Least Vulnerable', '2', '3', '4: Most Vulnerable')
names(quartile.labs) <- c("1", "2", "3", "4")

dat %>%
  filter(!is.na(quartile)) %>%
  group_by(LabelRace, quartile) %>%
  summarise(Total=n(), Severe=sum(Severe==T)) %>%
  mutate(Percent= Severe/Total, 
         LabelRace=ifelse(LabelRace=='American Indian and AK Native', 'American Indian\n and AK Native',
                          as.character(LabelRace))) %>%
  ggplot( aes(x=LabelRace, y=Percent))+
    geom_bar(stat='identity', fill='#0072B2') + 
    facet_grid(rows=vars(quartile), labeller=labeller(quartile=quartile.labs))+
    geom_text(aes(label=sprintf("%.1f%%", Percent*100), y=Percent), stat="identity", vjust=-.5) +
    scale_y_continuous(limits=c(0,1), labels = scales::percent) +
    ylab("Percent of Cases") +
    labs(title='Percent of Shigella Cases Classified as Severe by Race and SVI Quartile')+
    theme_light()+
    theme(axis.title.x = element_blank(), panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          strip.background =element_rect(fill='gray25'))
ggsave('charts/Percent_Severe_by_Race_figure3_4A.png', width = 8, height = 7, units='in')



#Table 3.4A
t1<-
dat %>%
  filter(!is.na(quartile)) %>%
  group_by(LabelRace, quartile) %>%
  summarise(Total=n(), Severe=sum(Severe==T)) %>%
  mutate(Percent= sprintf("%0.1f", Severe/Total*100)) %>%
  pivot_wider(names_from=quartile, values_from=c(Total, Severe, Percent), names_vary="slowest") 
  
  
colnames(t1)<-c("", rep(c('Total (N)', 'Severe (N)', '%'), times=4))

kbl(t1, align =c("l",  rep('r', 12))) %>%
  kable_classic(full_width=F, font_size = 14) %>%
  add_header_above(c(" "=1, "Quartile 1"=3, "Quartile 2"=3, "Quartile 3"=3, "Quartile 4"=3), 
                   bold=T, font_size = 16) %>%
  column_spec(1, '4.5cm') %>%
  #as_image(width = 8, height = 4)
  save_kable('charts/percent_severe_table3_4A.png')
