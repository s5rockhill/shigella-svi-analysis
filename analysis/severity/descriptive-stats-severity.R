library('stringr')
library('openxlsx')
library('ggplot2')
library('ggthemes')
library('kableExtra')
library('dplyr')

col_palette<-c('black', '#D55E00', '#0072B4', '#009E73', '#E69F00', '#56B4E9')
#--------------------------------------------------------------------------------
#Import county-level dataset 
#--------------------------------------------------------------------------------
folder<-"//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/Final Datasets/"
file<-'analytic_file_final_5192023.csv'

dat<-read.csv(paste0(folder, file), stringsAsFactors = F,
              colClasses = c('CTNO2000'='character', 'CTNO2010'='character', 'Deterministic'='character',
                             'CNTYFIPS'= 'character'))

#--------------------------------------------------------------------------------
#Data cleaning
#--------------------------------------------------------------------------------
svibreaks<-c(2000, 2006, 2011, 2015, 2017, 2020)
svilabels<-c('2000', '2010', '2014', '2016', '2018')

dat <- dat %>%
  mutate(FIPS =  ifelse(Year<2006, CTNO2000, CTNO2010), 
         sviyear = as.numeric(as.character(cut(Year, breaks=svibreaks, right=F, labels=svilabels))),
         LabelRace = factor(RaceGroupB, 
                            levels=c('White', 'Am Ind-AK Native', 'Asian-Pac Isldr', 'Black', 'Mltracial', 'Otr and Unkwn'),
                            labels=c('White', 'American Indian and AK Native', 'Asian and Pacific Islander', 
                                     'Black', 'Multiracial', 'Other and unknown')),
         Ethnicity = factor(Ethnicity, levels=c('N', 'H', 'U'), labels=c('non-Hispanic', 'Hispanic', 'Unknown')),
         Sex = factor(Sex, levels=c('F', 'M'), labels=c('Female', 'Male'))) %>%
  mutate(FIPS = ifelse(is.na(FIPS), Deterministic, FIPS))

#-------------------------------------------------------------------------------
# Left-join region-specific SVI scores to cases
#-------------------------------------------------------------------------------
source('helper funcs/calc-region-specific-svi-score-tract.R')

dat<-dat %>% left_join(svi_all, join_by(FIPS, sviyear))

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
# Demographic characteristics of severe and non-severe cases (categorical)
#-------------------------------------------------------------------------------
dat <-dat %>%  #Group years into periods
  mutate(Period = cut(Year, breaks=seq(2004, 2020, by=4), right=F, 
                    labels=c('2004-2007', '2008-2011', '2012-2015', '2016-2019')))

#Frequencies of categorical fields and chi-square test
dem.vars<-c('Sex', 'LabelRace', 'Ethnicity', 'Period', 'UrCode')
d<-data.frame()
cs<-data.frame()
for (i in dem.vars){
  cs<-rbind(cs, cbind(GroupNum=1, Group=i, p=with(dat, chisq.test(table(get(i), Severe))$p.value)))
  d<-rbind(d, cbind(Group=i, with(dat, table(get(i), Severe))))
}

#Format categorical results table
d$Values<-row.names(d)
row.names(d)<-NULL
d[, c('TRUE', 'FALSE')]<-lapply(d[, c('TRUE', 'FALSE')], as.numeric)
d$Severe<-paste0(d$`TRUE`, " (", sprintf("%0.1f", d$`TRUE`/(d$`TRUE`+d$`FALSE`)*100), ")")
d$`Non-Severe`<-paste0(d$`FALSE`, " (", sprintf("%0.1f", d$`FALSE`/(d$`TRUE`+d$`FALSE`)*100), ")")

d<-d %>% select(Group, Values, Severe, `Non-Severe`)

cs <- cs %>% 
  mutate(GroupNum=as.numeric(GroupNum), 
         p = cut(as.numeric(p), breaks=c(0, 0.001, 0.01, 0.05, 1), right=F,
         labels=c("<0.001", "<0.01", '<0.05', ' ')))

d <-d %>% 
  group_by(Group) %>%
  mutate(GroupNum = row_number()) %>%
  ungroup %>%
  left_join(cs, join_by(Group, GroupNum)) %>%
  replace_na(list(p=" ")) %>%
  select(Values, Severe, `Non-Severe`, p) 

names(d)<-c(' ', 'N (Percent)', 'N (Percent)', 'p')

d %>%
  kable(align='lrrr') %>%
    kable_classic(full_width=F, font_size = 14) %>%
    add_header_above(c(' '=1, 'Severe'=1, 'Non-Severe'=1, ' '=1), bold=T, font_size = 16) %>%
    column_spec(1, '5cm') %>%
    column_spec(2:3, '3cm') %>%
    pack_rows('Sex', 1, 2) %>%
    pack_rows('Race', 3, 8) %>%
    pack_rows('Ethnicity', 9, 11) %>%
    pack_rows('Year of Diagnosis', 12, 15) %>%
    pack_rows('Urban-Rural Designation', 16, 19) %>%
    save_kable('charts/severity_demographics_table3_4.png')


#-------------------------------------------------------------------------------
# Demographic characteristics of severe and non-severe cases (continuous)
#-------------------------------------------------------------------------------
age<-data.frame(Field='Age (Yrs)', Group=c('Non-Severe', 'Severe'),
                mean=sprintf("%.2f", t.test(dat$Age ~ dat$Severe)$estimate), 
                sd=sprintf("%.2f", aggregate(Age~Severe, data=dat, FUN='sd', na.rm=T)$Age),
                p=t.test(dat$Age ~ dat$Severe)$p.value)

rpl<-data.frame(Field='SVI Score', Group=c('Non-Severe', 'Severe'),
                mean=sprintf("%.2f", (t.test(dat$rpl ~ dat$Severe)$estimate)), 
                sd=sprintf("%.2f", aggregate(rpl~Severe, data=dat, FUN='sd', na.rm=T)$rpl),
                p=t.test(dat$rpl ~ dat$Severe)$p.value)


c<-bind_rows(age, rpl) %>%
   mutate(p=ifelse(p<0.05, as.character(cut(p, breaks=c(0, 0.001, 0.01, 0.05), right=F,
                           labels=c("<0.001", "<0.01", '<0.05'))), 
                           sprintf("%.4f", p)),
          mean_sd= paste0(mean, " (", sd, ")")) %>%
  select(Field, Group, mean_sd, p) %>%
  pivot_wider(names_from = Group, values_from=c(mean_sd, p)) %>%
  select(Field, mean_sd_Severe, `mean_sd_Non-Severe`, p_Severe)

names(c)<-c(' ', 'Mean (SD)', 'Mean (SD)', 'p')

kable(c) %>%
  kable_classic(full_width=F, font_size = 14) %>%
  add_header_above(c(" "=1, 'Severe'=1, 'Non-Severe'=1, ' '=1), bold = T, font_size = 16) %>%
  column_spec(1, '5cm') %>%
  column_spec(2:3, '3cm') %>%
  save_kable('charts/severe_demographics_continuous.png')

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
