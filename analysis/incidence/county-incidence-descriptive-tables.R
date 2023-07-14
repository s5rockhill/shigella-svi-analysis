library('ggplot2')
library('ggthemes')
library('data.table')
library('tidyverse')
library('kableExtra')
library('ggsci')

#--------------------------------------------------------------------------------
#Import county-level dataset 
#--------------------------------------------------------------------------------
folder<-"//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/Final Datasets/"
cntydat<-read.csv(paste0(folder, 'Final_County_ByRaceEth_2023-07-03.csv'), colClasses = c('County'='character'))

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
#Summary county-level statistics 
#--------------------------------------------------------------------------------

#Number of unique counties
cntydat[, length(unique(County)), by='sviyear']

#Number of unique counties with one or more shigella cases
cntydat[Cases>0, length(unique(County)), by='AgeRange']

#--------------------------------------------------------------------------------
#Summary statistics of rates/proportions by race/svi quartile
#--------------------------------------------------------------------------------
varlist<- c('LabelRaceEth', 'AgeRange', 'qrtile', 't1qrtile', 't2qrtile', 't3qrtile', 't4qrtile')
sumstats<-data.frame()
for (i in varlist) {
  temp<-
    cntydat[Pop>0, list(Category=i,
                        `One or more cases (%)`= sum(Cases>0)/.N*100,
                        Minimum=min(Cases/Pop*100000),
                        `25th percentile`=quantile(Cases/Pop*100000, 0.25),
                        Median=quantile(Cases/Pop*100000, 0.50),
                        `75th percentile`=quantile(Cases/Pop*100000, 0.75),
                        `95th percentile`=quantile(Cases/Pop*100000, .95),
                        Maximum=max(Cases/Pop*100000),
                        Mean=mean(Cases/Pop*100000, na.rm=T)), by=eval(i)]
  names(temp)[1]<-'Group'
  sumstats<-rbind(sumstats, temp)
}

sumstats[, 3:10]<-sumstats[, lapply(.SD, sprintf, fmt="%0.1f"), .SDcols=names(sumstats)[3:10]]
sumstats[, Category := factor(Category, levels=varlist)]
sumstats[,'Group' := factor(Group, levels=c(racelabels, '0-4','5-14', '15-44', '45+', 1:4))]
sumstats<-sumstats[order(Category, Group),]
sumstats<-sumstats[!is.na(Group), ]
names(sumstats)[1]<-''


kbl(sumstats[, c(1, 3:10)], align =c("l",  rep('r', 8))) %>%
  kable_classic(full_width=F, font_size = 12) %>%
  add_header_above(c(" "=2, "Rate per 100,000" = 7), bold=T) %>%
  pack_rows('Race and Ethnicity', 1, 5) %>%
  pack_rows('Age Group', 6, 9) %>%
  pack_rows('Overall SVI Quartile', 10, 13) %>%
  pack_rows('Theme 1 SVI Quartile', 14, 17) %>%
  pack_rows('Theme 2 SVI Quartile', 18, 21) %>%
  pack_rows('Theme 3 SVI Quartile', 22, 25) %>%
  pack_rows('Theme 4 SVI Quartile', 26, 29) %>%
  footnote(symbol = 'Excludes Hispanic American Indian and Alaska Native individuals') %>%
  save_kable('charts/summary_stat_table1.png')

#--------------------------------------------------------------------------------
#Data visualization
#--------------------------------------------------------------------------------
temp<-rbind(cntydat[Cases>0 & Pop>0, list(SVI='Overall', Rate=sum(Cases)/sum(Pop)*100000), 
                       by=list(County, LabelRaceEth, Quartile=qrtile)],
      cntydat[Cases>0 & Pop>0, list(SVI='Theme 1', Rate=sum(Cases)/sum(Pop)*100000), 
              by=list(County, LabelRaceEth, Quartile=t1qrtile)],
      cntydat[Cases>0 & Pop>0, list(SVI='Theme 2', Rate=sum(Cases)/sum(Pop)*100000), 
              by=list(County, LabelRaceEth, Quartile=t2qrtile)],
      cntydat[Cases>0 & Pop>0, list(SVI='Theme 3', Rate=sum(Cases)/sum(Pop)*100000), 
              by=list(County, LabelRaceEth, Quartile=t3qrtile)],
      cntydat[Cases>0 & Pop>0, list(SVI='Theme 4', Rate=sum(Cases)/sum(Pop)*100000), 
              by=list(County, LabelRaceEth, Quartile=t4qrtile)])


  ggplot(temp, aes(x=Quartile, y=Rate))+
  facet_grid(cols=vars(LabelRaceEth), rows=vars(SVI), scales='free')+
  stat_summary(fun.data=mean_cl_boot,size=.7)+
  coord_trans(y="log10")+
  #scale_y_continuous(breaks = range(temp$Rate))+
  theme_light()+
  scale_color_nejm()+
  labs(title = 'Average Shigella Incidence Rate by Social Vulnerability Index Quartile', 
       subtitle= 'among counties with one or more cases') +
  ylab('Log(Rate per 100K Residents)')+
  xlab('Social Vulnerability Index Quartile')+
  labs(caption = "*Excludes Hispanic American Indian and Alaska Native individuals") +
  theme(legend.position = 'none', 
        plot.caption = element_text(hjust = 0),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.text=element_text(color='black', size=10, face="bold"))

ggsave('charts/Incidence_by_Race_and_Quartile.png', width=8, height=5, units='in')
