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
#Summary county-level statistics 
#--------------------------------------------------------------------------------

#Number of unique counties
cntydat[, length(unique(County)), by='sviyear']

#Number of counties with one or more age-adjusted expected shigella cases 
cntydat[AdjCases>0, length(unique(County)), by='sviyear']

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
