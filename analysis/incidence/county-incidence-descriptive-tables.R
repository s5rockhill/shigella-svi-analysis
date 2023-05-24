library('ggplot2')
library('ggthemes')
library('data.table')
library('tidyverse')
library('kableExtra')

col_palette<-c('black', '#D55E00', '#0072B2', '#009E73')
#--------------------------------------------------------------------------------
#Import county-level dataset 
#--------------------------------------------------------------------------------
folder<-"//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/Final Datasets/"
cntydat<-read.csv(paste0(folder, 'Final_County_ByRaceEth_2023-05-24.csv'))

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
#Summary statistics of rates/proportions by race/svi quartile
#--------------------------------------------------------------------------------
sumstats<-data.frame()
for (i in c('LabelRaceEth', 'quartile', 'AgeRange')) {
  temp<-
    cntydat[Pop>9, list(`One or more cases (%)`= sum(Cases>0)/.N*100,
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

sumstats[, 2:9]<-sumstats[, lapply(.SD, sprintf, fmt="%0.1f"), .SDcols=names(sumstats)[-1]]
sumstats[,'Group' := factor(Group, levels=c(racelabels, 1:4, '0-4','5-14', '15-44', '45+'))]
sumstats<-sumstats[order(Group),]
sumstats<-sumstats[!is.na(Group), ]
names(sumstats)[1]<-''


kbl(na.omit(sumstats), align =c("l",  rep('r', 8))) %>%
  kable_classic(full_width=F, font_size = 12) %>%
  add_header_above(c(" "=2, "Rate per 100,000" = 7), bold=T) %>%
  pack_rows('Race and Ethnicity', 1, 5) %>%
  pack_rows('SVI Quartile', 6, 9) %>%
  pack_rows('Age Group', 10, 13) %>%
  footnote(symbol = 'Excludes Hispanic American Indian and Alaska Native individuals') %>%
  save_kable('analysis/incidence/charts/summary_stat_table1.png')

#--------------------------------------------------------------------------------
#Data visualization
#--------------------------------------------------------------------------------
cntydat[Cases>0, list(Rate=sum(Cases)/sum(Pop)*100000), by=list(County, LabelRaceEth, quartile)] %>%
  ggplot(aes(x=quartile, y=Rate))+
  facet_grid(cols=vars(LabelRaceEth), scales='free')+
  stat_summary(fun.data=mean_cl_boot,size=.7, color='#0072B2')+
  coord_trans(y="log10")+
  theme_light()+
  labs(title = 'Average Shigella Incidence Rate by Social Vulnerability Index Quartile', 
       subtitle= 'among counties with one or more cases') +
  ylab('Log(Rate per 100K Residents)')+
  xlab('Social Vulnerability Index Quartile')+
  labs(caption = "*Excludes Hispanic American Indian and Alaska Native individuals") +
  theme(legend.position = 'none', 
        plot.caption = element_text(hjust = 0),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.text.x=element_text(color='black', size=10, face="bold"))

ggsave('analysis/incidence/charts/Incidence_by_Race_and_Quartile.png', width=8, height=5, units='in')
