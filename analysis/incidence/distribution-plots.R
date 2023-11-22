library('ggplot2')
library('ggthemes')
library('dplyr')

#--------------------------------------------------------------------------------
#Import county-level dataset 
#--------------------------------------------------------------------------------
folder<-"//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/Final Datasets/"
cntydat<-read.csv(paste0(folder, 'Final_County_AgeAdjusted_ByRaceEth.csv'), 
                  colClasses = c('County'='character'))

#--------------------------------------------------------------------------------
#Format race variables
#--------------------------------------------------------------------------------
racelabels<-c('White, NH','American Indian\n or AK Native', 
              'Asian or\n Pacific Islander, NH',
              'Black, NH', 
              'Hispanic')

cntydat <- cntydat %>%
  mutate(raceeth=factor(raceeth, levels=c('White-NH', 'AmInd-AKNat', 'Asian, NH and PI', 
                  'Black-NH', 'Hispanic'), labels=racelabels))


#--------------------------------------------------------------------------------
#Histogram with density plot
#--------------------------------------------------------------------------------
cntydat %>%
  ggplot(aes(x=AdjCases))+
  geom_histogram(aes(fill=raceeth), binwidth = 20, col='white') +
  #geom_density(aes(fill=raceeth))+
  facet_grid(~raceeth)+
  scale_fill_ptol()+
  theme_classic()+
  theme(legend.position = 'none')

#--------------------------------------------------------------------------------
#Percent of counties with 1 or more cases
#--------------------------------------------------------------------------------
cntydat %>%
  group_by(raceeth, qrtile) %>%
  summarise(Percent = sum(AdjCases>0)/n()) %>%
  ggplot(aes(x=qrtile, y=Percent)) + 
  geom_bar(stat='identity')+
  facet_grid(cols=vars(raceeth)) + 
  scale_y_continuous(limits=c(0,1), labels=scales::percent)+
  labs(title = 'Percent of Counties with One or More Shigella Cases by Race and Ethnicity and\nSocial Vulnerability Index Quartile')+ 
  xlab('Social Vulnerability Index Quartile')+
  theme_light()+
  theme(legend.position='none',
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.text=element_text(color='black', size=10, face="bold"))

