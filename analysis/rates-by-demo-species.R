library(dplyr)

#Import final FoodNet case-level dataset
#--------------------------------------------------------------------------------
folder<-"//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/Final Datasets/"
dat<-read.csv(paste0(folder, 'analytic_file_final_06162023.csv'))

#Format age group
#--------------------------------------------------------------------------------
agegrpfun<-function(x)factor(as.numeric(cut(x, breaks=c(0, 18, 50, 199), right=F)),
                                levels=1:3, labels=c('0-17','18-49','50+'))

dat <- dat %>%
  mutate(Age = ifelse(is.na(Age) & AgeUnit=='Months', 0, 
                ifelse(is.na(Age) & AgeUnit != 'Months' & !is.na(narms_age), narms_age, 
                  Age)),
         AgeGroup=agegrpfun(Age))


#Format species
#--------------------------------------------------------------------------------
dat <- dat %>%
  mutate(Species = ifelse(is.na(SpeciesClean), 'unknown', SpeciesClean))

# Aggreate cases by sex, race/ethnicity, age group for each species excluding
# boydii and dysenteria
#--------------------------------------------------------------------------------
tab <- rbind(
  dat %>% group_by(Sex, AgeGroup, RacEthGroupA) %>%
    summarise(Species='Total', Cases = n()),
  dat %>% 
    filter(Species %in% c('flexneri', 'sonnei')) %>%
      group_by(Sex, AgeGroup, RacEthGroupA) %>%
      summarise(Cases = n())
)


#Import population data for FoodNet Surveillance area
#--------------------------------------------------------------------------------


