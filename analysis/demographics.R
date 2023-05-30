library('ggplot2')
library('ggthemes')
library('tidyverse')
library('kableExtra')

col_palette<-c('black', '#D55E00', '#0072B2', '#009E73')

#--------------------------------------------------------------------------------
#Import final FoodNet case-level dataset
#--------------------------------------------------------------------------------
folder<-"//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/Final Datasets/"
dat<-read.csv(paste0(folder, 'analytic_file_final_5192023.csv'))

#--------------------------------------------------------------------------------
#Formatting variables
#--------------------------------------------------------------------------------
racelabels<-c('American Indian or AK Native', 'Asian', 'Black', 
              'Native Hawaiian or Pac. Islander', 'White', 'Multiracial', 
              'Other race', 'Unknown')

dat <- dat %>%
  mutate(RaceGroupC = factor(Race, levels=c('I', 'A', 'B','P', 'W', 'M', 'O', 'U'),
                          labels=racelabels),
         AgeGroup = factor(ifelse(is.na(AgeGroup), 'Unknown', AgeGroup), 
                           levels=c('0-4','5-17', '18-29', '30-39', '40-49', '50+', 'Unknown')),
         Sex = factor(Sex, levels=c('F', 'M', 'U'), labels=c('Female', 'Male', 'Unknown')),
         Ethnicity = factor(Ethnicity, levels=c('H', 'N', 'U'), labels=c('Hispanic', 'non-Hispanic', 'Unknown')))

#--------------------------------------------------------------------------------
#Number and percent of cases by demographics
#--------------------------------------------------------------------------------
total<-dat %>% summarise(Group='Total', N=n(), Percent = sprintf("%0.1f", round(N/nrow(dat)*100, 1)))
        
sex<-dat %>%
  group_by(Group=Sex) %>%
  summarise(N=n(), Percent = sprintf("%0.1f", round(N/nrow(dat)*100, 1))) 

age <-dat %>%
  group_by(Group=AgeGroup) %>%
  summarise(N=n(), Percent = sprintf("%0.1f", round(N/nrow(dat)*100, 1))) 

race<-dat %>%
  group_by(Group=RaceGroupC) %>%
  summarise(N=n(), Percent = sprintf("%0.1f", round(N/nrow(dat)*100, 1))) 

ethn<-dat %>%
  group_by(Group=Ethnicity) %>%
  summarise(N=n(), Percent = sprintf("%0.1f", round(N/nrow(dat)*100, 1))) 

state<-dat %>%
  group_by(Group=State) %>%
  summarise(N=n(), Percent = sprintf("%0.1f", round(N/nrow(dat)*100, 1))) 

#--------------------------------------------------------------------------------
#Format table
#--------------------------------------------------------------------------------
t<-rbind(total, sex, age, race, ethn, state) 
t$Group = cell_spec(t$Group, bold=ifelse(t$Group=='Total', T, F)) 

t %>%
  rename(' '= Group) %>%
  kbl( align =c("l",  rep('r', 2)), escape = F) %>%
    kable_classic(full_width=F, font_size = 20) %>%
    column_spec(1, width='8cm') %>%
    column_spec(2:3, width='4cm') %>%
    row_spec(0, bold=T) %>%
    pack_rows('Sex', 2, 4) %>%
    pack_rows('Age Group', 5, 11) %>%
    pack_rows('Race', 12, 19) %>%
    pack_rows('Ethnicity', 20, 22) %>%
    pack_rows('State', 23, 32) %>%
    save_kable('charts/case_level_demographics.png')

#--------------------------------------------------------------------------------
#Additional statistics
#--------------------------------------------------------------------------------
dat %>%
  summarise(median(floor(Age), na.rm=T), 
            min(floor(Age), na.rm=T), 
            max(floor(Age), na.rm = T))

dat %>%
  group_by(SpeciesClean) %>%
  summarise(N=n(), Percent = sprintf("%0.1f", round(N/nrow(dat)*100, 1))) 

