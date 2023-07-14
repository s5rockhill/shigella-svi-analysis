library('ggplot2')
library('ggthemes')
library('tidyverse')
library('kableExtra')

col_palette<-c('black', '#D55E00', '#0072B2', '#009E73')

#--------------------------------------------------------------------------------
#Import final FoodNet case-level dataset
#--------------------------------------------------------------------------------
folder<-"//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/Final Datasets/"
dat<-read.csv(paste0(folder, 'analytic_file_final_06162023.csv'))

#--------------------------------------------------------------------------------
#Formatting variables
#--------------------------------------------------------------------------------
racelabels<-c('American Indian or AK Native', 'Asian, NH and Pacific Islander', 'Black, NH', 
              'Hispanic', 'Multiracial, NH', 'Other race, NH', 'White, NH', 'Unknown')

AgeGroupLabs<-c('0-4', '5-14', '15-44', '45+')

dat <- dat %>%
  mutate(raceeth=ifelse(RacEthGroupA %in% c('Asian-NH', 'NatHwn-PI'), 'Asian, NH and PI',  RacEthGroupA),
         raceeth=factor(raceeth, levels = c('AmInd-AKNat', 'Asian, NH and PI', 'Black-NH', 
                        'Hispanic', 'Multiracial-NH', 'Other-NH', 'White-NH', 'Unknown'),
                        labels=racelabels),
         AgeGroup = cut(Age, breaks=c(0, 5, 15, 45, 999), right=F, labels=AgeGroupLabs),
         Sex = factor(Sex, levels=c('F', 'M', 'U'), labels=c('Female', 'Male', 'Unknown')))

#--------------------------------------------------------------------------------
#Number and percent of cases by demographics
#--------------------------------------------------------------------------------
total<-dat %>% summarise(Group='Total', N=n(), Percent = sprintf("%0.1f", round(N/nrow(dat)*100, 1)))
        
sex<-dat %>%
  group_by(Group=Sex) %>%
  summarise(N=n(), Percent = sprintf("%0.1f", round(N/nrow(dat)*100, 1))) 

age <-dat %>%
  group_by(Group=AgeGroup) %>%
  summarise(N=n(), Percent = sprintf("%0.1f", round(N/nrow(dat)*100, 1))) %>%
  mutate(Group = ifelse(is.na(Group), 'Unknown', as.character(Group)))

raceeth<-dat %>%
  group_by(Group=raceeth) %>%
  summarise(N=n(), Percent = sprintf("%0.1f", round(N/nrow(dat)*100, 1))) 

state<-dat %>%
  group_by(Group=State) %>%
  summarise(N=n(), Percent = sprintf("%0.1f", round(N/nrow(dat)*100, 1))) 

#--------------------------------------------------------------------------------
#Format table
#--------------------------------------------------------------------------------
t<-rbind(total, sex, age, raceeth, state) 
t$Group = cell_spec(t$Group, bold=ifelse(t$Group=='Total', T, F)) 

t %>%
  rename(' '= Group) %>%
  kbl( align =c("l",  rep('r', 2)), escape = F) %>%
    kable_classic(full_width=F, font_size = 20) %>%
    column_spec(1, width='8cm') %>%
    column_spec(2:3, width='4cm') %>%
    row_spec(0, bold=T) %>%
    pack_rows('Sex', 2, 4) %>%
    pack_rows('Age Group', 5, 9) %>%
    pack_rows('Race and Ethnicity', 10, 17) %>%
    pack_rows('State', 18, 27) %>%
    save_kable('charts/incidence_case_level_demographics.png')

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

