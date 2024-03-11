library('ggplot2')
library('ggthemes')
library('tidyverse')

col_palette<-c('black', '#D55E00', '#0072B2', '#009E73')


#Import final FoodNet case-level dataset
#--------------------------------------------------------------------------------
folder<-"//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/Final Datasets/"
dat<-read.csv(paste0(folder, 'analytic_file_final_06162023.csv'))

#Formatting variables for table
#--------------------------------------------------------------------------------
dat <- dat %>%
  mutate(AgeGroup = factor(ifelse(is.na(AgeGroup), 'Unknown', 
                           ifelse(AgeGroup=='0-4' | AgeGroup=='5-17', '0-17', 
                                     AgeGroup)), 
                    levels=c('0-17', '18-29', '30-39', '40-49', '50+', 'Unknown')),
         
         Sex = factor(Sex, levels=c('F', 'M', 'U'), labels=c('Female', 'Male', 'Unknown')),
         
         RacEthGroupA = factor(RacEthGroupA, 
                        levels = c('AmInd-AKNat', 'Asian-NH', 'Black-NH', 'Multiracial-NH',
                                   'NatHwn-PI', 'Other-NH', 'White-NH', 'Hispanic', 'Unknown')),
         
         Species = ifelse(is.na(SpeciesClean), 'unknown', SpeciesClean)
        )


dat %>%
  filter(Species=='flexneri') %>%
  group_by(AgeGroup, Sex, RacEthGroupA) %>%
  count() %>%
  filter(n<10) %>%
  print(n=100)

#Number and percent of cases by shigella species
#--------------------------------------------------------------------------------
t <- rbind(
      dat %>%
      group_by(qrtile) %>%
      count(Species) %>% 
      mutate(Category='SVI Quartile', qrtile=as.character(qrtile), Percent = n/sum(n)) %>%
      rename(Value = qrtile),
      
      dat %>%
        group_by(AgeGroup) %>%
        count(Species) %>% 
        mutate(Category='Age Group', AgeGroup= as.character(AgeGroup), Percent = n/sum(n)) %>%
        rename(Value = AgeGroup),
      
      dat %>%
        group_by(RacEthGroupA) %>%
        count(Species) %>% 
        mutate(Category='Race/Ethnicity', RacEthGroupA= as.character(RacEthGroupA), Percent = n/sum(n)) %>%
        rename(Value = RacEthGroupA),
      
      dat %>%
        group_by(Sex) %>%
        count(Species) %>% 
        mutate(Category='Gender',Sex=as.character(Sex), Percent = n/sum(n)) %>%
        rename(Value = Sex)
  )

#Add totals by category
t <- t %>%
  rbind(t %>%
          group_by(Category, Value) %>%
  summarise(Species='Total', n=sum(n), Percent = n/sum(n))
      )

#Format percentages
#--------------------------------------------------------------------------------
 t <- t %>%
      mutate(N_Percent = 
               paste0(n, 
                      " (", 
                      sprintf("%0.1f", round(Percent*100, 1)), 
                      ")")
             )

#Pivot wider and export to csv
#--------------------------------------------------------------------------------
t %>%
  select(Value, Species, Category, N_Percent) %>%
  pivot_wider(names_from = Species, values_from = N_Percent) %>%
  select(Category, Value, sonnei, flexneri, boydii, dysenteriae, unknown, Total) %>%
  write.csv('table shells/demographics_by_species.csv', row.names = F)

