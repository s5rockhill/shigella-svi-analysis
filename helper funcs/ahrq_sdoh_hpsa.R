library(readxl)
library(dplyr)

#Import AHRQ SDOH Databases
#-------------------------------------------------------------------------------
folder<-"//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/AHRQ SDOH Database/"

file.list <- paste0(folder, list.files(path=folder, pattern='*.xlsx'))

df.list <- lapply(file.list, read_excel, sheet='Data')

#Combine datasets
#-------------------------------------------------------------------------------
sdoh<-data.frame()
for (i in seq_along(df.list)) {
  sdoh <- rbind(sdoh,
                df.list[[i]] %>%
                  filter(STATEFIPS %in% c('06','08','09','13','24','27','35','41','47')) %>%
                  select(YEAR, COUNTYFIPS, AHRF_HPSA_PRIM)
  )
}

#Classify data and align with SVI years
#-------------------------------------------------------------------------------
sdoh <- sdoh %>%
  mutate(PrimaryHpsa = factor(AHRF_HPSA_PRIM, levels= c('0', '1', '2'),
                              labels=c('None', 'Whole or part', 'Whole or part')), 
         sviyear = ifelse(YEAR %% 2 == 1, YEAR-1, YEAR)
         ) %>%
  select(County=COUNTYFIPS, PrimaryHpsa, sviyear)