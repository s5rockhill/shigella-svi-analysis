library('dplyr')
library('openxlsx')
library('stringr')

#-------------------------------------------------------------------------------
#Import shigella case data
#-------------------------------------------------------------------------------
setwd('//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/')
dat<-read.csv("FoodNet_NARMS/analytic_file_V5.csv")

#-------------------------------------------------------------------------------
#Fields requested by FoodNet for additional follow-up
#-------------------------------------------------------------------------------
fields<-c('State', 'County', 'Year', 'PersonID', 'PatID', 'SiteIsolateIDnumber', 
          'DtSpec', 'CTNO2000', 'CTNO2010')

#-------------------------------------------------------------------------------
#Cases with non-Shigella genus
#-------------------------------------------------------------------------------
temp1<-dat %>%
  filter(FinalGenusName != 'Shigella') %>%
  select(FinalGenusName, fields)

#-------------------------------------------------------------------------------
#Cases with missing 2010 census tract information
#-------------------------------------------------------------------------------
ctlist<-read.delim('ACS Data/Supplemental Files/2019_Gaz_tracts_national.txt')
ctlist$GEOID<-str_pad(ctlist$GEOID, 11, pad='0')

temp2<-dat %>%
  filter(is.na(CTNO2010) & !(CTNO2000 %in% ctlist$GEOID)) %>%
  select(fields)

#-------------------------------------------------------------------------------
# Output cases for follow-up
#-------------------------------------------------------------------------------
setwd('//cdc.gov/project/ATS_GIS_Store12/Shigella_SVI/Output Datasets For Follow-Up/')

#define sheet names for each data frame
dataset_names <- list('Non_Shigella' = temp1, 'Missing_2010CT' = temp2)

write.xlsx(dataset_names, file = paste0('Records For Follow-Up_', Sys.Date(), '.xlsx') )


