library('data.table')
library('stringr')
library('tidycensus')
library('tidyverse')
library('purrr')

#Set api key 
census_api_key(Sys.getenv("CENSUS_API_KEY"))
#-------------------------------------------------------------------------------
#Get list of all FoodNet counties
#-------------------------------------------------------------------------------
counties <- tidycensus::fips_codes %>%
  dplyr::filter(state %in% c('CT','GA', 'MD', 'MN', 'NM', 'OR', 'TN') |
      (state =='CA' & county_code %in% c('001', '013', '075')) |
      (state =='CO' & county_code %in% c('001', '005', '013', '014', '031', '035', '059')) |
      (state =='NY' & county_code %in% c('001', '003', '009', '013', '015', '019', '021', '025', '029',
                                       '031', '033', '035', '037', '039', '041', '051', '055', '057',
                                       '063', '069', '073', '077', '083', '091', '093', '095', '097', 
                                       '099', '101', '113', '115', '117', '121', '123')))

counties$GEO_ID<-paste0(counties$state_code, counties$county_code)

#-------------------------------------------------------------------------------
# Get list of population count by race/ethnicity, by sex, by age variables
# See https://api.census.gov/data/2010.html for field descriptions
#-------------------------------------------------------------------------------
popvars<-paste0('B01001', rep(c(LETTERS[seq(from=1, to=7)], 'I'), each=28), "_", 
                str_pad(c(3:16, 18:31), 3, side='left', pad="0"), 'E')

#-------------------------------------------------------------------------------
# Define function to loop over counties and get tract-level population data 
#-------------------------------------------------------------------------------
return_acs_dat <- function(vars, year) {
  map2_dfr(
    counties$state_code, counties$county_code,
    ~ get_acs(
      geography = "tract",
      variables = vars,
      state = .x,
      county = .y,
      year = year,
      survey = "acs5",
      geometry = FALSE
    )
  ) 
}

race19<-return_acs_dat(popvars, 2019)
race18<-return_acs_dat(popvars, 2018)
race17<-return_acs_dat(popvars, 2017)

#API call not working for <2019. According to the census developers group, this can 
#happen with very large datasets. We are going to have to do this the old-fashioned way. 

#-------------------------------------------------------------------------------
# Import race-specific CT age x sex population data for 2010-2018
#-------------------------------------------------------------------------------
#Set working directory
setwd('//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/ACS Data/Sex by Age by Race')

#Create list of files
files<-list.files(pattern = "*.csv")

#Import White-Only data (B01001A series)
wh<-files[grep('B01001A', files)]  %>% 
  set_names(nm = (basename(.) %>% tools::file_path_sans_ext())) %>%
  map_df(~fread(., header = T)[-1,], .id = "filename") %>%
  filter(substr(GEO_ID, 10, 14) %in% counties$GEO_ID) %>%
  melt(id.vars=c('filename', 'GEO_ID', 'NAME'))

#Import Black-Only data (B01001B series)
bl<-files[grep('B01001B', files)]  %>% 
  set_names(nm = (basename(.) %>% tools::file_path_sans_ext())) %>%
  map_df(~fread(., header = T)[-1,], .id = "filename") %>%
  filter(substr(GEO_ID, 10, 14) %in% counties$GEO_ID) %>%
  melt(id.vars=c('filename', 'GEO_ID', 'NAME'))

#Import American Indian data (B01001C series)
ai<-files[grep('B01001C', files)]  %>% 
  set_names(nm = (basename(.) %>% tools::file_path_sans_ext())) %>%
  map_df(~fread(., header = T)[-1,], .id = "filename") %>%
  filter(substr(GEO_ID, 10, 14) %in% counties$GEO_ID) %>%
  melt(id.vars=c('filename', 'GEO_ID', 'NAME'))

#Import Asian data (B01001D series)
as<-files[grep('B01001D', files)]  %>% 
  set_names(nm = (basename(.) %>% tools::file_path_sans_ext())) %>%
  map_df(~fread(., header = T)[-1,], .id = "filename") %>%
  filter(substr(GEO_ID, 10, 14) %in% counties$GEO_ID) %>%
  melt(id.vars=c('filename', 'GEO_ID', 'NAME'))

#Import Native Hawaiian/Pacific Islander data (B01001E series)
pi<-files[grep('B01001E', files)]  %>% 
  set_names(nm = (basename(.) %>% tools::file_path_sans_ext())) %>%
  map_df(~fread(., header = T)[-1,], .id = "filename") %>%
  filter(substr(GEO_ID, 10, 14) %in% counties$GEO_ID) %>%
  melt(id.vars=c('filename', 'GEO_ID', 'NAME'))

#Import Other Race data (B01001F series)
or<-files[grep('B01001F', files)]  %>% 
  set_names(nm = (basename(.) %>% tools::file_path_sans_ext())) %>%
  map_df(~fread(., header = T)[-1,], .id = "filename") %>%
  filter(substr(GEO_ID, 10, 14) %in% counties$GEO_ID) %>%
  melt(id.vars=c('filename', 'GEO_ID', 'NAME'))

#Import Two or More Races data (B01001G series)
mr<-files[grep('B01001G', files)]  %>% 
  set_names(nm = (basename(.) %>% tools::file_path_sans_ext())) %>%
  map_df(~fread(., header = T)[-1,], .id = "filename") %>%
  filter(substr(GEO_ID, 10, 14) %in% counties$GEO_ID) %>%
  melt(id.vars=c('filename', 'GEO_ID', 'NAME'))

#Import Hispanic data (B01001I series)
hs<-files[grep('B01001I', files)]  %>% 
  set_names(nm = (basename(.) %>% tools::file_path_sans_ext())) %>%
  map_df(~fread(., header = T)[-1,], .id = "filename") %>%
  filter(substr(GEO_ID, 10, 14) %in% counties$GEO_ID) %>%
  melt(id.vars=c('filename', 'GEO_ID', 'NAME'))

#Import total data (B01001 series)
#Calculate total pop by age and sex for each year and census tract
tl<-files[grep('B01001_', files)]  %>% 
  set_names(nm = (basename(.) %>% tools::file_path_sans_ext())) %>%
  map_df(~fread(., header = T)[-1,], .id = "filename") %>%
  filter(substr(GEO_ID, 10, 14) %in% counties$GEO_ID) %>%
  melt(id.vars=c('filename', 'GEO_ID', 'NAME'))

#-------------------------------------------------------------------------------
# Combine race-specific CT age x sex population data sets
#-------------------------------------------------------------------------------

all<-rbind(wh, bl, ai, as, pi, or, mr, hs)
rm(wh, bl, ai, as, pi, or, mr, hs)

#Add year variable
all$Year<-as.numeric(substr(all$filename, 8, 11))

#Drop unnecessary columns to help speed up processing
all[, c("filename", "NAME"):=NULL]  

#-------------------------------------------------------------------------------
# Pivot combined data set by variable type
#-------------------------------------------------------------------------------
#Add variable type
all$Type<-ifelse(str_sub(all$variable, -1, -1)=='E', 'estimate', 'moe')

# Remove additional age categories (Totals) 
all<-subset(all, !(str_sub(variable, -4, -2) %in% c('001', '002', '017')))

#Remove last character from variable
all[, variable := str_sub(variable, 1, -2)]

all<-dcast(all, GEO_ID + variable + Year ~ Type, value.var = 'value')

#-------------------------------------------------------------------------------
# Combine all dataset (2010-2018) with race19
#-------------------------------------------------------------------------------
all[, GEOID := str_sub(GEO_ID, 10, 20)]
all[, 'GEO_ID':=NULL]
race19$Year<-2019
all<-rbind(all, race19[, c('GEOID', 'Year', 'variable', 'estimate', 'moe')])
rm(race19)

#age groups for standardization
std.groups<-c('0-4','5-14', '15-24', '25-34', '35-44', '45-54', '55-64', '65-74','75-84', '85+')

#Extract age groups, sex, race, ethnicity and state from variable field
all$AgeGroup<-as.numeric(sub(".*_", "", all$variable))
all$AgeStd<-cut(all$AgeGroup, 
                   breaks=c(3,  4,  6,  9,  11, 12, 13, 14, 15, 16,
                            18, 19, 21, 24, 26, 27, 28, 29, 30, 31, 32), right=F, 
                   labels=rep(std.groups, 2))

all$Sex<-ifelse(all$AgeGroup %in% 3:16, 'M', 'F')

all$Race<-factor(substr(all$variable, 7, 7), levels=c(LETTERS[seq(from=1, to=7)], 'I'), 
                 labels=c('W', 'B', 'I', 'A', 'P', 'O', 'M', 'All'))

all$Ethnicity<-ifelse(substr(all$variable, 7, 7)=='I', 'H', 'All')

#-------------------------------------------------------------------------------
# Collapse population estimates into standardize age categories 
#-------------------------------------------------------------------------------
all[, c('estimate', 'moe'):= list(as.numeric(estimate), as.numeric(moe))]

dem_cols<-c('Year', 'GEOID', 'AgeStd', 'Sex', 'Race', 'Ethnicity')
all<-all[,list(estimate=sum(estimate), moe=moe_sum(moe, estimate)), by=dem_cols]

#-------------------------------------------------------------------------------
# Process total population data. This has to be done separately because the total
# population table has more age categories than the race-specific tables.
#-------------------------------------------------------------------------------
tl[, GEOID := str_sub(GEO_ID, 10, 20)]
tl[, 'GEO_ID':=NULL]

#Add year variable
tl$Year<-as.numeric(substr(tl$filename, 8, 11))

#Drop unnecessary columns to help speed up processing
tl[, c("filename", "NAME"):=NULL]  

#Add variable type
tl$Type<-ifelse(str_sub(tl$variable, -1, -1)=='E', 'estimate', 'moe')

# Remove additional age categories (Totals) 
tl<-subset(tl, !(str_sub(variable, -4, -2) %in% c('001', '002', '026')))

#Remove last character from variable
tl[, variable := str_sub(variable, 1, -2)]

# Pivot combined data set by variable type
tl<-dcast(tl, GEOID + variable + Year ~ Type, value.var = 'value')

#Extract age groups, sex, race, ethnicity and state from variable field
tl[, AgeGroup := as.numeric(sub(".*_", "", variable))]
tl$AgeStd<-cut(tl$AgeGroup, 
                breaks=c(3,  4,  6,  11, 13, 15, 17, 20, 23, 25,
                         27, 28, 30, 35, 37, 39, 41, 44, 47, 49, 50), right=F, 
                labels=rep(std.groups, 2))

tl$Sex<-ifelse(tl$AgeGroup %in% 3:25, 'M', 'F')

tl[, c('Race', 'Ethnicity') := list('All', 'All')]

tl[, c('estimate', 'moe'):= list(as.numeric(estimate), as.numeric(moe))]

#Sum by standard age categories
tl<-tl[, list(estimate=sum(estimate), moe=moe_sum(moe, estimate)), by=dem_cols]

#-------------------------------------------------------------------------------
# ACS doesn't have age X sex tables for the Non-Hispanic population, so we will
# subtract the Hispanic population from the total to calculate non-Hispanic
#-------------------------------------------------------------------------------
nh<-merge(tl[, list(Year, GEOID, AgeStd, Sex, Race, total.estimate=estimate, total.moe=moe)],
               all[Ethnicity=='H', list(Year, GEOID, AgeStd, Sex, Race, hisp.total=estimate, hisp.moe=moe)], 
               by=c('Year', 'GEOID', 'AgeStd', 'Sex', 'Race'))

nh[, c('estimate', 'moe', 'Ethnicity') := 
     list(total.estimate-hisp.total, total.moe=sqrt(total.moe^2 + hisp.moe^2), 'N')]

all<-rbind(all, tl,  subset(nh, select=-c(total.estimate, hisp.total, total.moe, hisp.moe)))
rm(tl, nh)

#-------------------------------------------------------------------------------
#Duplicate 2010 data for 2006-2009 and combine with 2010-2019
#-------------------------------------------------------------------------------
acs0609<-all[rep(which(Year==2010), 4), ]
acs0609$Year<-rep(2006:2009, each=nrow(all[which(Year==2010),]))

all<-rbind(all, acs0609)
rm(acs0609)

all$State<-factor(str_sub(all$GEOID, 1, 2), 
                  levels=c('06','08','09','13','24','27','35','36','41','47' ),
                  labels=c('CA', 'CO', 'CT', 'GA', 'MD', 'MN', 'NM', 'NY', 'OR', 'TN'))

#-------------------------------------------------------------------------------
# Output 2010-2019 dataset
#-------------------------------------------------------------------------------
setwd("\\\\cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/FoodNet Population Data/")
write.csv(all, 'foodnet_2010tract_population_2006_2019.csv', row.names = F)

#-------------------------------------------------------------------------------
# Import 2000 population data by race/ethnicity, by sex, by age 
# https://api.census.gov/data/2000/dec/sf1/groups.html
#-------------------------------------------------------------------------------
popvars00<-paste0('P012', 
                  rep(c(tables[1:7], 'H'), each=46), 
                  str_pad(c(3:25, 27:49), 3, side='left', pad="0"))

cenpop<-get_decennial(
  geography='tract', 
  variables=popvars00, 
  state=unique(counties$state_code), 
  year=2000, 
  survey = 'sf1')

setDT(cenpop)

#Drop records not in FoodNet Surveillance area
cenpop<-cenpop[(substr(GEOID, 1, 5) %in% counties$GEO_ID), ]
cenpop$Year<-2004 #We are extrapolating 2000 census data to 2004

#-------------------------------------------------------------------------------
# Extract age groups, sex, race, ethnicity and state from population data
#-------------------------------------------------------------------------------
cenpop$AgeGroup<-as.numeric(str_sub(cenpop$variable, 6, 8))
cenpop$AgeStd<-cut(cenpop$AgeGroup, 
                   breaks=c(3,  4,  6,  11, 13, 15, 17, 20, 23, 25,  
                            27, 28, 30, 35, 37, 39, 41, 44, 47, 49, 50), right=F, 
                   labels=rep(std.groups, 2))

cenpop$Sex<-ifelse(cenpop$AgeGroup %in% 3:25, 'M', 'F')

cenpop$Race<-factor(substr(cenpop$variable, 5, 5), levels=c(tables[1:7], 'H'),
                    labels=c('W', 'B', 'I', 'A', 'P', 'O', 'M', 'All'))

cenpop$Ethnicity<-ifelse(substr(cenpop$variable, 5, 5)=='H', 'H', 'All')

#-------------------------------------------------------------------------------
#Collapse population estimates into standardize age categories 
#-------------------------------------------------------------------------------
cenpop<-cenpop[,list(estimate=sum(value)), by=dem_cols]

#Calculate total pop by age and sex
tl<-cenpop[Ethnicity=='All', list(estimate=sum(estimate), Race='All', Ethnicity='All'), by=c(dem_cols[1:4])]

#-------------------------------------------------------------------------------
#The decennial census doesn't have age X sex tables for the Non-Hispanic population, so
#we will subtract the Hispanic population from the total to calculate non-Hispanic
#-------------------------------------------------------------------------------
hisp<-merge(tl[, list(Year, GEOID, AgeStd, Sex, Race, total.estimate=estimate)],
            cenpop[Ethnicity=='H', list(Year ,GEOID,  AgeStd, Sex, Race, hisp.total=estimate)], 
            by=c(dem_cols[1:5]))

hisp[, c('estimate', 'Ethnicity') := list(total.estimate-hisp.total, 'N')]

cenpop<-rbind(cenpop, tl, subset(hisp, select = -c(total.estimate, hisp.total)))
rm(hisp, tl)

#-------------------------------------------------------------------------------
#Duplicate 2000 census data for 2005
#-------------------------------------------------------------------------------
cenpop05<-cenpop
cenpop05$Year<-2005

#-------------------------------------------------------------------------------
# Combine 2004 and 2005 into single data.frame
#-------------------------------------------------------------------------------
cenpop<-rbind(cenpop, cenpop05)

cenpop$State<-factor(str_sub(cenpop$GEOID, 1, 2), 
                   levels=c('06','08','09','13','24','27','35','36','41','47' ),
                   labels=c('CA', 'CO', 'CT', 'GA', 'MD', 'MN', 'NM', 'NY', 'OR', 'TN'))

#output dataset
write.csv(cenpop, 'foodnet_2000tract_population_2004_2005.csv',row.names = F)
