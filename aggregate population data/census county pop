library('data.table')
library('stringr')
library('tidycensus')
library('purrr')

#Set api key 
census_api_key(Sys.getenv("CENSUS_API_KEY"))

#select FoodNet states
states<-c("GA", "MD", "OR", "TN", "NM", "CA", "CO", "MN", "NY", "CT")

#age groups for standardization
std.groups<-c('0-4',  '5-14',  '15-24', '25-34', '35-44', '45-54', '55-64', 
              '65-74','75-84', '85+')

#Select years
years<-as.list(2010:2019)
names(years)<-2010:2019

#-------------------------------------------------------------------------------
# Import 2010-2019 population data by race/ethnicity, by sex, by age
# See https://api.census.gov/data/2010.html for field descriptions
#-------------------------------------------------------------------------------
tables<-c(LETTERS[seq(from=1, to=7)], 'I')
popvars<-paste0('B01001', 
                rep(tables, each=28), "_", 
                str_pad(c(3:16, 18:31), 3, side='left', pad="0"), 'E')

#Pulling all data at county level. Will drop out counties not in FoodNet area later
acsdat<-map_dfr(
  years,
  ~get_acs(geography='county', variables=popvars, state=states, year=.x, survey = 'acs5'),
  .id='year')

setDT(acsdat)

# Extract age groups, sex, race, ethnicity and state from variable field
acsdat$AgeGroup<-as.numeric(sub(".*_", "", acsdat$variable))
acsdat$AgeStd<-cut(acsdat$AgeGroup, 
                   breaks=c(3,  4,  6,  9,  11, 12, 13, 14, 15, 16,
                            18, 19, 21, 24, 26, 27, 28, 29, 30, 31, 32), right=F, 
                   labels=rep(std.groups, 2))

acsdat$Sex<-ifelse(acsdat$AgeGroup %in% 3:16, 'M', 'F')

acsdat$Race<-factor(substr(acsdat$variable, 7, 7), levels=tables,
                    labels=c('W', 'B', 'I', 'A', 'P', 'O', 'M', 'All'))

acsdat$Ethnicity<-ifelse(substr(acsdat$variable, 7, 7)=='I', 'H', 'All')

#Collapse population estimates into standardize age categories 
acsdat<-acsdat[,list(estimate=sum(estimate)), by=list(year, GEOID, AgeStd, Sex, Race, Ethnicity)]

#Calculate total pop by age and sex for each year and county
total<-acsdat[Ethnicity=='All', list(estimate=sum(estimate), Race='All', Ethnicity='All'), 
              by=list(year, GEOID, AgeStd,  Sex)]

#The census doesn't have age X sex tables for the Non-Hispanic population, so we are going 
#to have to subtract the Hispanic population from the total to calculate non-Hispanic
hispanic<-merge(total[, list(year, GEOID, total.estimate=estimate, AgeStd, Sex)],
                acsdat[Ethnicity=='H', list(year, GEOID, hispanic.total=estimate, AgeStd, Sex)], 
                by=c('year', 'GEOID', 'AgeStd', 'Sex'))

hispanic[, c('estimate', 'Race', 'Ethnicity') := list(total.estimate-hispanic.total, 'All', 'N')]
acsdat<-rbind(acsdat, total,  subset(hispanic, select = -c(total.estimate, hispanic.total)))

#-------------------------------------------------------------------------------
# Import 2000 population data by race/ethnicity, by sex, by age 
# https://api.census.gov/data/2000/dec/sf1/groups.html
#-------------------------------------------------------------------------------
popvars00<-paste0('P012', 
                  rep(c(tables[1:7], 'H'), each=46), 
                  str_pad(c(3:25, 27:49), 3, side='left', pad="0"))

cenpop<-get_decennial(geography='county', variables=popvars00, state=states, year=2000, survey = 'sf1')
setDT(cenpop)

# Extract age groups, sex, race, ethnicity and state from population data
cenpop$AgeGroup<-as.numeric(str_sub(cenpop$variable, 6, 8))
cenpop$AgeStd<-cut(cenpop$AgeGroup, 
                   breaks=c(3,  4,  6,  11, 13, 15, 17, 20, 23, 25,  
                            27, 28, 30, 35, 37, 39, 41, 44, 47, 49, 50), right=F, 
                   labels=rep(std.groups, 2))

cenpop$Sex<-ifelse(cenpop$AgeGroup %in% 3:26, 'M', 'F')

cenpop$Race<-factor(substr(cenpop$variable, 5, 5), levels=c(tables[1:7], 'H'),
                    labels=c('W', 'B', 'I', 'A', 'P', 'O', 'M', 'All'))

cenpop$Ethnicity<-ifelse(substr(cenpop$variable, 5, 5)=='H', 'H', 'All')

#Collapse population estimates into standardize age categories 
cenpop<-cenpop[,list(estimate=sum(value)), by=list(GEOID, AgeStd, Sex, Race, Ethnicity)]

#Calculate total pop by age and sex
total<-cenpop[Ethnicity=='All', list(estimate=sum(estimate), Race='All', Ethnicity='All'), 
              by=list(GEOID, AgeStd, Sex)]

#The decennial census doesn't have age X sex tables for the Non-Hispanic population, so
#we have to subtract the Hispanic population from the total to calculate non-Hispanic
hispanic<-merge(total[, list(GEOID, total.estimate=estimate, AgeStd, Sex)],
                cenpop[Ethnicity=='H', list(GEOID,  hispanic.total=estimate, AgeStd, Sex)], 
                by=c('GEOID',  'AgeStd', 'Sex'))

hispanic[, c('estimate', 'Race', 'Ethnicity') := list(total.estimate-hispanic.total, 'All', 'N')]
cenpop<-rbind(cenpop, total, subset(hispanic, select = -c(total.estimate, hispanic.total)))

#-------------------------------------------------------------------------------
#Duplicate 2000 census data for 2005 and 2010 data for 2006-2009
#-------------------------------------------------------------------------------
cenpop05<-cenpop
cenpop$year<-2004 #We are extrapolating 2000 census data to 2004
cenpop05$year<-2005

acs0609<-acsdat[rep(which(year==2010), 4), ]
acs0609$year<-rep(2006:2009, each=nrow(acsdat[which(year==2010),]))

#-------------------------------------------------------------------------------
# Combine all years of data into single data.frame
#-------------------------------------------------------------------------------
pop<-rbind(cenpop, cenpop05, acs0609, acsdat)

pop$State<-factor(str_sub(pop$GEOID, 1, 2), 
                   levels=c('06','08','09','13','24','27','35','36','41','47' ),
                     labels=c('CA', 'CO', 'CT', 'GA', 'MD', 'MN', 'NM', 'NY', 'OR', 'TN'))

pop$County<-str_sub(pop$GEOID, 3, 5)

#-------------------------------------------------------------------------------
# Keep only counties in surveillance area
#-------------------------------------------------------------------------------
pop<-subset(pop, State %in% c('CT','GA', 'MD', 'MN', 'NM', 'OR', 'TN') |
                      (State =='CA' & County %in% c('001', '013', '075')) |
                      (State =='CO' & County %in% c('001', '005', '013', '014', '031', '035', '059')) |
                      (State =='NY' & County %in% c('001', '003', '009', '013', '015', '019', '021', '025', '029',
                                                    '031', '033', '035', '037', '039', '041', '051', '055', '057',
                                                    '063', '069', '073', '077', '083', '091', '093', '095', '097', 
                                                    '099', '101', '113', '115', '117', '121', '123')))

#Check population totals. According to FoodNet website,  surveillance area covers 
#roughly 50 million in 2018
pop[Race=='All' & Ethnicity=='All', sum(estimate), by=year]

#output dataset
write.csv(pop, '//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/population_est_by_age_sex.csv',
          row.names = F)