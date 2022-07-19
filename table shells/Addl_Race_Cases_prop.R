library('data.table')
library('stringr')
library('tidycensus')
library('openxlsx')
library('purrr')

setwd('//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/')
census_api_key(Sys.getenv("CENSUS_API_KEY"))

#-------------------------------------------------------------------------------
# Import Shigella case data
#-------------------------------------------------------------------------------
dat<-setDT(read.csv("FoodNet_NARMS/analytic_file_V6.csv"))

case.race<-dat[Race !='U', list(Cases=.N), by='Race']
case.ethn<-dat[Ethnicity !='U', list(Cases=.N), by='Ethnicity']

#-------------------------------------------------------------------------------
# # Population data for FoodNet area
#-------------------------------------------------------------------------------
fn<-read.csv('FoodNet Population Data/foodnet_county_population_2004_2019.csv', stringsAsFactors = F)
setDT(fn)

fn.race<-fn[Race!='All', list(FoodNetPop=sum(estimate)), by='Race']
fn.ethn<-fn[Ethnicity!='All', list(FoodNetPop=sum(estimate)), by='Ethnicity']

#-------------------------------------------------------------------------------
# 2006-2019 Population data for U.S. 
#-------------------------------------------------------------------------------
#Select years
years<-as.list(2010:2019)
names(years)<-2010:2019

#Select fields
popvars<-c(paste0('B02001_00', 2:8, 'E'), 'B03002_002E', 'B03002_012E')

us1019<-setDT(map_dfr(years,
                  ~get_acs(geography='us', variables=popvars, year=.x, survey = 'acs5'), 
                  .id='Year'))

us1019$Race<-factor(us1019$variable, levels=substr(popvars, 1, 10), labels=c('W', 'B', 'I', 'A', 'P', 'O', 'M', 'N', 'H'))

#Drop additional columns
us1019<-us1019[, c('GEOID', 'NAME', 'variable') := NULL]

#Rename "estimate" field
names(us1019)[names(us1019)=='estimate']<-'value'

#Replicate 2010 data for 2006-2009
us0609<-us1019[rep(which(Year==2010), 4), ]
us0609$Year<-rep(2006:2009, each=nrow(us1019[which(Year==2010),]))

#-------------------------------------------------------------------------------
# 2000 Population data for U.S. 
#-------------------------------------------------------------------------------
#Select fields
popvars<-c(paste0('P00700', 2:8), 'P008002', 'P008010')

us04<-setDT(get_decennial(geography='us', variables=popvars, year=2000, survey = 'sf1'))

us04$Race<-factor(us04$variable, levels=popvars, labels=c('W', 'B', 'I', 'A', 'P', 'O', 'M', 'N', 'H'))

#Drop additional columns
us04<-us04[, c('GEOID', 'NAME', 'variable') := NULL]

#Replicate 2000(2004) data for 2005
us05<-us04
us04$Year<-2004
us05$Year<-2005

#-------------------------------------------------------------------------------
# Combine 2004-2019 Population data for U.S. 
#-------------------------------------------------------------------------------
us<-rbind(us04, us05, subset(us0609, select= -moe), subset(us1019, select= -moe))
us<-us[, list(Total=sum(value)), by='Race']

#-------------------------------------------------------------------------------
# Combine case, FoodNet population and U.S. population counts and calculate
# chi-square and percentages by race (exclude unknown)
#-------------------------------------------------------------------------------

race<-merge(merge(case.race, fn.race, by='Race', all=T), us, by='Race', all.x=T)

#Overall chi-square
chisq.test(race[, .(Cases, FoodNetPop, Total)])

#Chi-square for each racial group compared to population not in race category
temp<-data.frame(Race=character(), pvalue=double())
for (i in race$Race){
 temp<-rbind(temp,
             cbind(i,chisq.test(race[, lapply(.SD, sum), by=list(Race==i)][, .(Cases, FoodNetPop, Total)])$p.value))
}
names(temp)<-c('Race', 'pvalue')
race<-merge(race, temp, by='Race')

#-------------------------------------------------------------------------------
# Add column percents for race table
#-------------------------------------------------------------------------------
cols<-c('Cases', 'FoodNetPop', 'Total')
race[, paste0(cols,".Perc") := lapply(.SD, function(x) round(x/sum(x)*100, 1)), .SDcols=cols]

#-------------------------------------------------------------------------------
# Combine case, FoodNet population and U.S. population counts and calculate
# chi-square and percentages by ETHNICITY (exclude unknown)
#-------------------------------------------------------------------------------
ethn<-merge(merge(case.ethn, fn.ethn, by='Ethnicity', all=T), us, by.x='Ethnicity', by.y='Race', all.x=T)

#Overall chi-square
ethn$pvalue<-double()
ethn$pvalue<-rep(chisq.test(ethn[, .(Cases, FoodNetPop, Total)])$p.value, 2)

#-------------------------------------------------------------------------------
# Add column percents for ethnicity table
#-------------------------------------------------------------------------------
ethn[, paste0(cols,".Perc") := lapply(.SD, function(x) round(x/sum(x)*100, 1)), .SDcols=cols]

#-------------------------------------------------------------------------------
# Format tables for output
#-------------------------------------------------------------------------------
race$Race<-factor(race$Race, levels = c('I', 'A', 'B', 'P', 'W', 'M', 'O'))
race<-race[order(Race),]
race[, paste0(cols,".Formatted") := 
       list(paste0(Cases, " (", Cases.Perc, ")"),
            paste0(FoodNetPop, " (", FoodNetPop.Perc, ")"),
            paste0(Total, " (", Total.Perc, ")"))]

race<-race[, c(1, 9:11)]

ethn[, paste0(cols,".Formatted") := 
       list(paste0(Cases, " (", Cases.Perc, ")"),
            paste0(FoodNetPop, " (", FoodNetPop.Perc, ")"),
            paste0(Total, " (", Total.Perc, ")"))]
ethn<-ethn[, c(1, 9:11)]

#define sheet names for each data frame
dataset_names <- list('Race' = race, 'Ethnicity' = ethn)

setwd('//cdc.gov/project/ATS_GIS_Store12/Shigella_SVI/Output Datasets For Follow-Up/')
write.xlsx(dataset_names, file = paste0('Addl_Race_Cases_prop_', Sys.Date(), '.xlsx') )


