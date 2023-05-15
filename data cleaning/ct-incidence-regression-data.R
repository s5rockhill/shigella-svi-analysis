library('data.table')
library('stringr')
library('openxlsx')
library('RODBC')
library('tidycensus')

#-------------------------------------------------------------------------------
#Import shigella case data
#-------------------------------------------------------------------------------
folder<-'//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/FoodNet_NARMS/'
dat<-setDT(read.csv(paste0(folder, "analytic_file_V7.csv"), stringsAsFactors = F,
                    colClasses = c('CTNO2000'='character', 'CTNO2010'='character')))

#Classify age
AgeGroupLabs<-c('0-4', '5-14', '15-44', '45+')
dat$AgeGroup<-cut(dat$AgeClean, breaks=c(0, 5, 15, 45, 999), right=F, labels=AgeGroupLabs)

#Update missing census tract information using deterministic imputation result
dat[is.na(CTNO2000), CTNO2000 := Deterministic]
dat[is.na(CTNO2010), CTNO2010 := Deterministic]

dat$CTNO2000<-str_pad(dat$CTNO2000, width=11, side='left', pad='0')

#Classify cases by which year of svi we want to join to 
svibreaks<-c(2000, 2006, 2011, 2015, 2017, 2020)
svilabels<-c('2000', '2010', '2014', '2016', '2018')
dat[,'sviyear' := cut(Year, breaks=svibreaks, right=F, labels=svilabels)]

#-------------------------------------------------------------------------------
# Sum cases by age group and sex for each race/ethnicity category 
#-------------------------------------------------------------------------------
CaseCounts<-rbind(
  dat[Year<2006, list(Race='All', Ethnicity='All', Cases=.N), by=list(sviyear, GEOID=CTNO2000, AgeGroup, Sex)],
  dat[Year>2005, list(Race='All', Ethnicity='All', Cases=.N), by=list(sviyear, GEOID=CTNO2010, AgeGroup, Sex)],
  dat[Year<2006, list(Ethnicity='All', Cases=.N), by=list(sviyear, GEOID=CTNO2000, AgeGroup, Sex, Race)],
  dat[Year>2005, list(Ethnicity='All', Cases=.N), by=list(sviyear, GEOID=CTNO2010, AgeGroup, Sex, Race)],
  dat[Year<2006, list(Race='All', Cases=.N), by=list(sviyear, GEOID=CTNO2000, AgeGroup, Sex, Ethnicity)],
  dat[Year>2005, list(Race='All', Cases=.N), by=list(sviyear, GEOID=CTNO2010, AgeGroup, Sex, Ethnicity)])

#Drop unknown race and unknown ethnicity records
CaseCounts<-subset(CaseCounts, Race != 'U' & Ethnicity != 'U')

#-------------------------------------------------------------------------------
# Import tract-level population data for foodnet surveillance area
#-------------------------------------------------------------------------------
folder<-'//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/FoodNet Population Data/'

#2004-2005 population by 2000 census tracts
fnpop00<-setDT(read.csv(paste0(folder, 'foodnet_2000tract_population_2004_2005.csv'), 
                        stringsAsFactors = F, colClasses = c("GEOID"='character')))

#2006-2019 population by 2010 census tract
fnpop10<-setDT(read.csv(paste0(folder, 'foodnet_2010tract_population_2006_2019.csv'), 
                        stringsAsFactors = F, colClasses = c("GEOID"='character')))

setkey(fnpop00, Year, GEOID)
setkey(fnpop10, Year, GEOID)

#Combine population data
fnpop00$moe<-NA
pop<-rbind(fnpop00, fnpop10)
rm(fnpop00, fnpop10)

#Create age group variable
pop[, AgeGroup := factor(ifelse(AgeStd %in% c('15-24', '25-34', '35-44'), '15-44', 
                    ifelse(AgeStd %in% c('45-54', '55-64', '65-74', '75-84', '85+'), '45+', AgeStd)),
                    levels=AgeGroupLabs, labels = AgeGroupLabs)]

#Classify svi year 
pop[,'sviyear' := cut(Year, breaks=svibreaks, right=F, labels=svilabels)]

#and sum over age group and svi year
pop<-pop[,list(Pop=sum(estimate), moe=moe_sum(moe, estimate=estimate)), 
           by=c('GEOID', 'sviyear', 'Sex', 'Race', 'Ethnicity', 'AgeGroup')]

#Drop all race / all ethnicity category
#pop<-subset(pop, Race !='All' | Ethnicity!='All')

#-------------------------------------------------------------------------------
# Merge population and case counts. Executing a left join to keep tracts with 0 cases. 
#-------------------------------------------------------------------------------
fin<-merge(pop, CaseCounts, by = c('sviyear', 'GEOID', 'AgeGroup', 'Sex', 'Race', 'Ethnicity'), all.x=T)
fin$Cases[is.na(fin$Cases)]<-0

#Classify state
fin[,'State':=factor(str_sub(GEOID, 1, 2), 
                     levels=c('06','08','09','13','24','27','35','36','41','47'),
                     labels=c('CA', 'CO', 'CT', 'GA', 'MD', 'MN', 'NM', 'NY', 'OR', 'TN'))]

#-------------------------------------------------------------------------------
# Calculate age-adjusted incidence COUNTS, excluding records with zero population
# and more cases than population
#-------------------------------------------------------------------------------
#fin<-merge(fin, std.pop[, c('AgeStd', 'Proportion')], by='AgeStd', all.x=T)

#fin[Pop>0 & Cases<=Pop, AgeSpecific := round((Cases/Pop*100000)*Proportion, 0)]

#fin<-
#  fin[, list(Cases=sum(Cases), Pop=sum(Pop), AdjInc=sum(AgeSpecific, na.rm=T), moe=moe_sum(moe, Pop)), 
#        by=list(sviyear, GEOID, Sex, Race, Ethnicity)]

#-------------------------------------------------------------------------------
# Import SVI datasets and calculate total SVI quartile
#-------------------------------------------------------------------------------
ch0 <- odbcDriverConnect("DSN=GRASP_MSSQL;UID=ppk8;DATABASE=sde_grasp_svi", rows_at_time = 1)
ch1 <- odbcDriverConnect("DSN=GRASP_MSSQL;UID=ppk8;DATABASE=sde_grasp_svi_2010", rows_at_time = 1)
ch2 <- odbcDriverConnect("DSN=GRASP_MSSQL;UID=ppk8;DATABASE=sde_grasp_svi_2014", rows_at_time = 1)
ch3 <- odbcDriverConnect("DSN=GRASP_MSSQL;UID=ppk8;DATABASE=sde_grasp_svi_2016", rows_at_time = 1)
ch4 <- odbcDriverConnect("DSN=GRASP_MSSQL;UID=ppk8;DATABASE=sde_grasp_svi_2018", rows_at_time = 1)

svi00 <- sqlQuery(ch0, "SELECT STCOFIPS, TRACT, USTP FROM sdeadmin.US_NATIONAL_2000_SVI", as.is=TRUE)
svi10 <- sqlQuery(ch1, "SELECT STCOFIPS, TRACT, R_PL_THEMES FROM sdeadmin.SVI2010_US", as.is=TRUE)
svi14 <- sqlQuery(ch2, "SELECT STCNTY, FIPS, RPL_THEMES FROM sdeadmin.SVICompiled_Tract_National", as.is=TRUE)
svi16 <- sqlQuery(ch3, "SELECT STCNTY, FIPS, RPL_THEMES FROM sdeadmin.SVI2016_US_tract", as.is=TRUE)
svi18 <- sqlQuery(ch4, "SELECT STCNTY, FIPS, RPL_THEMES FROM sde.SVI2018_US_tract", as.is=TRUE)

on.exit(RODBC::odbcClose(ch0, ch1, ch2, ch3, ch4))

#Combine 2000-2018 svi datasets
names(svi00)<-names(svi14)
names(svi10)<-names(svi14)
svi00$FIPS<-paste0(svi00$STCNTY, svi00$FIPS)
svi10$FIPS<-paste0(svi10$STCNTY, svi10$FIPS)
svi00$sviyear<-2000
svi10$sviyear<-2010
svi14$sviyear<-2014
svi16$sviyear<-2016
svi18$sviyear<-2018
svi_all<-setDT(rbind(svi00, svi10, svi14, svi16, svi18))
rm(svi00, svi10, svi14, svi16, svi18)

#Calculate quartile
svi_all$RPL_THEMES<-as.numeric(svi_all$RPL_THEMES)
svi_all[svi_all==-999]<-NA
svi_all[, quartile := cut(RPL_THEMES, breaks=c(0, .25, .5, .75, 1), include.lowest=T, labels=F), by='sviyear']

#-------------------------------------------------------------------------------
# Left join to SVI score based on Year/sviyear
#-------------------------------------------------------------------------------
names(svi_all)[names(svi_all)=='FIPS']<-'GEOID'
fin[, sviyear := as.numeric(as.character(sviyear))]
fin<-merge(fin, svi_all, by=c('GEOID', 'sviyear'), all.x=T)

#-------------------------------------------------------------------------------
#Import NCHS Urban-Rural classification and merge to data by STCNTY
#-------------------------------------------------------------------------------
folder<-'//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/NCHS/'
nchs<-read.xlsx(paste0(folder, 'NCHSURCodes2013.xlsx'))
nchs$STCNTY<-str_pad(nchs$FIPS.code, 5, "left", "0")
nchs$UrCode<-nchs$`2013.code`

fin[, STCNTY := ifelse(is.na(STCNTY), str_sub(GEOID, 1, 5), STCNTY)]
fin<-merge(fin, nchs[,10:11], by='STCNTY', all.x=T)
names(fin)[names(fin)=='STCNTY']<-'County'

########################## END DATA PREPARATION  #####################################

#-------------------------------------------------------------------------------
#Output final datasets
#-------------------------------------------------------------------------------
write.csv(fin[Race !='All', !"Ethnicity"], 
          paste0("//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/Final Datasets/",
          "Final_Aggregate_ByRace_", Sys.Date(), '.csv'), row.names = F)

write.csv(fin[Ethnicity !='All', !"Race"], 
          paste0("//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/Final Datasets/",
          "Final_Aggregate_ByEthnicity_", Sys.Date(), '.csv'), row.names = F)

