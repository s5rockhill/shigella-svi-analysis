library('data.table')
library('stringr')
library('purrr')
library('epitools')
library('openxlsx')
library('RODBC')

#-------------------------------------------------------------------------------
#U.S. 2000 population for age standardization
#-------------------------------------------------------------------------------
StdAgeCat<-c('0-4', '5-14', '15-24', '25-34', '35-44', '45-54', '55-64', '65-74','75-84', '85+')
std.pop<-data.frame(AgeStd= as.factor(StdAgeCat), 
                    Population=c(18986520, 39976619, 38076743, 37233437, 44659185, 
                                 37030152, 23961506, 18135514,  12314793, 4259173))

#-------------------------------------------------------------------------------
# Import tract-level population data for foodnet surveillance area
#-------------------------------------------------------------------------------
folder<-'//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/FoodNet Population Data/'

#2004-2005 population by 2000 census tracts
fnpop00<-setDT(read.csv(paste0(folder, 'foodnet_2000tract_population_2004_2005.csv'), 
                  stringsAsFactors = F, colClasses = c("GEOID"='character')))
setkey(fnpop00, Year, GEOID)

#2006-2019 population by 2010 census tract
fnpop10<-setDT(read.csv(paste0(folder, 'foodnet_2010tract_population_2006_2019.csv'), 
               stringsAsFactors = F, colClasses = c("GEOID"='character')))
setkey(fnpop10, Year, GEOID)

#Combine population data
fnpop00$moe<-NA
pop<-rbind(fnpop00, fnpop10)
rm(fnpop00, fnpop10)

#Classify state
pop[,'State':=factor(str_sub(GEOID, 1, 2), 
       levels=c('06','08','09','13','24','27','35','36','41','47'),
       labels=c('CA', 'CO', 'CT', 'GA', 'MD', 'MN', 'NM', 'NY', 'OR', 'TN'))]

#Classify age group
pop[,'AgeGroup':= factor(ifelse(AgeStd %in% c('25-34','35-44'), '25-44', 
                         ifelse(AgeStd %in% c('45-54','55-64'), '45-64', 
                         ifelse(AgeStd %in% c('65-74','75-84', '85+'), '65+', 
                         as.character(AgeStd)))), 
                         levels=c('0-4', '5-14', '15-24', '25-44', '45-64', '65+'))]

#Factor AgeStd so that categories are in correct order
pop[, 'AgeStd':=factor(AgeStd, levels=StdAgeCat)]

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
svi_all[svi_all==-999]<-NA
svi_all$RPL_THEMES<-as.numeric(svi_all$RPL_THEMES)
svi_all[, quartile := cut(RPL_THEMES, breaks=c(0, .25, .5, .75, 1), include.lowest=T, labels=F), by='sviyear']
svi_all[, c('STCNTY', 'RPL_THEMES') := NULL]

#-------------------------------------------------------------------------------
# Left join to SVI score based on Year/sviyear
#-------------------------------------------------------------------------------
names(svi_all)[names(svi_all)=='FIPS']<-'GEOID'

#Classify each year of fnpop by the year we want to join svi data to
pop[,'sviyear':= as.numeric(as.character(
                            cut(Year, breaks=c(2000, 2006, 2011, 2015, 2017, 2020), right=F,
                            labels=c('2000', '2010', '2014', '2016', '2018'))))]

pop<-merge(pop, svi_all, by=c('GEOID', 'sviyear'), all.x=T)

#-------------------------------------------------------------------------------
#Import NCHS Urban-Rural classification and merge to population data
#-------------------------------------------------------------------------------
folder<-'//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/NCHS/'
nchs<-read.xlsx(paste0(folder, 'NCHSURCodes2013.xlsx'))
nchs$STCNTY<-str_pad(nchs$FIPS.code, 5, "left", "0")
nchs$UrCode<-nchs$`2013.code`

pop$STCNTY<-str_sub(pop$GEOID, 1, 5)
pop<-merge(pop, nchs[,10:11], by='STCNTY', all.x=T)

#-------------------------------------------------------------------------------
#Import shigella case data
#-------------------------------------------------------------------------------
folder<-'//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/FoodNet_NARMS/'
dat<-setDT(read.csv(paste0(folder, "analytic_file_V7.csv"), stringsAsFactors = F,
                    colClasses = c('CTNO2000'='character', 'CTNO2010'='character')))

dat$CTNO2000<-str_pad(dat$CTNO2000, width=11, side='left', pad='0')

#Classify cases by number of resistance categories
dat$Abx1More<-dat$AbxResSum>=1
dat$Abx2More<-dat$AbxResSum>=2
dat$Abx3More<-dat$AbxResSum>=3
dat$Abx4More<-dat$AbxResSum>=4

#Classify by Age Group
dat$AgeGroup<-cut(dat$AgeClean, breaks=c(0, 5, 15, 25, 45, 65, 999), right=F, 
                  labels=c('0-4', '5-14', '15-24', '25-44', '45-64', '65+'))

dat$AgeStd<-cut(dat$AgeClean, breaks=c(0, 5, seq(15, 85, by=10), 999), right=F, 
                labels=StdAgeCat)

#Update Fever and Bloody diarreha values
dat[, c('Fever', 'BloodyDiarr'):= list(ifelse(Fever=='YES', T, F), ifelse(BloodyDiarr=='YES', T, F))]

#Update missing census tract information using deterministic imputation result
dat[is.na(CTNO2000), CTNO2000 := Deterministic]
dat[is.na(CTNO2010), CTNO2010 := Deterministic]

#-------------------------------------------------------------------------------
#Sum cases by demographic categories for rate calculation
#-------------------------------------------------------------------------------
#Sum cases for selected categories
varlist<-c('Sex', "Ethnicity", "Race", "SpeciesClean", "AgeGroup", "State",
           'Fever', 'BloodyDiarr', 'HospClean', 'Bacteremia', 'Death',
           "AmpR", "CipDSC", "CotR", "CipR", 'AxoR', 'AzmR', 
           'Abx1More', 'Abx2More', 'Abx3More', 'Abx4More', 'MDR', 'XDR')

Result<-rbind(
  dat[Year<2006, list(Group='Total', Values='All',Cases=.N), by=list(Year, GEOID=CTNO2000, AgeStd)],
  dat[Year>2005, list(Group='Total', Values='All',Cases=.N), by=list(Year, GEOID=CTNO2010, AgeStd)])

for (i in (varlist)){
  x<-dat[Year<2006, list(Group=i, Cases=.N) , by=c('Year', 'CTNO2000', 'AgeStd', i)]
  y<-dat[Year>2005, list(Group=i, Cases=.N) , by=c('Year', 'CTNO2010', 'AgeStd', i)]
  setnames(x, i, "Values")
  setnames(x, 'CTNO2000', "GEOID")
  setnames(y, i, "Values")
  setnames(y, 'CTNO2010', "GEOID")
  Result<-rbind(Result, x, y)
}

#Drop No/unknown/ FALSE and NA for abx resistance and severity categories
Result<-subset(Result, !(Group %in% varlist[7:23] & Values %in% c(NA, FALSE, 'UNKNOWN', 'NO')))

#-------------------------------------------------------------------------------
#Sum population denominators
#-------------------------------------------------------------------------------
bycols<-c('GEOID', 'Year', 'AgeStd', 'quartile')
p0<-pop[Race=='All' & Ethnicity=='All', list(Pop=sum(estimate)), by=c(bycols, 'State', 'UrCode', 'AgeGroup')]
p1<-pop[Race=='All' & Ethnicity=='All', list(Pop=sum(estimate)), by=c(bycols, 'Sex')]
p2<-pop[Race!='All' & Ethnicity=='All', list(Pop=sum(estimate)), by=c(bycols, 'Race')]
p3<-pop[Race=='All' & Ethnicity!='All', list(Pop=sum(estimate)), by=c(bycols, 'Ethnicity')]

#-------------------------------------------------------------------------------
#Merge aggregated cases to population counts and sum by demographic group
#I'm running out of RAM so I'm going to do each group sequentially
#-------------------------------------------------------------------------------
Ttl<-merge(p0, Result[Group=='Total',], by=c('GEOID', 'Year', 'AgeStd'), all.x=T)
Ttl$Cases[is.na(Ttl$Cases)]<-0
Age<-Ttl[, list(Group='AgeGroup', Cases=sum(Cases), Pop=sum(Pop)), by=list(quartile, AgeStd, AgeGroup)]
Ste<-Ttl[, list(Group='State', Cases=sum(Cases), Pop=sum(Pop)), by=list(quartile, AgeStd, State)]
Urc<-Ttl[, list(Group='UrCode', Cases=sum(Cases), Pop=sum(Pop)), by=list(quartile, AgeStd, UrCode)]
Ttl<-Ttl[, list(Group='Total', Values='All', Cases=sum(Cases), Pop=sum(Pop)), by=list(quartile, AgeStd)]

Sex<-merge(p1[,Values:=Sex], Result[Group=='Sex',], by=c('GEOID', 'Year', 'AgeStd', 'Values'), all.x=T)
Sex$Cases[is.na(Sex$Cases)]<-0
Sex<-Sex[, list(Group='Sex', Cases=sum(Cases), Pop=sum(Pop)), by=list(quartile, AgeStd, Sex)]

Rac<-merge(p2[,Values:=Race], Result[Group=='Race',], by=c('GEOID', 'Year', 'AgeStd', 'Values'), all.x=T)
Rac$Cases[is.na(Rac$Cases)]<-0
Rac<-Rac[, list(Group='Race',Cases=sum(Cases), Pop=sum(Pop)), by=list(quartile, AgeStd, Race)]

Eth<-merge(p3[,Values:=Ethnicity], Result[Group=='Ethnicity',], by=c('GEOID', 'Year', 'AgeStd', 'Values'), all.x=T)
Eth$Cases[is.na(Eth$Cases)]<-0
Eth<-Eth[, list(Group='Ethnicity',Cases=sum(Cases), Pop=sum(Pop)), by=list(quartile, AgeStd, Ethnicity)]

#Species 
for (i in (unique(dat$SpeciesClean))){
  x <- merge(p0, Result[Group=='SpeciesClean' & Values==i,], by=c('GEOID', 'Year', 'AgeStd'), all.x=T)
  x$Cases[is.na(x$Cases)]<-0
  x <- x[, list(Group='SpeciesClean', Values=i, Cases=sum(Cases), Pop=sum(Pop)), by=list(quartile, AgeStd)]
  Ttl<-rbind(Ttl, x)
  #rm(x)
}

#Abx resistance and severity categories
for (i in (varlist[c(7:16, 18:23)])){
  x <- merge(p0, Result[Group==i,], by=c('GEOID', 'Year', 'AgeStd'), all.x=T)
  x$Cases[is.na(x$Cases)]<-0
  x <- x[, list(Group=i, Values=TRUE, Cases=sum(Cases), Pop=sum(Pop)), by=list(quartile, AgeStd)]
  Ttl<-rbind(Ttl, x)
  rm(x)
}

#Calculate AzmR separately because it wasn't tested for until 20ll
Azm<-merge(p0[Year>2010,], Result[Group=='AzmR',], by=c('GEOID', 'Year', 'AgeStd'), all.x=T)
Azm$Cases[is.na(Azm$Cases)]<-0
Azm<-Azm[, list(Group='AzmR', Values=TRUE, Cases=sum(Cases), Pop=sum(Pop)), by=list(quartile, AgeStd)]

names(Age)[colnames(Age)=='AgeGroup']<-'Values'
names(Ste)[colnames(Ste)=='State']<-'Values'
names(Urc)[colnames(Urc)=='UrCode']<-'Values'
names(Sex)[colnames(Sex)=='Sex']<-'Values'
names(Rac)[colnames(Rac)=='Race']<-'Values'
names(Eth)[colnames(Eth)=='Ethnicity']<-'Values'

Ttl<-rbind(Ttl, Azm, Age, Ste, Urc, Sex, Rac, Eth)

Ttl[, 'AgeStd':=factor(AgeStd, levels=StdAgeCat)]
Ttl<-setorder(Ttl, Group, Values, quartile, AgeStd)

#-------------------------------------------------------------------------------
#Calculate Rates and 95% CI's (exact, gamma dist)
#Refer to: https://www.hsph.harvard.edu/thegeocodingproject/analytic-methods/
#-------------------------------------------------------------------------------
# Load Function for age-adjustment 
# I made a minor change to epiTools ageadjust.direct function to output variance

fileNameFunc <- "./helper funcs/ageadjust rate direct method with variance.R."
source(fileNameFunc)

ir<-Ttl[, as.list(age.adjust.direct.var(Cases, Pop, std.pop$Population)[2:5]*100000), 
          by=c('quartile', 'Group', 'Values')]

#Set lci NaN values to zero
ir[,lci:=ifelse(is.nan(lci), 0, lci)]

#Format confidence limits into single column
ir[,ci:=paste0("(", sprintf("%0.3f", round(lci, 3)), " - ", sprintf("%0.3f", round(uci,3)), ")")]

#Transpose 
ir<-dcast(ir, Group + Values ~ quartile, value.var=c('adj.rate', 'ci', 'variance'))

#Calcualte rate ratio
ir[, RateRatio:=round(adj.rate_4/adj.rate_1, 3)]

#Calculate rate ratio confidence limits
ir[, RRvar := ((variance_1/adj.rate_1^2)+(variance_4/adj.rate_4^2))*100000]
ir[,'RRci':= paste0("(", sprintf("%0.3f",round(exp(log(RateRatio)-1.96*sqrt(RRvar)), 3)), 
                          " - ",
                          sprintf("%0.3f",round(exp(log(RateRatio)+1.96*sqrt(RRvar)), 3)), ")")]

#Round adjusted rates
ratecols<-c('adj.rate_NA', 'adj.rate_1', 'adj.rate_2', 'adj.rate_3', 'adj.rate_4')
ir[, (ratecols):= round(.SD, 3), .SDcols=ratecols]

#Drop variance columns
ir[, c('variance_NA', 'variance_1', 'variance_2', 'variance_3', 'variance_4', 'RRvar'):=NULL]

#-------------------------------------------------------------------------------
#Merge cases to svi quartile and sum by Group and quartile and then merge to ir
#-------------------------------------------------------------------------------
Result<-merge(Result, unique(p0[,c('GEOID', 'Year', 'UrCode', 'quartile')]), by=c('Year', 'GEOID'), all.x=T)

ByUR<-Result[Group=='Total', list(Group='UrCode', Cases=sum(Cases)), by=list(UrCode, quartile)]
names(ByUR)[1]<-'Values'

Result<-rbind(Result[, list(Cases=sum(Cases)), by=list(Group, Values, quartile)], ByUR)

Result<-dcast(Result, Group + Values ~ quartile, value.var='Cases', fill = 0)      
names(Result)[3:7]<-c(paste0('Cases_', c(NA, 1:4)))

Result<-merge(Result, ir, by=c('Group', 'Values'), all.x=T)

#-------------------------------------------------------------------------------
#Order Columns and Rows for output (makes copying and pasting easier)
#-------------------------------------------------------------------------------
setcolorder(Result, c('Group', 'Values', 
                      paste0(rep(c("Cases_", "adj.rate_", "ci_"), 5), rep(c(NA, 1:4), each=3)),
                      'RateRatio', 'RRci'))

Result[,Group:=factor(Group, levels=c('Total', 'Sex', 'AgeGroup', 'Ethnicity', 'Race', 'SpeciesClean', 
                                      'UrCode', 'Fever', 'BloodyDiarr', 'HospClean', 'Bacteremia', 'Death', 'State', 
                                      'AmpR',  'CotR', 'CipDSC', "CipR", "AxoR", "AzmR", 'Abx1More', 'Abx2More',
                                      "Abx3More","Abx4More", "MDR", "XDR"))]

Result$Values<-as.character(Result$Values)
setorderv(Result, c('Group', 'Values'))

setwd('//cdc.gov/project/ATS_GIS_Store12/Shigella_SVI/Preliminary Results')
write.xlsx(Result, paste0('Demographics_3 ', Sys.Date(), '.xlsx'))
