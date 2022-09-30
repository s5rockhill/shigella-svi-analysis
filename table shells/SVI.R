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

#Combine population data for all race/all ethnicity estimates
fnpop00$moe<-NA
pop<-rbind(fnpop00[Race=='All' & Ethnicity=='All',], fnpop10[Race=='All' & Ethnicity=='All',])
rm(fnpop00, fnpop10)

#Sum by year, geoid, standard age group
pop<-pop[, list(pop=sum(estimate), moe=tidycensus::moe_sum(moe, estimate=estimate, na.rm=T)),
       by=c('Year', 'State', 'GEOID', 'AgeStd')]

#-------------------------------------------------------------------------------
# Import SVI datasets and calculate SVI quartiles
#-------------------------------------------------------------------------------
ch0 <- odbcDriverConnect("DSN=GRASP_MSSQL;UID=ppk8;DATABASE=sde_grasp_svi", rows_at_time = 1)
ch1 <- odbcDriverConnect("DSN=GRASP_MSSQL;UID=ppk8;DATABASE=sde_grasp_svi_2010", rows_at_time = 1)
ch2 <- odbcDriverConnect("DSN=GRASP_MSSQL;UID=ppk8;DATABASE=sde_grasp_svi_2014", rows_at_time = 1)
ch3 <- odbcDriverConnect("DSN=GRASP_MSSQL;UID=ppk8;DATABASE=sde_grasp_svi_2016", rows_at_time = 1)
ch4 <- odbcDriverConnect("DSN=GRASP_MSSQL;UID=ppk8;DATABASE=sde_grasp_svi_2018", rows_at_time = 1)

svi00 <- sqlQuery(ch0, "SELECT * FROM sdeadmin.US_NATIONAL_2000_SVI", as.is=TRUE)
svi10 <- sqlQuery(ch1, "SELECT * FROM sdeadmin.SVI2010_US", as.is=TRUE)
svi14 <- sqlQuery(ch2, "SELECT * FROM sdeadmin.SVICompiled_Tract_National", as.is=TRUE)
svi16 <- sqlQuery(ch3, "SELECT * FROM sdeadmin.SVI2016_US_tract", as.is=TRUE)
svi18 <- sqlQuery(ch4, "SELECT * FROM sde.SVI2018_US_tract", as.is=TRUE)

on.exit(RODBC::odbcClose(ch0, ch1, ch2, ch3, ch4))

#Select variables for analysis
svivars<-read.xlsx('SVI variables.xlsx')
svi00<-svi00[, svivars$`2000`]
svi10<-svi10[, na.omit(svivars$`2010`)]
svi14<-svi14[, na.omit(svivars$`2014`)]
svi16<-svi16[, svivars$`2016`]
svi18<-svi18[, svivars$`2018`]

#Update variable names for 2000 and 2010 to match later datasets
for (i in 1:length(colnames(svi00))){ 
  colnames(svi00)[i] <-
    svivars$`2014`[which(svivars$`2000`==colnames(svi00)[i])]}

for (i in 1:length(colnames(svi10))){ 
  colnames(svi10)[i] <-
    svivars$`2014`[which(svivars$`2010`==colnames(svi10)[i])]}

#Add state count to tract part for 2000 & 2010 tract fips
svi00$FIPS<-paste0(svi00$STCNTY, svi00$FIPS)
svi10$FIPS<-paste0(svi10$STCNTY, svi10$FIPS)

#Record year
svi00$sviyear<-2000
svi10$sviyear<-2010
svi14$sviyear<-2014
svi16$sviyear<-2016
svi18$sviyear<-2018

#Combine all datasets
svi_all<-setDT(plyr::rbind.fill(svi00, svi10, svi14, svi16, svi18))
rm(svi00, svi10, svi14, svi16, svi18)

#Calculate quartiles
svi_all[svi_all==-999]<-NA
pctileCols <- names(svi_all)[1:20]
svi_all<-svi_all[, (pctileCols):=lapply(.SD, as.numeric), .SDcols=pctileCols]
svi_all[,E_TOTPOP := as.numeric(E_TOTPOP)]

quart_func <- function(x) {  
   cut(x, breaks=c(0, .25, .5, .75, 1), include.lowest=T, labels=F)
}

svi_all[, (pctileCols) := lapply(.SD,quart_func), .SDcols=pctileCols,  by='sviyear']

#-------------------------------------------------------------------------------
# Sum population by quartile (and Year?) and calculate percents
#-------------------------------------------------------------------------------
us_svi_pop<-data.frame(Group=character(), Quartile=integer(), US_POP=integer())

for (i in pctileCols){
  x<-svi_all[, list(Group=i, US_POP=sum(E_TOTPOP)) , by=c(i)]
  setnames(x, i, 'Quartile')
  us_svi_pop<-rbind(us_svi_pop, x)
}

setDT(us_svi_pop)[, 'US_PCNT' := round(US_POP/sum(US_POP)*100, 1), by=c('Group')]

#-------------------------------------------------------------------------------
# Left join to SVI score to population based on Year/sviyear
#-------------------------------------------------------------------------------
names(svi_all)[names(svi_all)=='FIPS']<-'GEOID'

#Classify each year of population by the year we want to join svi data to
pop[,'sviyear':= as.numeric(as.character(
  cut(Year, breaks=c(2000, 2006, 2011, 2015, 2017, 2020), right=F,
      labels=c('2000', '2010', '2014', '2016', '2018'))))]

pop<-merge(pop, svi_all, by=c('GEOID', 'sviyear'), all.x=T)

#-------------------------------------------------------------------------------
# Sum FoodNet Population by SVI quartile
#-------------------------------------------------------------------------------
fn_svi_pop<-data.frame(Group=character(), Quartile=integer(), FN_POP=integer())

for (i in pctileCols){
  x<-pop[, list(Group=i, FN_POP=sum(pop)) , by=c(i)]
  setnames(x, i, 'Quartile')
  fn_svi_pop<-rbind(fn_svi_pop, x)
}

setDT(fn_svi_pop)[, 'FN_PCNT' := round(FN_POP/sum(FN_POP)*100, 1), by=c('Group')]

#-------------------------------------------------------------------------------
#Import shigella case data
#-------------------------------------------------------------------------------
folder<-'//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/FoodNet_NARMS/'
dat<-setDT(read.csv(paste0(folder, "analytic_file_V7.csv"), stringsAsFactors = F,
                    colClasses = c('CTNO2000'='character', 'CTNO2010'='character')))

dat$AgeStd<-cut(dat$AgeClean, breaks=c(0, 5, seq(15, 85, by=10), 999), right=F, labels=StdAgeCat)

#Update missing census tract information using deterministic imputation result
dat[is.na(CTNO2000), CTNO2000 := Deterministic]
dat[is.na(CTNO2010), CTNO2010 := Deterministic]

dat$CTNO2000<-str_pad(dat$CTNO2000, width=11, side='left', pad='0')

#-------------------------------------------------------------------------------
# Incidence Rates: Sum cases by standard age category, census tract, and year & 
# bind to pop data to calculate age-adjusted rate by SVI quartile
#-------------------------------------------------------------------------------
cases<-rbind(
  dat[Year<2006, list(Cases=.N), by=list(Year, GEOID=CTNO2000, AgeStd)],
  dat[Year>2005, list(Cases=.N), by=list(Year, GEOID=CTNO2010, AgeStd)])

rates<-merge(pop, cases, by=c('Year', 'GEOID', 'AgeStd'), all.x = T)
rates$Cases[is.na(rates$Cases)]<-0

#Load direct age adjustment helper function
fileNameFunc <- "./helper funcs/ageadjust rate direct method with variance.R."
source(fileNameFunc)

#Create empty data frame for storing results
ir<-data.frame(Quartile=integer(), Cases=integer(), adj.rate=numeric(), 
               lci=numeric(), uci=numeric(), variance=numeric())

#Loop through each SVI theme/var and calculate age-adjusted rates
for (i in pctileCols){
  x<-rates[, list(Group=i, Cases=sum(Cases), pop=sum(pop)), by=c('AgeStd', eval(i))]
  setnames(x, i, "Quartile")
  ir<-rbind(ir, 
            x[!is.na(Quartile), as.list(
                    age.adjust.direct.var(Cases, pop, std.pop$Population)[2:5]
                    *100000), 
              by=c('Group', 'Quartile')]
  )
}

#Set lci NaN values to zero
ir[,lci:=ifelse(is.nan(lci), 0, lci)]

#Calcualte rate ratio between 4th and 1st quartiles
ir<-ir[order(Group, Quartile),]
ir[, RateRatio:=round(last(adj.rate)/first(adj.rate), 3), by='Group']

#Calculate rate ratio confidence limits
ir[,RRvar := ((first(variance)/first(adj.rate)^2)+(last(variance)/last(adj.rate)^2))*100000,
   by='Group']

ir[,'RRci':= paste0("(", sprintf("%0.3f",round(exp(log(RateRatio)-1.96*sqrt(RRvar)), 3)), 
                    " - ",
                    sprintf("%0.3f",round(exp(log(RateRatio)+1.96*sqrt(RRvar)), 3)), ")")]

#Round adjusted rates and format confidence limits into single column
ir[,c('adj.rate', 'lci', 'uci') := round(.SD,3), .SDcols=c('adj.rate', 'lci', 'uci')]
ir[,ci:=paste0("(", sprintf("%0.3f", lci), " - ", sprintf("%0.3f", uci), ")")]

#Keep necessary columns
ir<-ir[, c('Group', 'Quartile', 'adj.rate', 'ci', 'RateRatio', 'RRci')]

#############################
for (i in pctileCols){
  x<-rates[, list(Group=i, Cases=sum(Cases), pop=sum(pop)), by=c('AgeStd', eval(i))]
  setnames(x, i, "Quartile")
  ir<-rbind(ir, 
    x[, 
      as.list(
        unlist(
          round(
            ageadjust.direct(Cases, pop, rate=NULL, std.pop$Population)[2:5]
              *100000, digits = 3))), 
      by=c('Group', 'Quartile')
      ]
  )
}

#-------------------------------------------------------------------------------
# Frequencies and Percents: Sum cases by census tract and bind to svi data 
# Doing this separately will keep cases with unknown age
#-------------------------------------------------------------------------------
cases[,'sviyear':= as.numeric(as.character(
          cut(Year, breaks=c(2000, 2006, 2011, 2015, 2017, 2020), right=F,
          labels=c('2000', '2010', '2014', '2016', '2018'))))]

cases<-cases[, list(Cases=sum(Cases)), by=c('sviyear', 'GEOID')]

cases<-merge(cases, svi_all, by=c('GEOID', 'sviyear'), all.x = T)

pcnt<-data.frame(Group=character(), Quartile=integer(), Cases=integer(), Percent=numeric())

for (i in pctileCols){
  x<-cases[, list(Group=i, Cases=sum(Cases)), by=c(eval(i))]
  x[, 'Percent':= round(Cases/sum(Cases)*100, 1)]
  setnames(x, i, "Quartile")
  pcnt<-rbind(pcnt, x) 
}

#-------------------------------------------------------------------------------
# Merge results and format for output
#-------------------------------------------------------------------------------
result<-merge(pcnt, ir, by=c('Group', 'Quartile'))
result<-merge(result, us_svi_pop, by=c('Group', 'Quartile'))
result<-merge(result, fn_svi_pop, by=c('Group', 'Quartile'))

result[, c('Rate.formatted', 'RateRatio.formatted') := list(paste(adj.rate, ci), paste(RateRatio, RRci))]

result[1:3, RateRatio.formatted := NULL, by='Group']

result$Group<-factor(result$Group, 
                     levels=c("RPL_THEMES", 
                              "RPL_THEME1",
                                "EPL_POV", "EPL_UNEMP", "EPL_PCI", "EPL_NOHSDP",
                              "RPL_THEME2",
                                "EPL_AGE65", "EPL_AGE17", "EPL_DISABL", "EPL_SNGPNT",
                              "RPL_THEME3", 
                                "EPL_MINRTY", "EPL_LIMENG",
                              "RPL_THEME4",
                                "EPL_MUNIT", "EPL_MOBILE", "EPL_CROWD", "EPL_NOVEH","EPL_GROUPQ"))

result<-result[order(Group, Quartile),]

result<-result[!is.na(Quartile), c('Group', 'Quartile', 'Cases', 'Percent',
                                  'FN_POP', 'FN_PCNT', 'US_POP', 'US_PCNT',
                                  'Rate.formatted', 'RateRatio.formatted')]

#Output results
setwd('//cdc.gov/project/ATS_GIS_Store12/Shigella_SVI/Preliminary Results')
write.xlsx(result, paste0('SVI_ ', Sys.Date(), '.xlsx'))
