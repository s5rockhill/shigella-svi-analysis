library('data.table')
library('stringr')
library('openxlsx')
library('RODBC')
library('ggplot2')

##########################   DATA PREPARATION  #####################################

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

#Factor AgeStd so that categories are in correct order
StdAgeCat<-c('0-4', '5-14', '15-24', '25-34', '35-44', '45-54', '55-64', '65-74','75-84', '85+')
pop[, 'AgeStd':=factor(AgeStd, levels=StdAgeCat)]

#-------------------------------------------------------------------------------
#Import shigella case data
#-------------------------------------------------------------------------------
folder<-'//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/FoodNet_NARMS/'
dat<-setDT(read.csv(paste0(folder, "analytic_file_V7.csv"), stringsAsFactors = F,
                    colClasses = c('CTNO2000'='character', 'CTNO2010'='character')))

dat$CTNO2000<-str_pad(dat$CTNO2000, width=11, side='left', pad='0')

dat$AgeStd<-cut(dat$AgeClean, breaks=c(0, 5, seq(15, 85, by=10), 999), right=F, labels=StdAgeCat)

#Update missing census tract information using deterministic imputation result
dat[is.na(CTNO2000), CTNO2000 := Deterministic]
dat[is.na(CTNO2010), CTNO2010 := Deterministic]

#-------------------------------------------------------------------------------
# Sum cases by standard age and sex for each race/ethnicity category 
#-------------------------------------------------------------------------------
CaseCounts<-rbind(
  dat[Year<2006, list(Race='All', Ethnicity='All', Cases=.N), by=list(Year, GEOID=CTNO2000, AgeStd, Sex)],
  dat[Year>2005, list(Race='All', Ethnicity='All', Cases=.N), by=list(Year, GEOID=CTNO2010, AgeStd, Sex)],
  dat[Year<2006, list(Ethnicity='All', Cases=.N), by=list(Year, GEOID=CTNO2000, AgeStd, Sex, Race)],
  dat[Year>2005, list(Ethnicity='All', Cases=.N), by=list(Year, GEOID=CTNO2010, AgeStd, Sex, Race)],
  dat[Year<2006, list(Race='All', Cases=.N), by=list(Year, GEOID=CTNO2000, AgeStd, Sex, Ethnicity)],
  dat[Year>2005, list(Race='All', Cases=.N), by=list(Year, GEOID=CTNO2010, AgeStd, Sex, Ethnicity)])

#-------------------------------------------------------------------------------
# Merge population and case counts to calculate expected cases for each tract
# Executing an outer join so we keep tracts with no cases and cases with unknown
# age, race, sex, or ethnicity
#-------------------------------------------------------------------------------
fin<-merge(pop, CaseCounts, by = c('Year', 'GEOID', 'AgeStd', 'Sex', 'Race', 'Ethnicity'), all=T)
fin$Cases[is.na(fin$Cases)]<-0

#Classify state
fin[,'State':=factor(str_sub(GEOID, 1, 2), 
                     levels=c('06','08','09','13','24','27','35','36','41','47'),
                     labels=c('CA', 'CO', 'CT', 'GA', 'MD', 'MN', 'NM', 'NY', 'OR', 'TN'))]

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

#-------------------------------------------------------------------------------
# Left join to SVI score based on Year/sviyear
#-------------------------------------------------------------------------------
names(svi_all)[names(svi_all)=='FIPS']<-'GEOID'

#Classify each year of fnpop by the year we want to join svi data to
fin[,c('sviyear') := as.numeric(as.character(cut(Year, breaks=c(2000, 2006, 2011, 2015, 2017, 2020), right=F,
                                                  labels=c('2000', '2010', '2014', '2016', '2018'))))]

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

########################## END DATA PREPARATION  #####################################

#-------------------------------------------------------------------------------
#Data quality checks
#-------------------------------------------------------------------------------
fin[Cases>estimate, .N, by=c('Race', 'Ethnicity')]
#There are 2638 records where there are one or more cases in a specific sex/race/ethnicity/age category
#but the population estimate for that category is 0. 97% are a difference of one case.

#-------------------------------------------------------------------------------
# Visualize distribution of number of cases for census tract for all race groups
#-------------------------------------------------------------------------------
#This plot shows distribution of tracts by log(cases) for all years combined by race
#Unknown race is excluded
vis1<-fin[Ethnicity=='All' & !(Race %in% c('U', 'All')), list(Cases=sum(Cases)), by=list(Race, GEOID)]
ggplot(vis1, aes(x=Cases)) +
  geom_histogram(position="dodge", binwidth = 0.5, fill = 'darkblue', color='white')+
  scale_x_continuous(trans = scales::log1p_trans()) +
  facet_wrap(~Race, ncol=2)+
  theme_light()

#This plot shows distribution of tracts by log(cases) for all years combined by ethnicity
#Unknown ethnicity is excluded
vis2<-fin[Race=='All' & !(Ethnicity %in% c('U', 'All')), list(Cases=sum(Cases)), by=list(Ethnicity, GEOID)]
ggplot(vis2, aes(x=Cases)) +
  geom_histogram(position="dodge", binwidth = 0.5, fill = 'darkblue', color='white')+
  scale_x_continuous(trans = scales::log1p_trans()) +
  facet_wrap(~Ethnicity, ncol=2)+
  theme_light()

#For any given race/ethnicity (except for unknown), less than 1% of tracts have 1+ cases
fin[, sum(Cases==0)/.N, by=list(Race, Ethnicity)]

#These histograms illustrate a large degree of excess zeros for all race and ethnicity groups.
#Therefore, we we will need to use a model that can account for zero-inflation. 
rm(vis1, vis2)
#-------------------------------------------------------------------------------
# Check mean and variance of cases by tract
#-------------------------------------------------------------------------------
fin[, list(Mean=mean(Cases), Variance = var(Cases), Ratio = var(Cases)/mean(Cases)), 
           by = c('Race', 'Ethnicity')]
#It looks like in general the variance is > the mean for most race/ethnicity groups
#I'm going to run a zero-inflated Poisson first and check the dispersion parameter
#I'm thinking I'll probably end up with a zero-inflated negative binomial, but we'll see

###########################  Model Selection   ##################################
#I'm making a very simplified dataset so I can test out some different models without
#overloading my RAM

temp<-fin[Race=='B' & sviyear==2000 & Cases <= estimate, 
          list(Cases=sum(Cases), Pop=sum(estimate)), 
          by = list(STCNTY, GEOID, sviyear, Sex, State, RPL_THEMES, quartile, UrCode)]

#-------------------------------------------------------------------------------
# Zero-inflated Poisson with population as offset
#-------------------------------------------------------------------------------
library('pscl')

# I have records with zero population so it doesn't work to include population as an offset
# (i.e., log(0)=-inf error), so I can include it as in the inflation-portion of the model only...
model.zip1 = zeroinfl(Cases ~ RPL_THEMES + Sex | #Poisson portion
                     Pop, #Bernoulli portion
                     dist = 'poisson',
                     data = temp)
summary(model.zip1)

# Dispersion statistic
E2 <- resid(model.zip1, type = "pearson")
N  <- nrow(temp)
p  <- length(coef(model.zip1))  
sum(E2^2) / (N - p) #slightly over-dispersed

# OR I can drop records with zero population counts and include the log of pop as an offset
# I think this approach makes more sense because the results provide a rate and I don't want to model
# rates from a zero population anyway. Also, this approach eliminates a source of structural zeros while
# maintaining random zeros. 
model.zip2 = zeroinfl(Cases ~ RPL_THEMES + Sex + offset(log(Pop)) | #Poisson portion
                     1, #Bernoulli portion
                     dist = 'poisson',
                     data = temp[Pop>0,])
summary(model.zip2)

# Dispersion statistic
E2 <- resid(model.zip2, type = "pearson")
N  <- nrow(temp[Pop>0,])
p  <- length(coef(model.zip2))  
sum(E2^2) / (N - p) # a little more over-dispersed

#Adding urban/rural code to the binomial component
model.zip3 = zeroinfl(Cases ~ RPL_THEMES + Sex +  offset(log(Pop)) | #Poisson portion
                      UrCode, #Bernoulli portion
                      dist = 'poisson',
                      data = temp[Pop>0,])
summary(model.zip3)

# Dispersion statistic
E2 <- resid(model.zip3, type = "pearson")
N  <- nrow(temp[Pop>0,])
p  <- length(coef(model.zip3))  
sum(E2^2) / (N - p) # a little less over-dispersed than model 2

#Compare model 2 to model 3
library('lmtest')
lrtest(model.zip2, model.zip3) #Model 3 looks better

#-------------------------------------------------------------------------------
# Zero-inflated negative binomial with population as offset
#-------------------------------------------------------------------------------
model.zinb4 = zeroinfl(Cases ~ RPL_THEMES + Sex + offset(log(Pop)) | #Negative-binomial portion
                      UrCode, #Bernoulli portion
                      dist = 'negbin',
                      data = temp[Pop>0,])
summary(model.zinb4)  # STANDARD ERRORS ARE MISSING!?

# Dispersion statistic
E2 <- resid(model.zinb4, type = "pearson")
N  <- nrow(temp[Pop>0,])
p  <- length(coef(model.zinb4))  
sum(E2^2) / (N - p) # a little less over-dispersed than model 3

#Taking out Urban-Rural code for zero-inflation process
model.zinb5 = zeroinfl(Cases ~ RPL_THEMES + Sex + offset(log(Pop)) |  #Negative-binomial portion
                       1, #Bernoulli portion
                       dist = 'negbin',
                       link = 'logit',
                       data = temp[Pop>0,])
summary(model.zinb5)  

# Dispersion statistic
E2 <- resid(model.zinb5, type = "pearson")
N  <- nrow(temp[Pop>0,])
p  <- length(coef(model.zinb5))  
sum(E2^2) / (N - p) # a little less over-dispersed than model 3

lrtest(model.zip3, model.zinb5) #Model 5 looks better
