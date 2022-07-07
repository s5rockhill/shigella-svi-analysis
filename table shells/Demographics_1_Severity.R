library('data.table')
library('stringr')
library('tidycensus')
library('purrr')
library('epitools')
library('openxlsx')

#-------------------------------------------------------------------------------
#U.S. 2000 population for age standardization
#-------------------------------------------------------------------------------
StdAgeCat<-c('0-4', '5-14', '15-24', '25-34', '35-44', '45-54', '55-64', '65-74','75-84', '85+')
std.pop<-data.frame(AgeStd= as.factor(StdAgeCat), 
                    Population=c(18986520, 39976619, 38076743, 37233437, 44659185, 
                                 37030152, 23961506, 18135514,  12314793, 4259173))

#-------------------------------------------------------------------------------
#Import and aggregate population counts
#-------------------------------------------------------------------------------
setwd('//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/')
pop<-read.csv("FoodNet Population Data/foodnet_county_population_2004_2019.csv")
setDT(pop)

pop$AgeStd<-factor(pop$AgeStd, levels=StdAgeCat)
pop$AgeGroup<-factor(
               ifelse(pop$AgeStd %in% c('25-34','35-44'), '25-44', 
                 ifelse(pop$AgeStd %in% c('45-54','55-64'), '45-64', 
                   ifelse(pop$AgeStd %in% c('65-74','75-84', '85+'), '65+', 
                      as.character(pop$AgeStd)))), 
               levels=c('0-4', '5-14', '15-24', '25-44', '45-64', '65+'))
                 
t1<-pop[Race=='All' & Ethnicity=='All', list(Population=sum(estimate)), by=AgeStd]
t2<-pop[Race=='All' & Ethnicity=='All', list(Population=sum(estimate)), by=list(AgeStd, AgeGroup)]
t3<-pop[Race=='All' & Ethnicity=='All', list(Population=sum(estimate)), by=list(AgeStd, Sex)]
t4<-pop[Race!='All' & Ethnicity=='All', list(Population=sum(estimate)), by=list(AgeStd, Race)]
t5<-pop[Race=='All' & Ethnicity!='All', list(Population=sum(estimate)), by=list(AgeStd, Ethnicity)]
t6<-pop[Race=='All' & Ethnicity=='All', list(Population=sum(estimate)), by=list(AgeStd, State)]
t7<-pop[year>2010 & Race=='All' & Ethnicity=='All', list(Population=sum(estimate)), by=AgeStd]


#-------------------------------------------------------------------------------
#Import shigella case data
#-------------------------------------------------------------------------------
dat<-read.csv("FoodNet_NARMS/analytic_file_V6.csv")
setDT(dat)

#Classify cases by number of resistance categories
dat$Abx1More<-dat$AbxResSum>=1
dat$Abx2More<-dat$AbxResSum>=2
dat$Abx3More<-dat$AbxResSum>=3
dat$Abx4More<-dat$AbxResSum>=4

#Classify by Age Group
dat$AgeGroup<-cut(dat$AgeClean, breaks=c(0, 5, 15, 25, 45, 65, 999), right=F, 
               labels=c('0-4', '5-14', '15-24', '25-44', '45-64', '65+'))

#-------------------------------------------------------------------------------
#Calculate number of cases by categories and age groups for age-standardization
#-------------------------------------------------------------------------------
dat$AgeStd<-cut(dat$AgeClean, breaks=c(0, 5, seq(15, 85, by=10), 999), right=F, labels=StdAgeCat)


cols<-c('Severe', 'Fever', 'HospClean', 'BloodyDiarr', 'Bacteremia', 'Death')

Result<-merge(dat[, list(Category='Total', Values='All', Total=.N, NonSevere=sum(Severe==F)), by='AgeStd'],
              dat[, lapply(.SD, function(x) sum(x %in% c("YES", T), na.rm=T)), by='AgeStd', .SDcols=cols],
              by='AgeStd')

varlist<-c('Sex', "AgeGroup", "Ethnicity", "Race",  "State", 
           "SpeciesClean", "AmpR", "CipDSC", "CotR", "CipR", 'AxoR', 'AzmR', 
           'Abx1More', 'Abx2More', 'Abx3More', 'Abx4More', 'MDR', 'XDR')

for (i in (varlist)){
  x<-dat[, list(Category=i, Total=.N, NonSevere=sum(Severe==F)) , by=c('AgeStd', eval(i))]
  y<-dat[, lapply(.SD, function(x) sum(x %in% c("YES", T), na.rm=T)), by=c('AgeStd', eval(i)), .SDcols=cols]
  z<-merge(x, y, by=c(i, 'AgeStd'))
  setnames(z, i, "Values")
  Result<-rbind(Result, z)
  rm(x, y, z)
}

#Drop FALSE and NA for abx resistance categories
Result<-subset(Result, !(Category %in% varlist[7:18] & Values %in% c(NA, FALSE)))

#-------------------------------------------------------------------------------
# Merge aggregated cases with total population counts for age-adjusted IR 
# calculation. Note, cases with unknown age are excluded from IR calculation
#-------------------------------------------------------------------------------

#Replicate t1 for categories that are age-adjusted to total population, then
#format remaining population tables for stacking
species<-unique(dat$SpeciesClean)

t1<-do.call("rbind", replicate(12+length(species), t1, simplify = FALSE))
t1$Category<-c(rep("Total", 10), rep(varlist[c(7:11, 13:18)], each=10), rep('SpeciesClean', each=10, times=length(species)))
t1$Values<-c(rep("All", 10), rep(TRUE, each=10, times=11), rep(species, each=10))

names(t2)[colnames(t2)=='AgeGroup']<-'Values'
t2$Category<-'AgeGroup'

names(t3)[colnames(t3)=='Sex']<-'Values'
t3$Category<-'Sex'

names(t4)[colnames(t4)=='Race']<-'Values'
t4$Category<-'Race'

names(t5)[colnames(t5)=='Ethnicity']<-'Values'
t5$Category<-'Ethnicity'

names(t6)[colnames(t6)=='State']<-'Values'
t6$Category<-'State'

t7$Values<-TRUE
t7$Category<-'AzmR'

t8<-rbind(t1, t2, t3, t4, t5, t6, t7)
rm(t1, t2, t3, t4, t5, t6, t7)

#Merge case counts to population table
ir<-merge(t8, Result, by=c('Category', 'Values', 'AgeStd'), all.x=T)

#Fill in NA values for records that have zero cases in that category
cols<-c("Total", "NonSevere", cols)
ir[ , c(cols)  := lapply(.SD, nafill, fill=0), .SDcols = cols]

#Calculate incidence rate and confidence limits within each category
ir<-ir[, as.list(unlist(
         lapply(.SD, function(x) 
           round(ageadjust.direct(x, Population, rate=NULL, std.pop$Population)[2:4]
           *100000, digits = 3)))), 
           by=c('Category', 'Values'), .SDcols=cols]

#Replace lcl NaN values with zero
ir[is.na(ir)] <- 0   

#Format confidence limits into single column
for (i in cols){
    ir[,paste0(i, ".ci"):=
         list(paste0("(", 
                  sprintf("%0.3f", get(paste0(i, ".lci"))), 
                  " - ", 
                    sprintf("%0.3f", get(paste0(i, ".uci"))), 
                      ")"))]
}
       
#-------------------------------------------------------------------------------
#Calculate total cases and row percents within each category
#-------------------------------------------------------------------------------
Result<-Result[, lapply(.SD, sum), by=c('Category', 'Values'), .SDcols=cols]

Result[,paste0(cols,".Perc") := lapply(.SD, function(x) round(x/Total*100, 1)), .SDcols=cols]

#-------------------------------------------------------------------------------
#Merge age-adjusted incidence rates and confidence limits
#-------------------------------------------------------------------------------
ir.cols<-grep("Category|Values|adj.rate|\\.ci", names(ir), value = T)
Result<-merge(Result, ir[,..ir.cols], by=c('Category', 'Values'), all=T)

#-------------------------------------------------------------------------------
#Order Columns for output (makes copying and pasting easier) and output
#-------------------------------------------------------------------------------
setcolorder(Result, c('Category', 'Values', paste0(rep(cols, each=4), c("", ".Perc", ".adj.rate", ".ci"))))
Result$Category<-factor(Result$Category, 
                    levels=c('Total', 'Sex', 'AgeGroup', 'Ethnicity', 'Race', 'SpeciesClean',
                             'State', "AmpR",  "CotR", "CipDSC", "CipR", "AxoR", "AzmR", 
                             "Abx1More","Abx2More","Abx3More","Abx4More", "MDR", "XDR"))

Result$Values<-as.character(Result$Values)
setorderv(Result, c('Category', 'Values'))
                    
setwd('//cdc.gov/project/ATS_GIS_Store12/Shigella_SVI/Output Datasets For Follow-Up')
write.xlsx(Result, 'Preliminary Results for Table 1_07072022.xlsx')


#Median and Range of Age by Severity Category
dat[, list(Median=median(AgeClean, na.rm=T), Range=paste(min(AgeClean, na.rm=T), max(AgeClean, na.rm=T)))]
dat[,list(Median=median(AgeClean, na.rm=T), Range=paste(min(AgeClean, na.rm=T), max(AgeClean, na.rm=T))), by='Severe']
dat[Fever=="YES",list(Median=median(AgeClean, na.rm=T), Range=paste(min(AgeClean, na.rm=T), max(AgeClean, na.rm=T)))]
dat[HospClean==T,list(Median=median(AgeClean, na.rm=T), Range=paste(min(AgeClean, na.rm=T), max(AgeClean, na.rm=T)))]
dat[BloodyDiarr=='YES',list(Median=median(AgeClean, na.rm=T), Range=paste(min(AgeClean, na.rm=T), max(AgeClean, na.rm=T)))]
dat[Bacteremia==T,list(Median=median(AgeClean, na.rm=T), Range=paste(min(AgeClean, na.rm=T), max(AgeClean, na.rm=T)))]
dat[Death==T,list(Median=median(AgeClean, na.rm=T), Range=paste(min(AgeClean, na.rm=T), max(AgeClean, na.rm=T)))]
# End
#-------------------------------------------------------------------------------