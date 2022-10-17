library('data.table')
library('stringr')
library('openxlsx')
library('epitools')

setwd('//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/')

#-------------------------------------------------------------------------------
#U.S. 2000 population for age standardization
#-------------------------------------------------------------------------------
StdAgeCat<-c('0-4', '5-14', '15-24', '25-34', '35-44', '45-54', '55-64', '65-74','75-84', '85+')
std.pop<-data.frame(
  AgeStd= as.factor(StdAgeCat), 
  Pop=c(18986520, 39976619, 38076743, 37233437, 44659185, 37030152, 23961506, 18135514,  12314793, 4259173))

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

#Calculate total pop and pop from 2011 and forward because Azm was only tested for beginning in 2011
t1<-pop[Race=='All' & Ethnicity=='All', list(Pop=sum(estimate), Pop11=sum(estimate[year>2010])), by=list(AgeStd)]
t2<-pop[Race=='All' & Ethnicity=='All', list(Pop=sum(estimate), Pop11=sum(estimate[year>2010])), by=list(AgeStd, Sex)]
t3<-pop[Race=='All' & Ethnicity=='All', list(Pop=sum(estimate), Pop11=sum(estimate[year>2010])), by=list(AgeStd, AgeGroup)]
t4<-pop[Race!='All' & Ethnicity=='All', list(Pop=sum(estimate), Pop11=sum(estimate[year>2010])), by=list(AgeStd, Race)]
t5<-pop[Race=='All' & Ethnicity!='All', list(Pop=sum(estimate), Pop11=sum(estimate[year>2010])), by=list(AgeStd, Ethnicity)]
t6<-pop[Race=='All' & Ethnicity=='All', list(Pop=sum(estimate), Pop11=sum(estimate[year>2010])), by=list(AgeStd, State)]


#Replicate t1 for categories that are age-adjusted to total pop, then format remaining tables for stacking.
species<-unique(dat$SpeciesClean)

t1<-do.call("rbind", replicate(1+length(species), t1, simplify = FALSE))
t1$Category<-c(rep("Total", 10), rep('SpeciesClean', each=10, times=length(species)))
t1$Values<-c(rep("All", 10),  rep(species, each=10))

names(t2)[colnames(t2)=='Sex']<-'Values'
t2$Category<-'Sex'

names(t3)[colnames(t3)=='AgeGroup']<-'Values'
t3$Category<-'AgeGroup'

names(t4)[colnames(t4)=='Race']<-'Values'
t4$Category<-'Race'

names(t5)[colnames(t5)=='Ethnicity']<-'Values'
t5$Category<-'Ethnicity'

names(t6)[colnames(t6)=='State']<-'Values'
t6$Category<-'State'

poptab<-rbind(t1, t2, t3, t4, t5, t6)
rm(t1, t2, t3, t4, t5, t6)

#-------------------------------------------------------------------------------
# Import shigella case data
#-------------------------------------------------------------------------------
dat<-read.csv("FoodNet_NARMS/analytic_file_V6.csv", stringsAsFactors = F)
dat$AgeGroup[dat$AgeGroup=='9-May']<-'5-9'
dat$AgeGroup[dat$AgeGroup=='19-Oct']<-'10-19'

setDT(dat)

#Classify cases by number of resistance categories
dat$Abx1More<-dat$AbxResSum>=1
dat$Abx2More<-dat$AbxResSum>=2
dat$Abx3More<-dat$AbxResSum>=3
dat$Abx4More<-dat$AbxResSum>=4

#Classify by Age Group
dat$AgeGroup<-cut(dat$AgeClean, breaks=c(0, 5, 15, 25, 45, 65, 999), right=F, 
                  labels=c('0-4', '5-14', '15-24', '25-44', '45-64', '65+'))
dat$AgeGroup<-questionr::addNAstr(dat$AgeGroup, value='U')

#Cut standard age categories for rate adjustment
dat$AgeStd<-cut(dat$AgeClean, breaks=c(0, 5, seq(15, 85, by=10), 999), right=F, labels=StdAgeCat)

#Keep cases in SupPopNew
dat<-dat[SubPopNew==T, ]

#-------------------------------------------------------------------------------
# Resistance category cases by demographics and standard age categories
#-------------------------------------------------------------------------------
cols<-c("AmpR", "CotR",  "CipDSC", "CipR", 'AxoR', 'AzmR', 'Abx1More')

varlist<-c('Sex', "AgeGroup", "Ethnicity", "Race", "SpeciesClean", "State")

Result<-merge(dat[, list(Category="Total", Values='All', Cases=.N, Cases11=sum(Year>2010)), by='AgeStd'],
              dat[, lapply(.SD, function(x) sum(x==T, na.rm=T)), .SDcols=cols, by='AgeStd'],
              by='AgeStd')
            
for (i in (varlist)){
  x<-merge(dat[, list(Category=i, Cases=.N, Cases11=sum(Year>2010)), by=c(i, 'AgeStd')],
           dat[, lapply(.SD, function(x) sum(x==T, na.rm=T)), .SDcols=cols, by=c(eval(i), 'AgeStd')],
           by=c(i, 'AgeStd'))
  setnames(x, i, "Values")
  Result<-rbind(Result, x)}

#-------------------------------------------------------------------------------
# Merge cases to population counts for rate calculation
#-------------------------------------------------------------------------------
ir<-merge(poptab, Result[, !c("Cases", "Cases11")], by=c('Category', 'Values', 'AgeStd'), all.x=T)

#Fill in NA values for records that have zero cases in that category
ir[ , c(cols)  := lapply(.SD, nafill, fill=0), .SDcols = cols]

#Calculate incidence rate and confidence limits within each category
temp<-ir[, as.list(unlist(
      lapply(.SD, function(x) 
         round(ageadjust.direct(x, Pop, rate=NULL, std.pop$Pop)[2:4]*100000, digits=3)))), 
              by=c('Category', 'Values'), .SDcols=cols[c(-6)]]


#Calculate AzmR incidence rate and confidence limits within each category
temp2<-ir[, as.list(unlist(
            round(ageadjust.direct(AzmR, Pop11, rate=NULL, std.pop$Pop)[2:4]*100000, digits=3))), 
            by=c('Category', 'Values')]
names(temp2)[3:5]<-paste0('AzmR.', names(temp2)[3:5])

#Merge AzmR incidence rates with other abx resistance categories
ir<-merge(temp, temp2, by=c('Category', 'Values'), all=T)
rm(temp, temp2)

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
# Sum cases over standard age categories and calculate % of cases in each 
# demographic group by resistance category 
#-------------------------------------------------------------------------------
Result<-Result[, lapply(.SD, sum), .SDcols=c('Cases', 'Cases11', cols), by=list(Category, Values) ]
Result[, paste0(cols[-c(6)],".Perc") := lapply(.SD, function(x) round(x/Cases*100, 1)), .SDcols=cols[-c(6)]]
Result[, AzmR.Perc := round(AzmR/Cases11*100, 1)]

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
                        levels=c('Total', 'Sex', 'AgeGroup', 'Ethnicity', 'Race', 'SpeciesClean', 'State'))

Result$Values<-as.character(Result$Values)
setorderv(Result, c('Category', 'Values'))

Result[, c('Cases', 'Cases11') := NULL]

setwd('//cdc.gov/project/ATS_GIS_Store12/Shigella_SVI/Preliminary Results')
write.xlsx(Result, paste0('Demographics_AMR_', Sys.Date(), '.xlsx'))




