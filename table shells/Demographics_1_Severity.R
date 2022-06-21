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

t1<-pop[Race=='All' & Ethnicity=='All', list(Population=sum(estimate)), by=AgeStd]
t2<-pop[Race=='All' & Ethnicity=='All', list(Population=sum(estimate)), by=list(AgeStd, Sex)]
t3<-pop[Race!='All' & Ethnicity=='All', list(Population=sum(estimate)), by=list(AgeStd, Race)]
t4<-pop[Race=='All' & Ethnicity!='All', list(Population=sum(estimate)), by=list(AgeStd, Ethnicity)]
t5<-pop[Race=='All' & Ethnicity=='All', list(Population=sum(estimate)), by=list(AgeStd, State)]

#-------------------------------------------------------------------------------
#Import shigella case data
#-------------------------------------------------------------------------------
dat<-read.csv("FoodNet_NARMS/analytic_file_V5.csv")
setDT(dat)

#Classify cases by number of resistance categories
#-------------------------------------------------------------------------------
dat$Abx1More<-dat$AbxResSum>=1
dat$Abx2More<-dat$AbxResSum>=2
dat$Abx3More<-dat$AbxResSum>=3
dat$Abx4More<-dat$AbxResSum>=4

#-------------------------------------------------------------------------------
#Calculate number of cases by categories and age groups for age-standardization
#-------------------------------------------------------------------------------
dat$AgeStd<-cut(dat$AgeClean, breaks=c(0, 5, seq(15, 85, by=10), 999), right=F, labels=StdAgeCat)

cols<-c('NonSevere', 'Fever', 'HospClean', 'BloodyDiarr', 'Bacteremia', 'Death')

Result<-merge(dat[, list(Category='Total', Values='All', Total=.N), by='AgeStd'],
              dat[, lapply(.SD, function(x) sum(x %in% c("YES", T), na.rm=T)), by='AgeStd', .SDcols=cols],
              by='AgeStd')

varlist<-c('Sex', "Ethnicity", "Race",  "State", 
           "SpeciesClean", "AmpR", "CotR", "CipR", 'AxoR', 'AzmR', 
           'Abx1More', 'Abx2More', 'Abx3More', 'Abx4More', 'MDR', 'XDR')

for (i in (varlist)){
  x<-dat[, list(Category=i, Total=.N) , by=c('AgeStd', eval(i))]
  y<-dat[, lapply(.SD, function(x) sum(x %in% c("YES", T), na.rm=T)), by=c('AgeStd', eval(i)), .SDcols=cols]
  z<-merge(x, y, by=c(i, 'AgeStd'))
  setnames(z, i, "Values")
  Result<-rbind(Result, z)
  rm(x, y, z)
}

#Drop FALSE and NA for abx resistance categories
Result<-subset(Result, !(Category %in% varlist[6:16] & Values %in% c(NA, FALSE)))

#-------------------------------------------------------------------------------
# Merge aggregated cases with total population counts for age-adjusted IR 
# calculation. Note, cases with unknown age are excluded from IR calculation
#-------------------------------------------------------------------------------

#First, we need to replicate t1 for the 12 categories that are age-adjusted to total population, 
#then we will format remaining population tables for stacking
species<-unique(dat$SpeciesClean)

t1<-do.call("rbind", replicate(12+length(species), t1, simplify = FALSE))
t1$Category<-c(rep("Total", 10), rep(varlist[6:16], each=10), rep('SpeciesClean', each=10, times=length(species)))
t1$Values<-c(rep("All", 10), rep(TRUE, each=10, times=11), rep(species, each=10))

names(t2)[colnames(t2)=='Sex']<-'Values'
t2$Category<-'Sex'

names(t3)[colnames(t3)=='Race']<-'Values'
t3$Category<-'Race'

names(t4)[colnames(t4)=='Ethnicity']<-'Values'
t4$Category<-'Ethnicity'

names(t5)[colnames(t5)=='State']<-'Values'
t5$Category<-'State'

t6<-rbind(t1, t2, t3, t4, t5)

#Merge case counts to population table
ir<-merge(t6, Result, by=c('Category', 'Values', 'AgeStd'), all.x=T)

#Fill in NA values for records that have zero cases in that category
cols<-c("Total", cols)
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
Result$Category<-factor(Result$Category, levels=c('Total', 'Sex', 'Ethnicity', 'Race', 'SpeciesClean',
                                                  'State', "AmpR",  "CotR", "CipR", "AxoR", "AzmR", 
                                                  "Abx1More","Abx2More","Abx3More","Abx4More", "MDR", "XDR"))

Result<-Result[order(Category),]
                    
setwd('//cdc.gov/project/ATS_GIS_Store12/Shigella_SVI/Output Datasets For Follow-Up')
write.xlsx(Result, 'Preliminary Results for Table 1.xlsx')

# End
#-------------------------------------------------------------------------------