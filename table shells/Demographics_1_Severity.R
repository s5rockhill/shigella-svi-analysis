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
pop<-read.csv("population_est_by_age_sex.csv")
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
dat<-read.csv("FoodNet_NARMS/analytic_file_V4.csv")

#Classify cases by number of resistance categories
#-------------------------------------------------------------------------------
dat$Abx1More<-dat$AbxResSum>=1
dat$Abx2More<-dat$AbxResSum>=2
dat$Abx3More<-dat$AbxResSum>=3
dat$Abx4More<-dat$AbxResSum>=4

#-------------------------------------------------------------------------------
#Calculate number of cases by categories and age groups for age-standardization
#-------------------------------------------------------------------------------
#We can use other breaks for age standardization if desired, but they have to align with 
#available population denominators for all demographic categories
dat$AgeStd<-cut(dat$AgeClean, breaks=c(0, 5, 15, 25, 35, 45, 55, 65, 75, 85, 999), right=F, labels=StdAgeCat)

varlist<-c('Sex', "Ethnicity", "Race",  "State", 
           "SpeciesClean", "AmpR", "CotR", "CipR", 'AxoR', 'AzmR', 
           'Abx1More', 'Abx2More', 'Abx3More', 'Abx4More', 'MDR', 'XDR')

Result <- data.frame(matrix(nrow = 0, ncol = 10))
for (i in 1:length(varlist)){
  t<-dat[, list(
                Total=.N, 
                NonSevere=sum(NonSevere==T, na.rm=T), 
                Fever=sum(Fever=='YES', na.rm=T), 
                Hospitalization=sum(HospClean==T, na.rm=T),
                Bloody_Diarrhea=sum(BloodyDiarr=='YES', na.rm=T),
                Bacteremia=sum(Bacteremia==T, na.rm=T),
                Death=sum(Death==T, na.rm=T)), 
         by=c('AgeStd', eval(varlist[i]))]
  
  setnames(t, varlist[i], "Values")
  t$Category<-varlist[i]
  Result<-rbind(Result, t)
}

#Drop FALSE and NA for abx resistance categories
Result<-subset(Result, !(Category %in% varlist[6:16] & Values %in% c(NA, FALSE)))

#-------------------------------------------------------------------------------
# Merge aggregated cases with total population counts for age-adjusted IR 
# calculation. Note, cases with unknown age are excluded from IR calculation
#-------------------------------------------------------------------------------

#First, we need to replicate t1 for all categories that are age-adjusted to 
# total population, then we will format remaining population tables for stacking
t1<-do.call("rbind", replicate(11+length(unique(dat$SpeciesClean)), t1, simplify = FALSE))
t1$Category<-c(rep(varlist[6:16], each=10), 
               rep('SpeciesClean', each=10, times=length(unique(dat$SpeciesClean))))
t1$Values<-c(rep(TRUE, each=10, times=11), rep(unique(dat$SpeciesClean), each=10))

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
cols<-c('Total', 'NonSevere', 'Fever', 'Hospitalization', 'Bloody_Diarrhea', 'Bacteremia', 'Death')
ir[ , (cols) := lapply(.SD, nafill, fill=0), .SDcols = cols]

#Calculate incidence rate and confidence limits within each category
ir<-ir[, as.list(unlist(
         lapply(.SD, function(x) 
           round(
           ageadjust.direct(x, Population, rate=NULL, std.pop$Population)[2:4]
           *100000, digits = 3)))), 
         by=c('Category', 'Values'),
         .SDcols=names(ir)[5:11]]

#Replace lcl NaN values with zero
ir[is.nan(ir)] <- 0   

#-------------------------------------------------------------------------------
#Calculate total cases and row percents within each category
#-------------------------------------------------------------------------------
Result<-Result[, lapply(.SD, sum), by=c('Category', 'Values'), .SDcols=cols]

Result[,paste0(cols, ".Perc") := lapply(.SD, function(x) round(x/Total*100, 1)), 
       .SDcols=cols]

#-------------------------------------------------------------------------------
#Merge age-adjusted incidence rates and confidence limits
#-------------------------------------------------------------------------------
Result<-merge(Result, ir, by=c('Category', 'Values'), all=T)



#-------------------------------------------------------------------------------
#Order Columns for output (makes copying and pasting easier) and output
#-------------------------------------------------------------------------------
setcolorder(Result, c('Category', 'Values', 
                      paste0(rep(cols, each=5), c("", ".Perc", ".adj.rate", ".lci", ".uci"))))
                    
setwd('//cdc.gov/project/ATS_GIS_Store12/Shigella_SVI/Output Datasets For Follow-Up')
write.xlsx(Result, 'Preliminary Results for Table 1.xlsx')

# End
#-------------------------------------------------------------------------------
  
