library('data.table')
library('stringr')
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

setnames(pop, "year", "Year")
pop$AgeStd<-factor(pop$AgeStd, levels=StdAgeCat)

t1<-pop[Race=='All' & Ethnicity=='All', list(Population=sum(estimate)), by=list(AgeStd, Year)]
t2<-pop[Race=='All' & Ethnicity=='All', list(Population=sum(estimate)), by=list(AgeStd, Sex, Year)]
t3<-pop[Race!='All' & Ethnicity=='All', list(Population=sum(estimate)), by=list(AgeStd, Race, Year)]
t4<-pop[Race=='All' & Ethnicity!='All', list(Population=sum(estimate)), by=list(AgeStd, Ethnicity, Year)]
t5<-pop[Race=='All' & Ethnicity=='All', list(Population=sum(estimate)), by=list(AgeStd, State, Year)]

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

Result<-dat[, list(Category='Total', Values='All', Total=.N), by=c('Year', 'AgeStd')]

varlist<-c('Sex', "Ethnicity", "Race",  "State", 
           "SpeciesClean", "AmpR", "CotR", "CipR", 'AxoR', 'AzmR', 
           'Abx1More', 'Abx2More', 'Abx3More', 'Abx4More', 'MDR', 'XDR')

for (i in varlist) {
  t<-dat[, list(Total=.N, Category=i), by=c('Year', 'AgeStd', eval(i))]
  setnames(t, i, "Values")
  Result<-rbind(Result, t)
  rm(t)
}

#Drop NA for abx resistance categories
Result<-subset(Result, !(Category %in% varlist[6:16] & is.na(Values)))

#-------------------------------------------------------------------------------
# Merge aggregated cases with population counts for age-adjusted IR calculation. 
#Note, cases with unknown demographics are excluded from IR calculation
#-------------------------------------------------------------------------------
#First, we need to replicate t1 for all categories that are age-adjusted to 
#total population, then we will format remaining population tables for stacking

species<-unique(dat$SpeciesClean)

t1<-do.call("rbind", replicate(12+length(species), t1, simplify = FALSE))
t1$Category<-c(rep("Total", 160), rep(varlist[6:16], each=160), rep('SpeciesClean', each=160, times=length(species)))
t1$Values<-c(rep("All", 160), rep(TRUE, times=11*160), rep(species, each=160))

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
ir<-merge(t6, Result, by=c('Category', 'Values', 'Year', 'AgeStd'), all.x=T)

#Fill in NA values for records that have zero cases in that category
ir$Total[is.na(ir$Total)]<-0

#Calculate incidence rate and confidence limits within each category
ir<-ir[, as.list(unlist(
          round(ageadjust.direct(Total, Population, rate=NULL, std.pop$Population)[2:4]
          *100000, digits = 3))), 
          by=c('Category', 'Values', 'Year')]

#Replace lcl NaN values with zero
ir$lci[is.nan(ir$lci)] <- 0   

#Format confidence limits into single column
ir[,ci := paste0("(", sprintf("%0.3f", lci), " - ", sprintf("%0.3f", uci), ")")]

#-------------------------------------------------------------------------------
#Calculate total cases and row percents within each category
#-------------------------------------------------------------------------------
Result<-Result[, list(Total=sum(Total)), by=c('Category', 'Values', 'Year')]
Result[, 'Percent' := list(round(Total/sum(Total)*100, 1)), by=c('Category', 'Year')]

#-------------------------------------------------------------------------------
#Drop FALSE values from Abx resistance categories
#-------------------------------------------------------------------------------
Result<-subset(Result, Values !=FALSE)

#-------------------------------------------------------------------------------
#Merge age-adjusted incidence rates and CI's with Totals and Percents
#-------------------------------------------------------------------------------
Result<-merge(Result, ir[, -c('lci', 'uci')], by=c('Year', 'Category', 'Values'), all=T)

#-------------------------------------------------------------------------------
#Fill in Total/Percent NA's with zeros and transform wide 
#-------------------------------------------------------------------------------
Result$Total[is.na(Result$Total)]<-0
Result$Percent[is.na(Result$Percent)]<-0.0
Result<-dcast(Result, Category+Values ~ Year, value.var = c("Total","Percent","adj.rate","ci"))

#-------------------------------------------------------------------------------
#Order Columns and Rows for output (makes copying and pasting easier) and output
#-------------------------------------------------------------------------------
setcolorder(Result, c('Category', 'Values', 
            paste0(c("Total_", "Percent_", "adj.rate_", "ci_"), rep(2004:2019, each=4))))

Result$Category<-factor(Result$Category, 
                        levels=c('Total', 'Sex', 'Ethnicity', 'Race', 'SpeciesClean',
                                 'State', "AmpR",  "CotR", "CipR", "AxoR", "AzmR", 
                                 "Abx1More","Abx2More","Abx3More","Abx4More", "MDR", "XDR"))

Result<-Result[order(Category),]

setwd('//cdc.gov/project/ATS_GIS_Store12/Shigella_SVI/Output Datasets For Follow-Up')
write.xlsx(Result, 'Preliminary Results for Table 2.xlsx')

# End
#-------------------------------------------------------------------------------

