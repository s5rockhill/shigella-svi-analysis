library(data.table)
library(stringr)

#Import shigella case data
#-------------------------------------------------------------------------------
setwd('//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/FoodNet_NARMS')
dat<-read.csv("analytic_file_V1.csv", stringsAsFactors = F)

#Keep selected columns of interest for demographic tables
keep.fields<-c("ID",  "Year", 'State', "CountyName" , "CTNO2000", "CTNO2010" ,
               "Race", "Ethnicity",  "Sex", 'AgeRangeNew', 'AgeClean', 
               "SpeciesClean", "AmpR", "CotR", "CipR", 'CtxR', 'AzmR', "MDR", "XDR", "MT2Abx",
               "Fever", "HospClean", "BloodyDiarr", "Bacteremia", "Death", "NonSevere")

dat<-setDT(dat[, keep.fields])

#Calculate number of cases by demographic categories
#-------------------------------------------------------------------------------
demvarlist<-c('Sex', 'AgeRangeNew', "Ethnicity", "Race", "SpeciesClean", "State")
Result <- data.frame(matrix(nrow = 0, ncol = 9))

for (i in 1:length(demvarlist)){
  t<-dat[, list(
                Total=.N, 
                NonSevere=sum(NonSevere==T, na.rm=T), 
                Fever=sum(Fever=='YES', na.rm=T), 
                Hospitalization=sum(HospClean==T, na.rm=T),
                Bloody_Diarrhea=sum(BloodyDiarr=='YES', na.rm=T),
                Bacteremia=sum(Bacteremia==T, na.rm=T),
                Death=sum(Death==T, na.rm=T)), by=eval(demvarlist[i])]
  setnames(t, demvarlist[i], "Values")
  t$Category<-demvarlist[i]
  Result<-rbind(Result, t)
}

#Calculate row percent within each demographic category
#-------------------------------------------------------------------------------
Result[, c(paste0(names(Result)[-c(1, 9)], ".Perc")) := 
           lapply(.SD, function(x) round(x/Total*100, 1)), 
           .SDcols=names(Result)[-c(1, 9)]]

#Import population data
#-------------------------------------------------------------------------------
setwd('//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/ACS Data/Population Estimates')
pop<-read.csv('Population_data_combined.csv')
setDT(pop)

pop[,State := substr(str_pad(GEOID, 11, side='left', pad='0'), 1, 2)]

#There are 14 records with a state value of 00, 02, or 03, but these have zero
#population counts, so they will not affect totals
pop<-subset(pop, !(State %in% c('00', '02', '03')))
pop$State<-factor(pop$State, levels=c(row.names(table(pop$State))),
                  labels=c('CA', 'CO', 'CT', 'GA', 'MD', 'MN', 'NM', 'NY', 'OR', 'TN'))

#Duplicate rows 2000 data for 2005 and 2010 data for 2006-2009
#----------------------------------------------------------------
pop$year[pop$year==2000]<-2004
pop05<-pop[which(year==2004), 2:41]
pop05$year<-2005

pop06_09<-pop[rep(which(year==2010), 4), 2:41]
pop06_09$year<-rep(2006:2009, each=nrow(pop[which(year==2010),]))

pop<-rbind(pop, pop05, pop06_09)
rm(pop05, pop06_09)

#Aggregate population counts
#----------------------------------------------------------------
t2<-pop[, list(
    Total=sum(totalpop), 
    "F"    =sum(est.Female_Total), 
    "M"    =sum(est.Male_Total),
    "0-4"  =sum(est.Under5),
    "5-17" =sum(est.age5to17),
    "18-29"=sum(est.age18to29),
    "30-39"=sum(est.age30to50), #this field was mis-labeled        
    "40-49"=sum(est.age40to49),
    "50+"  =sum(est.ageover50),
    "N"    =sum(est.Hispanic_Latino),
    "H"    =sum(est.Non_Hispanic),
    "B"    =sum(est.Black_Alone),
    "A"    =sum(est.Asian_Alone),
    "W"    =sum(est.White_Alone),
    "I"    =sum(est.AIAN_Alone),
    "O"    =sum(est.Other_Alone),       
    "M"    =sum(est.Two_More),
    "P"    =sum(est.NHPI_Alone))]

t2 = melt(t2, measure.vars = 1:18, variable.name = "Values", value.name = "Population")

#Aggregate by state
t3<-pop[, list(Population=sum(totalpop)), by=c('State')]
names(t3)[1]<-'Values'

popdat<-rbind(t2, t3)

#Merge aggregated cases with population counts for IR calculation
#----------------------------------------------------------------
Result<-merge(Result, popdat, by='Values', all.x=T)
Result$Population[Result$Category=='SpeciesClean']<-popdat$Population[popdat$Values=='Total']

#Calculate incidence rate within each demographic category
#-------------------------------------------------------------------------------
Result[, c(paste0(names(Result)[2:8], ".IR")) := 
         lapply(.SD, function(x) round(x/Population*100000, 1)), 
         .SDcols=names(Result)[2:8]]

#Order Columns for output (makes copying and pasting easier)
#-------------------------------------------------------------------------------
setcolorder(Result, c('Category', 'Values', "Total", "Total.Perc",
                      "NonSevere", "NonSevere.Perc" , "Fever", "Fever.Perc", 
                      "Hospitalization",  "Hospitalization.Perc",
                      "Bloody_Diarrhea",  "Bloody_Diarrhea.Perc" ,
                      "Bacteremia", "Bacteremia.Perc", "Death", "Death.Perc"))