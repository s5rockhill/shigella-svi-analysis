library('data.table')
library('stringr')
library('openxlsx')

setwd('//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/')

#-------------------------------------------------------------------------------
# Import shigella case data
#-------------------------------------------------------------------------------
dat<-read.csv("FoodNet_NARMS/analytic_file_V6.csv")
setDT(dat)

#Classify cases by number of resistance categories
dat$Abx1More<-dat$AbxResSum>=1
dat$Abx2More<-dat$AbxResSum>=2
dat$Abx3More<-dat$AbxResSum>=3
dat$Abx4More<-dat$AbxResSum>=4

#-------------------------------------------------------------------------------
# Number of cases in SubPopNew by resistance category and severity
#-------------------------------------------------------------------------------

dat<-dat[SubPopNew==T, ]

cols<-c('Severe', 'Fever', 'HospClean', 'BloodyDiarr', 'Bacteremia', 'Death')

Result<-cbind(dat[, list(Category='Total', Total=.N, NonSevere=sum(Severe==F))],
              dat[, lapply(.SD, function(x) sum(x %in% c("YES", T), na.rm=T)),  .SDcols=cols])

varlist<-c("AmpR", "CotR",  "CipDSC", "CipR", 'AxoR', 'AzmR', 'Abx1More', 'Abx2More', 'Abx3More', 'Abx4More', 'MDR', 'XDR')
for (i in (varlist)){
  x<-cbind(dat[get(i)==T, list(Category=i, Total=.N, NonSevere=sum(Severe==F))],
           dat[get(i)==T, lapply(.SD, function(x) sum(x %in% c("YES", T), na.rm=T)), .SDcols=cols])
  Result<-rbind(Result, x)
  rm(x)
}

#-------------------------------------------------------------------------------
# Percent of cases in resistance category and severity
#-------------------------------------------------------------------------------
cols<-c('Total', 'NonSevere', cols)
Result[, paste0(cols,".Perc") := lapply(.SD, function(x) round(x/x[1]*100, 1)), .SDcols=cols]

#-------------------------------------------------------------------------------
# Set order of columns and output
#-------------------------------------------------------------------------------

setcolorder(Result, c('Category', paste0(rep(cols, each=2), c("", ".Perc"))))

setwd('//cdc.gov/project/ATS_GIS_Store12/Shigella_SVI/Output Datasets For Follow-Up')
write.xlsx(Result, paste0('Resistance_1_Severity_', Sys.Date(),  '.xlsx'))
