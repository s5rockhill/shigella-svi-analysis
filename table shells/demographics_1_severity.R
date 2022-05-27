library('data.table')

#Import foodnet data
setwd('//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/FoodNet_NARMS')
dat<-read.csv('FN_NARMS_PulseNet_Census.csv', stringsAsFactors = F)
setDT(dat)

######################################################
# Key Variables Used for Demographic Tables
#-----------------------------------------------------
# Sex, AgeRange, Ethnicity, Race, State, 
# Serotype, FoodNet Site, and Resistance 
#
# Demographics are stratified by severity of symptoms
#-----------------------------------------------------
# BloodyDiarr
# Fever
# Hospitalization, derived from DtAdmit and Outcome
# Bacteremia
# Death (Outcome=='DIED')
#
######################################################
# Clean demographic variables

sum(is.na(dat$AgeRange)) #count records with missing age

addmargins(table(is.na(dat$PatientAgeYears), is.na(dat$Age), useNA='ifany')) #looks like we can fill in some missing values
dat[AgeUnit=='Months', c('Age', 'AgeRange')] #Double-checking to see if AgeRange was coded correctly



dat[, list(min=min(Age, na.rm=T), max=max(Age, na.rm=T)), by='AgeRange']


# Calculate measures of severity


#Calculate Severity
table(dat$BloodyDiarr, useNA = 'ifany')


#need to group serotypes
table(cases2$serotypesummary)
table(cases2$SpeciesName)

cases2$species=ifelse(cases2$serotypesummary=="SONNEI", "sonnei",
                      ifelse(cases2$serotypesummary=="UNKNOWN"| cases2$serotypesummary==
                               "NOT SPECIATED", "unknown",
                             ifelse(grepl("FLEXNERI", cases2$serotypesummary, fixed=T), "flexneri",
                                    ifelse(grepl("BOYDII", cases2$serotypesummary, fixed=T), "boydii",
                                           "dysenteriae")))) 
