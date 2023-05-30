library('dplyr')

#File and folder locations
folder<-"//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/FoodNet_NARMS/"
narms_file<-'FoodNet_1505_MERGED_NARMSData_No DUPS_2004 to 2019_v05.18.23.csv'

#Import NARMS data 
narms<-read.csv(paste0(folder, narms_file), stringsAsFactors = F)

#-------------------------------------------------------------------------------
#Filter out non-Shigella cases
#-------------------------------------------------------------------------------
#narms<-narms %>% filter(FinalGenusName=='Shigella') 

#-------------------------------------------------------------------------------
# Resistance Profile
#-------------------------------------------------------------------------------
# Categorize resistance for lab-approved isolates not associated with an outbreak 
# (Surveillance ==1 & Exclude ==0 & LabApproval ==1). The field SurveillanceReportable
# flags cases meeting this definition. Cases must also have non-missing data in the
# ResistancePattern Field. L. Dorough indicated (8/1/22) that I could also use a
# valid NarmsLink value (contains the letters "AM") in place of non-missing Resistance
# pattern, but this field is not included in the final dataset. 

narms$ResistancePattern[narms$ResistancePattern==""]<-NA

narms <- narms %>% mutate(SubPop=SurveillanceReportable==1 & !is.na(ResistancePattern))

#-------------------------------------------------------------------------------
# Categorize antibiotic resistance
#-------------------------------------------------------------------------------
abxclassfunc <- function(x, SubPop) (ifelse(SubPop==T & x=='R', T, ifelse(SubPop==T & x !='R', F, NA)))

narms<- narms %>%
  mutate(AmpR = abxclassfunc(AMP_Concl, SubPop),
         AzmR = abxclassfunc(AZM_Concl, SubPop),
         CipDSC = ifelse(SubPop==T & CIP_Concl %in% c('R', 'I'), T, 
                  ifelse(SubPop==T & !CIP_Concl %in% c('R', 'I'), F, NA)),
         CipR = abxclassfunc(CIP_Concl, SubPop),
         CotR = abxclassfunc(COT_Concl, SubPop),
         AxoR = abxclassfunc(AXO_Concl, SubPop))

#Number of antibiotic resistance categories, multi-drug and x-drug resistant
narms <- narms  %>% 
  rowwise() %>%
  mutate(AbxResSum=ifelse(SubPop==T, sum(AmpR, AzmR, CipR, CotR, AxoR, na.rm=T), NA))

#Multi-drug resistance, extensive drug resistance, and no drug resistance flags
narms <- narms  %>% 
  rowwise() %>%
  mutate(MDR=ifelse(SubPop==T & sum(AmpR, AzmR, CipR, CotR)==4, T, 
             ifelse(SubPop==T & sum(AmpR, AzmR, CipR, CotR)<4, F, NA)), 
         XDR=ifelse(SubPop==T & AbxResSum==5, T, 
             ifelse(SubPop==T & AbxResSum<5, F, NA)), 
         NoAbxRes=ifelse(SubPop==T & AbxResSum==0, T, 
                  ifelse(SubPop==T & AbxResSum>0, F, NA))) 

#-------------------------------------------------------------------------------
# Convert NARMS age variable to age in years
#-------------------------------------------------------------------------------
narms <- narms %>%
  mutate(narms_age = ifelse(AgeUnit=='Months' & !is.na(Age), as.numeric(Age)/12, 
                     ifelse(AgeUnit=='Years'  & !is.na(Age), as.numeric(Age), NA)))

#-------------------------------------------------------------------------------
# Select NARMS variables to merge with FoodNet dataset
#-------------------------------------------------------------------------------
narms<-narms %>%
  select(SLabsID, narms_age, AgeUnit, PatientAgeFromPulsenet, PatientSexFromPulsenet,
         Gender, FinalGenusName, FinalSpeciesName, SubPop, AmpR, AzmR, CipR, CipDSC, CotR, AxoR, 
         AbxResSum, MDR, XDR, NoAbxRes, SourceSiteFromPulsenet, SpecimenSource)