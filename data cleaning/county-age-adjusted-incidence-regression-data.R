library('dplyr')
library('stringr')
library('openxlsx')
library('tigris')
library('sf')

#-------------------------------------------------------------------------------
#Import shigella case data 
#-------------------------------------------------------------------------------
folder<-'//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/Final Datasets/'

dat<-read.csv(paste0(folder, "analytic_file_final_06162023.csv"), stringsAsFactors = F,
        colClasses = c('CTNO2000'='character', 'CTNO2010'='character', 
                       'Deterministic'='character', 'CNTYFIPS'='character'))

#-------------------------------------------------------------------------------
#Create aggregation fields
#Note: American Indian/Alaska Native is top-coded
#-------------------------------------------------------------------------------
AgeLabs<-c('0-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44', 
           '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85+')
svibreaks<-c(2000, 2006, 2011, 2015, 2017, 2020)
svilabels<-c('2000', '2010', '2014', '2016', '2018')

dat <- dat %>%
  mutate(AgeRange=cut(Age, breaks=c(seq(0, 85, by=5),999), right=F, labels=AgeLabs),
         raceeth= ifelse(RacEthGroupA %in% c('Asian-NH', 'NatHwn-PI'), 'Asian, NH and PI',  
                    ifelse(RacEthGroupA %in% c('Multiracial-NH', 'Other-NH', 'Unknown'),  'Other-Unknown-multi', 
                                RacEthGroupA)),
         sviyear=cut(Year, breaks=svibreaks, right=F, labels=svilabels))

#-------------------------------------------------------------------------------
#Classify county based on CNTYFIPS if available and otherwise based on foodnet 
#county. Adds land area for population density calculation
#-------------------------------------------------------------------------------
counties<-counties(cb=T) %>%
  st_drop_geometry() %>%
  select(GEOID, NAME, STUSPS, ALAND) %>%
  mutate(NAME=ifelse(GEOID=='24510', 'BALTIMORE CITY', 
            toupper(stringi::stri_trans_general(NAME, "Latin-ASCII"))))

dat <- dat %>%
  mutate(County=ifelse(County=='PRINCE GEORGES', "PRINCE GEORGE'S", County)) %>%
  left_join(counties, join_by(State==STUSPS, County==NAME)) %>%
  mutate(CNTYFIPS=ifelse(is.na(CNTYFIPS), GEOID, CNTYFIPS))

#-------------------------------------------------------------------------------
# Sum cases by year, county, age group, and race/ethnicity category 
#-------------------------------------------------------------------------------
CaseCounts<-dat %>%
  group_by(sviyear, CNTYFIPS, AgeRange, raceeth) %>%
  summarise(Cases=n()) %>%
  filter(raceeth != 'Other-Unknown-multi' & !is.na(AgeRange))

#-------------------------------------------------------------------------------
# Import 2004-2019 County population data for foodnet surveillance area
#-------------------------------------------------------------------------------
folder<-'//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/FoodNet Population Data/'

pop<-read.csv(paste0(folder, 'std_ages_nchs_pop_by_county.csv'), 
             stringsAsFactors = F, colClasses = c("GEOID"='character'))
#-------------------------------------------------------------------------------
# Merge population and case counts. Executing a left join to keep tracts with 0 cases. 
#-------------------------------------------------------------------------------
CaseCounts$sviyear<-as.numeric(as.character(CaseCounts$sviyear))
fin<-pop %>%
  rename(CNTYFIPS=GEOID) %>%
  left_join(CaseCounts, join_by(sviyear, CNTYFIPS, AgeRange, raceeth)) %>%
   mutate(Cases=tidyr::replace_na(Cases, 0)) %>%
  filter(Pop >= Cases)

#-------------------------------------------------------------------------------
# Calculate age-adjusted expected cases
#-------------------------------------------------------------------------------
fin <- fin %>%
  mutate(WeightedRate = (Cases/Pop)*Weight) %>%
  group_by(CNTYFIPS, sviyear, raceeth) %>%
  summarise(Pop=sum(Pop), Cases=sum(Cases), AdjRate=sum(WeightedRate, na.rm=T)) %>%
  mutate(AdjCases = round(AdjRate*Pop))

#-------------------------------------------------------------------------------
#Import NCHS Urban-Rural classification and merge to final dataset
#-------------------------------------------------------------------------------
folder<-'//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/NCHS/'
nchs<-read.xlsx(paste0(folder, 'NCHSURCodes2013.xlsx'))
nchs$CNTYFIPS<-str_pad(nchs$FIPS.code, 5, "left", "0")
nchs$UrCode<-nchs$`2013.code`

fin<-fin %>%
  left_join(nchs[,10:11], join_by(CNTYFIPS)) %>%
  rename(County=CNTYFIPS)
            
#--------------------------------------------------------------------------------
# Merge in additional SDOH covariates: number of childcare businesses and
# temporary shelters, and healthcare provider shortage area designation
#--------------------------------------------------------------------------------
source('helper funcs/county_business_patterns.R')

fin <- fin %>%
  left_join(cbp, join_by(County, sviyear==cbp_year))

source('helper funcs/ahrq_sdoh_hpsa.R')

fin <- fin %>%
  left_join(sdoh, join_by(County, sviyear))

#-------------------------------------------------------------------------------
# Merge to 2000-2018 SVI scores for FoodNet Counties 
#-------------------------------------------------------------------------------
source('helper funcs/calc-region-specific-svi-score-county.R')

fin<-fin %>%
  left_join(svi_all, join_by(County, sviyear==Year))

#-------------------------------------------------------------------------------
# Merge land area population density
#-------------------------------------------------------------------------------
density<-pop %>%
    group_by(GEOID, sviyear) %>%
    summarise(Pop=sum(Pop)) %>%
    left_join(counties[, c('GEOID', 'ALAND')], join_by(GEOID)) %>%
    mutate(PopDensity = Pop/(ALAND/1000000)) %>%
    select(GEOID, sviyear, ALAND, PopDensity)

fin <- fin %>%
  left_join(density, join_by(County==GEOID, sviyear))

#-------------------------------------------------------------------------------
# Merge in HIFLD prison data
#-------------------------------------------------------------------------------
source('helper funcs/hifld_prisons.R')

fin <- fin %>%
  left_join(prisons_agg, join_by(County)) %>%
  mutate(Prisons=replace_na(Prisons, 0), Capacity=replace_na(Capacity, 0))

########################## END DATA PREPARATION  #####################################

#-------------------------------------------------------------------------------
#Output final datasets
#-------------------------------------------------------------------------------
output.location<-c("//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/Final Datasets/Final_County_")

write.csv(fin, paste0(output.location, "AgeAdjusted_ByRaceEth.csv"), row.names=F)