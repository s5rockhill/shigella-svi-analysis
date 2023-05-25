library('data.table')
library('stringr')
library('openxlsx')
library('RODBC')

#-------------------------------------------------------------------------------
#Import shigella case data and create aggregation fields
#-------------------------------------------------------------------------------
folder<-'//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/Final Datasets/'

dat<-setDT(read.csv(paste0(folder, "analytic_file_final_5192023.csv"), stringsAsFactors = F,
                    colClasses = c('CTNO2000'='character', 'CTNO2010'='character')))

#Classify age
AgeGroupLabs<-c('0-4', '5-14', '15-44', '45+')
dat$AgeRange<-cut(dat$Age, breaks=c(0, 5, 15, 45, 999), right=F, labels=AgeGroupLabs)

#Update missing census tract information using deterministic imputation result
dat[is.na(CTNO2000), CTNO2000 := Deterministic]
dat[is.na(CTNO2010), CTNO2010 := Deterministic]
dat$CTNO2000<-str_pad(dat$CTNO2000, width=11, side='left', pad='0')

#Classify cases by which year of svi we want to join to 
svibreaks<-c(2000, 2006, 2011, 2015, 2017, 2020)
svilabels<-c('2000', '2010', '2014', '2016', '2018')
dat[,'sviyear' := cut(Year, breaks=svibreaks, right=F, labels=svilabels)]

#Classify county
dat[, GEOID := ifelse(Year<2006, str_sub(CTNO2000, 1, 5), str_sub(CTNO2010, 1, 5))]

#Classify race and ethnicity (top-coding american indian)
dat[, 'raceeth' := ifelse(RacEthGroupA %in% c('Asian-NH', 'NatHwn-PI'), 'Asian, NH and PI',  
                   ifelse(RacEthGroupA %in% c('Multiracial-NH', 'Other-NH', 'Unknown'),  'Other-Unknown-multi', 
                    RacEthGroupA))]

#-------------------------------------------------------------------------------
# Sum cases by year, county, age group, and race/ethnicity category 
#-------------------------------------------------------------------------------
CaseCounts<-dat[, list(Cases=.N), by=list(sviyear, GEOID, AgeRange, raceeth)]

#Drop unknown, multiracial and unknown race and missing age 
CaseCounts<-subset(CaseCounts, raceeth != 'Other-Unknown-multi' & !is.na(AgeRange))

#-------------------------------------------------------------------------------
# Import 2004-2019 County population data for foodnet surveillance area
#-------------------------------------------------------------------------------
folder<-'//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/FoodNet Population Data/'

pop<-setDT(read.csv(paste0(folder, 'nchs_pop_est_by_county_aian_topcode.csv'), 
                      stringsAsFactors = F, colClasses = c("GEOID"='character')))
#-------------------------------------------------------------------------------
# Merge population and case counts. Executing a left join to keep tracts with 0 cases. 
#-------------------------------------------------------------------------------
pop$sviyear<-as.factor(pop$sviyear)
fin<-merge(pop, CaseCounts, by = c('sviyear', 'GEOID', 'AgeRange', 'raceeth'), all.x=T)
fin$Cases[is.na(fin$Cases)]<-0

#Classify state
fin[,'State':=factor(str_sub(GEOID, 1, 2), 
                     levels=c('06','08','09','13','24','27','35','36','41','47'),
                     labels=c('CA', 'CO', 'CT', 'GA', 'MD', 'MN', 'NM', 'NY', 'OR', 'TN'))]

#-------------------------------------------------------------------------------
#Import NCHS Urban-Rural classification and merge to data by STCNTY
#-------------------------------------------------------------------------------
folder<-'//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/NCHS/'
nchs<-read.xlsx(paste0(folder, 'NCHSURCodes2013.xlsx'))
nchs$GEOID<-str_pad(nchs$FIPS.code, 5, "left", "0")
nchs$UrCode<-nchs$`2013.code`

fin<-merge(fin, nchs[,10:11], by='GEOID', all.x=T)
names(fin)[names(fin)=='GEOID']<-'County'

#-------------------------------------------------------------------------------
# Merge to 2000-2018 SVI scores for FoodNet Counties 
#-------------------------------------------------------------------------------
source('helper funcs/calc-region-specific-svi-score.R')

fin[, sviyear := as.numeric(as.character(sviyear))]
fin<-fin %>%
  left_join(svi_all[, c('Year', 'County', 'rpl', 'quartile')], 
            join_by(County==County, sviyear==Year))

########################## END DATA PREPARATION  #####################################

#-------------------------------------------------------------------------------
#Output final datasets
#-------------------------------------------------------------------------------
output.location<-c("//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/Final Datasets/Final_County_")

write.csv(fin, paste0(output.location, "ByRaceEth_", Sys.Date(), '.csv'), row.names=F)



