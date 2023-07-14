library('dplyr')
library('stringr')
library('readr')

#-------------------------------------------------------------------------------
# Import NCHS Bridged Race Population Estimates for 2000-2019 
# Files: (1) Vintage 2019 2010-2019 postcensal estimates
#        (2) Vintage 2010 2000-2009 intercensal estimates
# See https://www.cdc.gov/nchs/nvss/bridged_race/data_documentation.htm#Vintage2019
#-------------------------------------------------------------------------------
setwd('//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/NCHS Bridged Race Population Estimates')
cols<-c("series", "state", "county", "age", 'racesex', 'hispanic', paste0('Pop', 2000:2019))

p04 <-read_fwf('icen_2000_09_y0004.txt', fwf_widths(c(8, 2, 3, 2, 1, 1, rep(8, 5)), cols[1:11]))
p09 <-read_fwf('icen_2000_09_y0509.txt', fwf_widths(c(8, 2, 3, 2, 1, 1, rep(8, 5)), cols[c(1:6, 12:16)]))
p19<-read_fwf('pcen_v2020_y1020.txt', fwf_widths(c(4, 2, 3, 2, 1, 1, rep(8, 10)), cols[c(1:6, 17:26)]))

#-------------------------------------------------------------------------------
# Merge datasets and limit to FoodNet Counties and collapse to long-form
#-------------------------------------------------------------------------------
pop <- p04 %>%
  left_join(p09, join_by(state, county, age, racesex, hispanic)) %>%
  left_join(p19, join_by(state, county, age, racesex, hispanic)) %>%
  select(-starts_with('series')) %>%
  filter(state %in% c('09','13', '24', '27', '35', '41', '47') |
           (state =='06' & county %in% c('001', '013', '075')) |
           (state =='08' & county %in% c('001', '005', '013', '014', '031', '035', '059')) |
           (state =='36' & county %in% c('001', '003', '009', '013', '015', '019', '021', '025', '029',
                                         '031', '033', '035', '037', '039', '041', '051', '055', '057',
                                         '063', '069', '073', '077', '083', '091', '093', '095', '097', 
                                         '099', '101', '113', '115', '117', '121', '123'))) %>%
  pivot_longer(cols=Pop2000:Pop2019, names_to='year', values_to='Pop') 

#-------------------------------------------------------------------------------
# Filter out pre-2004 population data
#-------------------------------------------------------------------------------
pop <- pop %>%
  mutate(year=as.numeric(gsub('Pop', "", year))) %>%
  filter(year>2003)
#-------------------------------------------------------------------------------
# Create new demographic fields
#-------------------------------------------------------------------------------
AgeLabs<-c('0-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44', 
           '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85+')
svibreaks<-c(2000, 2006, 2011, 2015, 2017, 2020)
svilabels<-c('2000', '2010', '2014', '2016', '2018')

pop <- pop %>%
  mutate(AgeRange = cut(age, breaks=seq(0, 90, by=5), right=F, labels=AgeLabs),
         sviyear = cut(year, breaks=svibreaks, right=F, labels=svilabels),
         raceeth = ifelse(hispanic==1 & racesex < 3, 'White-NH', 
                    ifelse(hispanic==1 & racesex %in% 3:4, 'Black-NH',
                      ifelse(racesex %in% 5:6, 'AmInd-AKNat',
                        ifelse(hispanic==1 & racesex >6, 'Asian, NH and PI', 'Hispanic')))),
         GEOID = paste0(state, county))
#-------------------------------------------------------------------------------
# Sum over age group, time period and race/ethncity
#-------------------------------------------------------------------------------
pop <- pop %>%
  group_by(GEOID, sviyear, raceeth, AgeRange) %>%
  summarise(Pop=sum(Pop))

#-------------------------------------------------------------------------------
# Merge with 2000 U.S. Standard Population Weights
#-------------------------------------------------------------------------------
stdpop<-read.fwf('https://seer.cancer.gov/stdpopulations/stdpop.18ages.txt',
                 widths=c(3, 3, 8), col.names = c('Standard', 'Age', 'Pop'))

stdpop <- stdpop %>% 
  filter(Standard==204) %>%
  mutate(AgeRange = factor(Age, levels=1:18, labels=AgeLabs),
         Weight = Pop/sum(Pop)) %>%
  select(AgeRange, Weight)
  
pop<-pop %>% left_join(stdpop, join_by(AgeRange))
#-------------------------------------------------------------------------------
#output dataset
#-------------------------------------------------------------------------------
output<-'//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/FoodNet Population Data/'
write.csv(pop, paste0(output,'std_ages_nchs_pop_by_county.csv'),row.names = F)
