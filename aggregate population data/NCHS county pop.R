library('data.table')
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
# Merge datasets and limit to FoodNet Counties
#-------------------------------------------------------------------------------
pop<-merge(merge(p04, p09, by=cols[1:6]), p19, by=cols[2:6])

pop<-subset(pop, state %in% c('09','13', '24', '27', '35', '41', '47') |
              (state =='06' & county %in% c('001', '013', '075')) |
              (state =='08' & county %in% c('001', '005', '013', '014', '031', '035', '059')) |
              (state =='36' & county %in% c('001', '003', '009', '013', '015', '019', '021', '025', '029',
                                       '031', '033', '035', '037', '039', '041', '051', '055', '057',
                                       '063', '069', '073', '077', '083', '091', '093', '095', '097', 
                                       '099', '101', '113', '115', '117', '121', '123')))

#-------------------------------------------------------------------------------
# Collapse to long-form dataset
#-------------------------------------------------------------------------------
setDT(pop)
pop<-melt(pop, id.vars = c("state", "county", 'age', 'racesex', 'hispanic'),
          measure.vars = c(cols[7:26]), variable.name = 'year', value.name = 'Pop')

#-------------------------------------------------------------------------------
# Create new demographic fields
#-------------------------------------------------------------------------------
AgeGroupLabs<-c('0-4', '5-14', '15-44', '45+')
pop[, 'AgeRange' := cut(age, breaks=c(0, 5, 15, 45, 999), right=F, labels=AgeGroupLabs)]

svibreaks<-c(2000, 2006, 2011, 2015, 2017, 2020)
svilabels<-c('2000', '2010', '2014', '2016', '2018')
pop[,'sviyear' := cut(as.numeric(str_sub(year, -4)), breaks=svibreaks, right=F, labels=svilabels)]

pop[, 'raceeth' := ifelse(hispanic==1 & racesex < 3, 'White-NH', 
                    ifelse(hispanic==1 & racesex %in% 3:4, 'Black-NH',
                      ifelse(racesex %in% 5:6, 'AmInd-AKNat',
                        ifelse(hispanic==1 & racesex >6, 'Asian, NH and PI',
                               'Hispanic'))))]

pop[, 'GEOID' := paste0(state, county)]

#-------------------------------------------------------------------------------
# Sum over age, time period and race/ethncity
#-------------------------------------------------------------------------------
pop<-pop[, list(Pop=sum(Pop)), by=list(GEOID, sviyear, raceeth, AgeRange)]

#-------------------------------------------------------------------------------
#output dataset
#-------------------------------------------------------------------------------
setwd('//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/FoodNet Population Data/')
write.csv(pop, 'nchs_pop_est_by_county_aian_topcode.csv',row.names = F)