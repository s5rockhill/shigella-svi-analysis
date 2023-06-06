library('dplyr')
library('stringr')
library('tidyr')
library('RODBC')
library('tidyverse')

##-------------------------------------------------------------------------------
# Create comprehensive list of census tracts to include in region-specific score 
# calculation usingthe U.S. Census Bureau Gazetteer File
#-------------------------------------------------------------------------------
states<-c('CT','GA', 'MD', 'MN', 'NM', 'OR', 'TN')
counties<-c(paste0('06', c('001', '013', '075')), 
            paste0('08', c('001', '005', '013', '014', '031', '035', '059')),
            paste0('36', c('001', '003', '009', '013', '015', '019', '021', '025', '029',
                           '031', '033', '035', '037', '039', '041', '051', '055', '057',
                           '063', '069', '073', '077', '083', '091', '093', '095', '097', 
                           '099', '101', '113', '115', '117', '121', '123')))

folder<-'//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/ACS Data/Supplemental Files/'
ctlist1<-read.delim(paste0(folder, '2019_Gaz_tracts_national.txt'))
ctlist2<-read.delim(paste0(folder, '2010_Gaz_tracts_national.txt'))

ctlist10<-union(ctlist1[,c('USPS', 'GEOID')], ctlist2[,c('USPS', 'GEOID')]) %>%
  mutate(GEOID = str_pad(GEOID, 11, side='left', pad='0')) %>%
  filter(USPS %in% states | substr(GEOID, 1, 5) %in% counties) 


rm(ctlist1, ctlist2)

ctlist00<-read_lines(paste0(folder, '2000_Gaz_tracts_national.txt'))
ctlist00<-read.fwf(textConnection(ctlist00), widths=c(2, 2, 3, 6, 9, 9, 14, 14, 14, 14, 14, 15))
names(ctlist00)<-c('USPS', 'ST', 'CO', 'CT', 'POP', 'HU', 'LArea', 'WArea', 'LAreaMiles', 'WAreaMiles', 'Lat', 'Long')

ctlist00 <- ctlist00 %>% 
  mutate(GEOID=paste0(str_pad(ST, 2, pad='0'), str_pad(CO, 3, pad='0'), str_pad(CT, 6, pad="0"))) %>%
  select(USPS, GEOID) %>%
  filter(USPS %in% states | substr(GEOID, 1, 5) %in% counties)

#-------------------------------------------------------------------------------
# Create ranking function that is analogous to SQL PERCENT_RANK().The default dplyr 
# percent_rank() function ranks NA values; however, this function excludes NAs. 
# The function also sets the ties method to "max" when ranking descending values. 
#-------------------------------------------------------------------------------

sql_rank <- function(x, direction='asc'){
  x[x==-999] <- NA
  if(direction=='desc') {
    output<-(
      ifelse(is.na(x), NA, 
             round(
               (rank(desc(round(x, 4)), ties.method = "max") - 1) / (sum(!is.na(x))-1), 4)))
  }
  
  if(direction=='asc'){
    output<-(
      ifelse(is.na(x), NA, 
             round(
               (rank(round(x, 4), ties.method = "min") - 1) / (sum(!is.na(x))-1), 4)))
  }
  
  return(output)
}

#-------------------------------------------------------------------------------
#Import national census tract-level SVI datasets
#-------------------------------------------------------------------------------

ch0 <- odbcDriverConnect("DSN=GRASP_MSSQL;UID=ppk8;DATABASE=sde_grasp_svi", rows_at_time = 1)
ch1 <- odbcDriverConnect("DSN=GRASP_MSSQL;UID=ppk8;DATABASE=sde_grasp_svi_2010", rows_at_time = 1)
ch2 <- odbcDriverConnect("DSN=GRASP_MSSQL;UID=ppk8;DATABASE=sde_grasp_svi_2014", rows_at_time = 1)
ch3 <- odbcDriverConnect("DSN=GRASP_MSSQL;UID=ppk8;DATABASE=sde_grasp_svi_2016", rows_at_time = 1)
ch4 <- odbcDriverConnect("DSN=GRASP_MSSQL;UID=ppk8;DATABASE=sde_grasp_svi_2018", rows_at_time = 1)

svi00 <- sqlQuery(ch0, "SELECT * FROM sdeadmin.US_NATIONAL_2000_SVI", as.is=TRUE)
svi10 <- sqlQuery(ch1, "SELECT * FROM sdeadmin.SVI2010_US", as.is=TRUE)
svi14 <- sqlQuery(ch2, "SELECT * FROM sdeadmin.SVICompiled_Tract_National", as.is=TRUE)
svi16 <- sqlQuery(ch3, "SELECT * FROM sdeadmin.SVI2016_US_tract", as.is=TRUE)
svi18 <- sqlQuery(ch4, "SELECT * FROM sde.SVI2018_US_tract", as.is=TRUE)

#on.exit( RODBC::odbcClose(ch0, ch1, ch2, ch3, ch4))

#-------------------------------------------------------------------------------
#Keep fields used to calculate percentile rankings
#-------------------------------------------------------------------------------
svi00<-svi00 %>%
  dplyr::select(FIPS, ends_with('R'), -STATE_ABBR)

svi10<-svi10 %>%
  dplyr::select(FIPS, starts_with('E_P_') | starts_with('P_'))

svi14<-svi14 %>%
  dplyr::select(FIPS, starts_with('EP_'), -EP_UNINSUR)

svi16<-svi16 %>%
  dplyr::select(FIPS, starts_with('EP_'), -EP_UNINSUR)

svi18<-svi18 %>%
  dplyr::select(FIPS, starts_with('EP_'), -EP_UNINSUR)


#-------------------------------------------------------------------------------
#Calculate total SVI score for select census tracts
#-------------------------------------------------------------------------------

svi00<-svi00 %>%
  filter(FIPS %in% ctlist00$GEOID) %>%
  mutate(across(G1V1R:G4V5R, as.numeric)) %>%
  mutate(sviyear=2000, 
         t1rpl=sql_rank(rowSums(across(G1V1R:G1V4R, ~ sql_rank(.x)))),
         t2rpl=sql_rank(rowSums(across(G2V1R:G2V4R, ~ sql_rank(.x)))),
         t3rpl=sql_rank(rowSums(across(G3V1R:G3V2R, ~ sql_rank(.x)))),
         t4rpl=sql_rank(rowSums(across(G4V1R:G4V4R, ~ sql_rank(.x)))),
         rpl=sql_rank(rowSums(across(G1V1R:G4V5R, ~ sql_rank(.x))))) %>%
  dplyr::select(sviyear, FIPS, t1rpl, t2rpl, t3rpl, t4rpl, rpl)
  
svi10<-svi10 %>%
  filter(FIPS %in% ctlist10$GEOID) %>%
  mutate(across(E_P_POV:P_GROUPQ, as.numeric)) %>%
  mutate(sviyear=2010, 
         t1rpl=sql_rank(rowSums(across(E_P_POV:E_P_NOHSDIP, ~ sql_rank(.x)))),
         t2rpl=sql_rank(rowSums(across(P_AGE65:P_SNGPRNT, ~ sql_rank(.x)))),
         t3rpl=sql_rank(rowSums(across(c(P_MINORITY, E_P_LIMENG), ~ sql_rank(.x)))),
         t4rpl=sql_rank(rowSums(across(c(E_P_MUNIT:E_P_NOVEH, P_GROUPQ), ~ sql_rank(.x)))),
         rpl=sql_rank(rowSums(across(E_P_POV:P_GROUPQ, ~ sql_rank(.x))))) %>%
  dplyr::select(sviyear, FIPS, t1rpl, t2rpl, t3rpl, t4rpl, rpl)

svi14<-svi14 %>%
  filter(FIPS %in% ctlist10$GEOID) %>%
  mutate(across(EP_POV:EP_GROUPQ, as.numeric)) %>%
  mutate(sviyear=2014, 
         t1rpl=sql_rank(rowSums(across(EP_POV:EP_NOHSDP, ~ sql_rank(.x)))),
         t2rpl=sql_rank(rowSums(across(EP_AGE65:EP_SNGPNT, ~ sql_rank(.x)))),
         t3rpl=sql_rank(rowSums(across(c(EP_MINRTY:EP_LIMENG), ~ sql_rank(.x)))),
         t4rpl=sql_rank(rowSums(across(c(EP_MUNIT:EP_GROUPQ), ~ sql_rank(.x)))),
         rpl=sql_rank(rowSums(across(EP_POV:EP_GROUPQ, ~ sql_rank(.x))))) %>%
  dplyr::select(sviyear, FIPS, t1rpl, t2rpl, t3rpl, t4rpl, rpl)

svi16<-svi16 %>%
  filter(FIPS %in% ctlist10$GEOID) %>%
  mutate(across(EP_POV:EP_GROUPQ, as.numeric)) %>%
  mutate(sviyear=2016, 
         t1rpl=sql_rank(rowSums(across(EP_POV:EP_NOHSDP, ~ sql_rank(.x)))),
         t2rpl=sql_rank(rowSums(across(EP_AGE65:EP_SNGPNT, ~ sql_rank(.x)))),
         t3rpl=sql_rank(rowSums(across(c(EP_MINRTY:EP_LIMENG), ~ sql_rank(.x)))),
         t4rpl=sql_rank(rowSums(across(c(EP_MUNIT:EP_GROUPQ), ~ sql_rank(.x)))),
         rpl=sql_rank(rowSums(across(EP_POV:EP_GROUPQ, ~ sql_rank(.x))))) %>%
  dplyr::select(sviyear, FIPS, t1rpl, t2rpl, t3rpl, t4rpl, rpl)

svi18<-svi18 %>%
  filter(FIPS %in% ctlist10$GEOID) %>%
  mutate(across(EP_POV:EP_GROUPQ, as.numeric)) %>%
  mutate(sviyear=2018, 
         t1rpl=sql_rank(rowSums(across(EP_POV:EP_NOHSDP, ~ sql_rank(.x)))),
         t2rpl=sql_rank(rowSums(across(EP_AGE65:EP_SNGPNT, ~ sql_rank(.x)))),
         t3rpl=sql_rank(rowSums(across(c(EP_MINRTY:EP_LIMENG), ~ sql_rank(.x)))),
         t4rpl=sql_rank(rowSums(across(c(EP_MUNIT:EP_GROUPQ), ~ sql_rank(.x)))),
         rpl=sql_rank(rowSums(across(EP_POV:EP_GROUPQ, ~ sql_rank(.x))))) %>%
  dplyr::select(sviyear, FIPS, t1rpl, t2rpl, t3rpl, t4rpl, rpl)

#-------------------------------------------------------------------------------
#Combine 2000-2018 svi datasets and calculate quartiles
#-------------------------------------------------------------------------------
svi_all<-rbind(svi00, svi10, svi14, svi16, svi18)
rm(svi00, svi10, svi14, svi16, svi18)

#Calculate quartile
breaks<-c(0, .25, .5, .75, 1)

svi_all<-svi_all %>%
  group_by(sviyear) %>%
  mutate(t1qrtile = cut(t1rpl, breaks=breaks, include.lowest=T, labels=F),
         t2qrtile = cut(t2rpl, breaks=breaks, include.lowest=T, labels=F),
         t3qrtile = cut(t3rpl, breaks=breaks, include.lowest=T, labels=F),
         t4qrtile = cut(t4rpl, breaks=breaks, include.lowest=T, labels=F),
         qrtile = cut(rpl, breaks=breaks, include.lowest=T, labels=F)) %>%
  ungroup()
