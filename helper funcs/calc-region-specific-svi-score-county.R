library(dplyr)
library(stringr)
library(tidyr)

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
#Import national county-level SVI datasets
#-------------------------------------------------------------------------------

ch0 <- odbcDriverConnect("DSN=GRASP_MSSQL;UID=ppk8;DATABASE=sde_grasp_svi", rows_at_time = 1)
ch1 <- odbcDriverConnect("DSN=GRASP_MSSQL;UID=ppk8;DATABASE=sde_grasp_svi_2010", rows_at_time = 1)
ch2 <- odbcDriverConnect("DSN=GRASP_MSSQL;UID=ppk8;DATABASE=sde_grasp_svi_2014", rows_at_time = 1)
ch3 <- odbcDriverConnect("DSN=GRASP_MSSQL;UID=ppk8;DATABASE=sde_grasp_svi_2016", rows_at_time = 1)
ch4 <- odbcDriverConnect("DSN=GRASP_MSSQL;UID=ppk8;DATABASE=sde_grasp_svi_2018", rows_at_time = 1)

svi00 <- sqlQuery(ch0, "SELECT * FROM sdeadmin.SVI2000_US_county", as.is=TRUE)
svi10 <- sqlQuery(ch1, "SELECT * FROM sdeadmin.SVI2010_US_county", as.is=TRUE)
svi14 <- sqlQuery(ch2, "SELECT * FROM sdeadmin.SVICompiled_County_National", as.is=TRUE)
svi16 <- sqlQuery(ch3, "SELECT * FROM sdeadmin.SVI2016_US_county", as.is=TRUE)
svi18 <- sqlQuery(ch4, "SELECT * FROM sde.SVI2018_US_county", as.is=TRUE)

#on.exit( RODBC::odbcClose(ch0, ch1, ch2, ch3, ch4))

#-------------------------------------------------------------------------------
#Keep fields used to calculate percentile rankings
#-------------------------------------------------------------------------------
svi00<-svi00 %>%
  dplyr::select(STCOFIPS, ends_with('R'), OldSVI=USTP, -STATE_ABBR)

svi10<-svi10 %>%
  dplyr::select(FIPS, starts_with('E_P_') | starts_with('P_'), OldSVI=R_PL_THEMES)

svi14<-svi14 %>%
  dplyr::select(FIPS, starts_with('EP_'), -EP_UNINSUR, OldSVI=RPL_THEMES)

svi16<-svi16 %>%
  dplyr::select(FIPS, starts_with('EP_'), -EP_UNINSUR, OldSVI=RPL_THEMES)

svi18<-svi18 %>%
  dplyr::select(FIPS, starts_with('EP_'), -EP_UNINSUR, OldSVI=RPL_THEMES)

#-------------------------------------------------------------------------------
#Calculate total SVI score for select counties
#-------------------------------------------------------------------------------

svi00<-svi00 %>%
  filter(STCOFIPS %in% fin$County) %>%
  mutate(across(G1V1R:G4V5R, as.numeric)) %>%
  mutate(Year=2000, rpl=sql_rank(rowSums(across(G1V1R:G4V5R, ~ sql_rank(.x))))) %>%
  dplyr::select(Year, County=STCOFIPS, rpl, OldSVI)
  
svi10<-svi10 %>%
  filter(FIPS %in% fin$County) %>%
  mutate(across(E_P_POV:P_GROUPQ, as.numeric)) %>%
  mutate(Year=2010, rpl=sql_rank(rowSums(across(E_P_POV:P_GROUPQ, ~ sql_rank(.x))))) %>%
  dplyr::select(Year, County=FIPS, rpl, OldSVI)

svi14<-svi14 %>%
  filter(FIPS %in% fin$County) %>%
  mutate(across(EP_POV:EP_GROUPQ, as.numeric)) %>%
  mutate(Year=2014, rpl=sql_rank(rowSums(across(EP_POV:EP_GROUPQ, ~ sql_rank(.x))))) %>%
  dplyr::select(Year, County=FIPS, rpl, OldSVI)

svi16<-svi16 %>%
  filter(FIPS %in% fin$County) %>%
  mutate(across(EP_POV:EP_GROUPQ, as.numeric)) %>%
  mutate(Year=2016, rpl=sql_rank(rowSums(across(EP_POV:EP_GROUPQ, ~ sql_rank(.x))))) %>%
  dplyr::select(Year, County=FIPS, rpl, OldSVI)

svi18<-svi18 %>%
  filter(FIPS %in% fin$County) %>%
  mutate(across(EP_POV:EP_GROUPQ, as.numeric)) %>%
  mutate(Year=2018, rpl=sql_rank(rowSums(across(EP_POV:EP_GROUPQ, ~ sql_rank(.x))))) %>%
  dplyr::select(Year, County=FIPS, rpl, OldSVI)

#-------------------------------------------------------------------------------
#Combine 2000-2018 svi datasets and calculate quartiles
#-------------------------------------------------------------------------------
svi_all<-rbind(svi00, svi10, svi14, svi16, svi18)
rm(svi00, svi10, svi14, svi16, svi18)

#Calculate quartile
breaks<-c(0, .25, .5, .75, 1)

svi_all<-svi_all %>%
  group_by(Year) %>%
  mutate(quartile = cut(rpl, breaks=breaks, include.lowest=T, labels=F),
         OldQuartile = cut(as.numeric(OldSVI), breaks=breaks, include.lowest=T, labels=F)) %>%
  ungroup()
