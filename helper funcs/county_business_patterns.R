library(dplyr)


readRenviron("~/.Renviron")
Sys.getenv("CENSUS_KEY")

source('helper funcs/getcensus_functions.R')

naics_list<-c("624410", "624221")
vars = c("GEO_ID", "EMP", "ESTAB")
states='state:06,08,09,13,24,27,35,41,47'

################################################################################
# Retrieve data from Census API
################################################################################

cbp00<-data.frame()
for (i in naics_list) {
  cbp00 <-rbind(cbp00, 
                getCensus(name = "cbp", vintage = '2000', vars = vars, 
                          region = "county:*", regionin = states, naics1997 = i))
}

cbp10<-data.frame()
for (i in naics_list) {
  cbp10 <-rbind(cbp10,
                getCensus(name = "cbp", vintage = '2010', vars = vars,
                          region = "county:*", regionin = states,  naics2007 = i))
}

cbp14<-data.frame()
for (i in naics_list) {
cbp14 <-rbind(cbp14,
              getCensus(name = "cbp",  vintage = '2014', vars = vars,
                        region = "county:*", regionin = states, naics2012  = i))
}

cbp16<-data.frame()
for (i in naics_list) {
cbp16 <-rbind(cbp16,
              getCensus(name = "cbp", vintage = '2016', vars = vars,
                        region = "county:*", regionin = states, naics2012 = i))
}

cbp18<-data.frame()
for (i in naics_list) {
  cbp18 <-rbind(cbp18,
      getCensus(name = "cbp", vintage = '2018', vars = vars,
                region = "county:*", regionin = states, naics2017 = i))
}

################################################################################
# Combine County Business Pattern Datasets
################################################################################
cbp00$cbp_year<-2000
cbp10$cbp_year<-2010
cbp14$cbp_year<-2014
cbp16$cbp_year<-2016
cbp18$cbp_year<-2018

datlist<-list(cbp00, cbp10, cbp14, cbp16, cbp18)

for (i in seq_along(datlist)) {
  datlist[[i]] <- datlist[[i]] %>%
    rename_at(vars(starts_with('NAICS')), ~gsub("[0-9]", "", .x)) %>%
    mutate(NAICS=as.character(NAICS))
}

cbp <- bind_rows(datlist)
rm(datlist, cbp00, cbp10, cbp14, cbp16, cbp18)

################################################################################
# Format county FIPS code and naics descriptor 
################################################################################
cbp <- cbp %>%
  mutate(County=paste0(state, county),
         NAICS = factor(NAICS, levels=c('624410', '624221'), labels=c('ChildCare', 'Shelters'))) %>%
  select(County, EMP, ESTAB, NAICS, cbp_year) %>%
  tidyr::pivot_wider(names_from=NAICS, values_from = c(EMP, ESTAB))

