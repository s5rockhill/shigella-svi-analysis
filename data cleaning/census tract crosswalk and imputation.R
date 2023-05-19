#Import libraries
library('rvest')
library('stringr')
library('data.table')
library('tigris')

#---------------------------------------------------------------------------------
#Set any census tract GEOID's to missing if not in FoodNet state
#---------------------------------------------------------------------------------
foodnetstates<-c('06','08','09','13','24','27','35','36' ,'41','47')
dat[!(substr(CTNO2010, 1, 2) %in% foodnetstates), 'CTNO2010':=NA]
dat[!(substr(CTNO2000, 1, 2) %in% foodnetstates), 'CTNO2000':=NA]

#---------------------------------------------------------------------------------
#Import U.S. Census gazetteer files
#---------------------------------------------------------------------------------
folder<-'//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/'
ctlist1<-read.delim(paste0(folder, 'ACS Data/Supplemental Files/2019_Gaz_tracts_national.txt'))
ctlist2<-read.delim(paste0(folder, 'ACS Data/Supplemental Files/2010_Gaz_tracts_national.txt'))
ctlist3<-read_lines(paste0(folder, 'ACS Data/Supplemental Files/2000_Gaz_tracts_national.txt'))
ctlist3<-read.fwf(textConnection(ctlist3), widths=c(2, 2, 3, 6, 9, 9, 14, 14, 14, 14, 14, 15))
names(ctlist3)<-c('USPS', 'ST', 'CO', 'CT', 'POP', 'HU', 'LArea', 'WArea', 'LAreaMiles', 'WAreaMiles', 'Lat', 'Long')
ctlist3$GEOID<-paste0(str_pad(ctlist3$ST, 2, pad='0'), str_pad(ctlist3$CO, 3, pad='0'), str_pad(ctlist3$CT, 6, pad="0"))

#---------------------------------------------------------------------------------
#Combine gazetteer files
#---------------------------------------------------------------------------------
ctlist1$year<-2010
ctlist2$year<-2019
ctlist3$year<-2000

keep.fields<-c('USPS', 'GEOID', 'year')
ctlist<-rbind(ctlist1[, keep.fields], 
              ctlist2[!(ctlist2$GEOID %in% ctlist1$GEOID), keep.fields],
              ctlist3[, keep.fields])
rm(ctlist1, ctlist2, ctlist3)

ctlist$GEOID<-str_pad(ctlist$GEOID, 11, pad='0')
setDT(ctlist)

#---------------------------------------------------------------------------------
#Reassign GEOIDs to the correct year for census tracts that have a 2000 GEOID 
#in the CTNO2010 column (or visa versa)
#---------------------------------------------------------------------------------
dat[CTNO2010 %in% ctlist[year==2000,GEOID] & !CTNO2010 %in% ctlist[year>2000,GEOID] 
    & is.na(CTNO2000), 'CTNO2000':=CTNO2010]

dat[!(CTNO2010 %in% ctlist[year>2000,GEOID]) & CTNO2010==CTNO2000, 'CTNO2010':=NA]

#---------------------------------------------------------------------------------
#Extract county fips codes
#---------------------------------------------------------------------------------
dat[, 'CNTYFIPS':= ifelse(!is.na(CTNO2010), substr(CTNO2010, 1, 5), substr(CTNO2000, 1, 5))]

#---------------------------------------------------------------------------------
#Set invalid census tracts to missing (these are 2020 GEOID's)
#---------------------------------------------------------------------------------
dat[!CTNO2010 %in% ctlist[year>2000,GEOID] & !is.na(CTNO2010), CTNO2010 := NA]

#---------------------------------------------------------------------------------
#Check number of cases with missing census tract information
#---------------------------------------------------------------------------------
missct<-dat[, list(Set='Original', 
                   Missing_2000 = sum(is.na(CTNO2000)), 
                   Missing_2010=sum(is.na(CTNO2010))), by='Year']

#---------------------------------------------------------------------------------
#Import-relationship-file
#---------------------------------------------------------------------------------
u<-"https://www.census.gov/programs-surveys/geography/technical-documentation/records-layout/2010-census-tract-record-layout.html"

rf_header <- read_html(u) %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(fill = TRUE)

rf<-setDT(read.csv('https://www2.census.gov/geo/docs/maps-data/data/rel/trf_txt/us2010trf.txt',
                   col.names = rf_header[[1]]$`Column Name`,
                   colClasses = c(GEOID00='character', GEOID10='character')))

rf[, c('STATE00', 'COUNTY00', 'TRACT00', 'STATE10', 'COUNTY10', 'TRACT10') := NULL]
setnames(rf, c('GEOID00','GEOID10'),  c('CTNO2000', 'CTNO2010'))

#---------------------------------------------------------------------------------
#Select tracts with cases that are missing the corresponding tract value
#---------------------------------------------------------------------------------
t0<-dat[is.na(CTNO2010) & !is.na(CTNO2000) & Year > 2005, .(cases=.N), by=c('CTNO2000')]
t1<-dat[is.na(CTNO2000) & !is.na(CTNO2010) & Year < 2006, .(cases=.N), by=c('CTNO2010')]

#---------------------------------------------------------------------------------
#Classify tracts in relationship file by change type
#---------------------------------------------------------------------------------
change_vars<-c('AREALANDPCT00PT', 'AREALANDPCT10PT', 'POPPCT00', 'POPPCT10')

rf[,Type :=ifelse(PART00=='W' & PART10=='W', 'Unchanged',
            ifelse(rowSums(rf[, .SD, .SDcols=change_vars]>95)==4 & (PART00=='P'|PART10=='P'), 'Minimal',
             ifelse(AREALANDPCT00PT==100 & POPPCT00==100 & AREALANDPCT10PT<=95, 'Merged',
              ifelse(AREALANDPCT00PT<=95 & AREALANDPCT10PT==100 & POPPCT10==100, 'Split',
                'Major'))))]

#---------------------------------------------------------------------------------
#Combine and merge tracts by type of change to list of tracts with missing data
#---------------------------------------------------------------------------------
rf_vars<-c('CTNO2000', 'CTNO2010', 'Type')

t0<-merge(t0, rf[, ..rf_vars], by='CTNO2000', all.x=T)
t1<-merge(t1, rf[, ..rf_vars], by='CTNO2010', all.x=T)

tract_with_missing_by_rf<-rbind( t0[, list(Missing='2010 CT', Tracts=length(unique(CTNO2000)), Cases=sum(cases)), by='Type'],
       t1[, list(Missing='2000 CT', Tracts=length(unique(CTNO2010)), Cases=sum(cases)), by='Type'])

t0<-subset(t0, !Type %in% c('Split', 'Major'))
t1<-subset(t1, !Type %in% c('Merged', 'Major'))

#---------------------------------------------------------------------------------
#Update missing census tract values using crosswalk
#---------------------------------------------------------------------------------
dat[t0, c('CTNO2010') := ifelse(is.na(CTNO2010), i.CTNO2010, CTNO2010), on = "CTNO2000"]
dat[t1, c('CTNO2000') := ifelse(is.na(CTNO2000), i.CTNO2000, CTNO2000), on = "CTNO2010"]

#---------------------------------------------------------------------------------
#Split cases with missing tract information for imputation
#---------------------------------------------------------------------------------
dat[,MissCT := (is.na(CTNO2010) & !is.na(CTNO2000) & Year > 2005 | 
                is.na(CTNO2000) & !is.na(CTNO2010) & Year < 2006)]

dat[,ID := row.names(dat)]
dtlist<-split(dat, by='MissCT')

#---------------------------------------------------------------------------------
#Deterministic imputation: allocates all cases to the intersecting tract with 
#highest population weight
#---------------------------------------------------------------------------------
dtm<-dtlist[["TRUE"]]
dtm<-dtm[, c('ID', 'CTNO2000', 'CTNO2010', 'Year', 'State')]

#Find the census tract that contains the largest % of the known CT population
rf[, Largest10Part := (POPPCT00==max(POPPCT00)), by='CTNO2000']
rf[, Largest00Part := (POPPCT10==max(POPPCT10)), by='CTNO2010']

#Assign the 2010 CT that contains the largest percent of the known 2000 CT population
dtm[rf[Largest10Part==T,], c('Deterministic', 'POPPCT00') := list(
                        ifelse(is.na(CTNO2010), i.CTNO2010, CTNO2010), i.POPPCT00), 
          on = "CTNO2000"]

#Assign the 2000 CT that contains the largest percent of the known 2010 CT population
dtm[rf[Largest00Part==T,],  c('Deterministic', 'POPPCT10') := list(
                          ifelse(is.na(CTNO2000), i.CTNO2000, CTNO2000), i.POPPCT10), 
          on = "CTNO2010"]

#---------------------------------------------------------------------------------
#Stochastic imputation: allocates cases to intersecting tracts using population-
#weighted probability
#---------------------------------------------------------------------------------
stc<-dtlist[["TRUE"]]
stc<-stc[, c('ID', 'CTNO2000', 'CTNO2010', 'Year', 'State')]

#Generate random number between 0 and 1 for each case 
set.seed(1)
stc[, randoms := round(runif(nrow(stc), min = 0, max = 1), digits=3)]

#Calculate the cumulative % 2000 population contained in 2010 CT part
rf<-rf[order(CTNO2000, -POPPCT00), ]
rf[, CPropMax00 := round(cumsum(POP10PT/POP00), 3), by='CTNO2000']
rf[, CPropMin00 := c(0, CPropMax00[-.N]+0.001), by='CTNO2000']

#Average number of 2010 candidates per 2000 tract
rf[CTNO2000 %in% stc$CTNO2000, length(unique(CTNO2010)), by='CTNO2000'][, mean(V1)]

#Merge in all potential 2010 matches for known 2000 tracts
stc<-merge(stc, rf[,.(CTNO2000, Stochastic=CTNO2010, CPropMin00, CPropMax00, POPPCT00)], 
        by='CTNO2000', all.x=T)

#Calculate the cumulative % of 2010 Ct population contained in 2000 CT part
rf<-rf[order(CTNO2010, -POPPCT10),]
rf[, CPropMax10 := round(cumsum(POP10PT/POP10), 3), by='CTNO2010']
rf[, CPropMin10 := c(0, CPropMax10[-.N]+0.001), by='CTNO2010']

#Average number of 2000 candidates per 2010 tract
stc_avg_candidates<-rf[CTNO2010 %in% stc$CTNO2010, length(unique(CTNO2000)), by='CTNO2010'][, mean(V1)]

#Merge in all potential 2000 matches for known 2010 tracts
stc<-merge(stc, rf[,.(CTNO2010, Stochastic=CTNO2000, CPropMin10, CPropMax10, POPPCT10)],
           by='CTNO2010', all.x=T)

#Keep only records that contain the random generated number within the range of probabilities
stc<-subset(stc, randoms>=CPropMin00 & randoms<=CPropMax00 |randoms>=CPropMin10 & randoms<=CPropMax10)

#Drop extra columns
stc[, c('Stochastic') := .(fcoalesce(Stochastic.x, Stochastic.y))]
stc<-stc[, c('ID', 'Stochastic', 'POPPCT00', 'POPPCT10')]

#---------------------------------------------------------------------------------
#Combine Deterministic and Stochastic results for comparison
#---------------------------------------------------------------------------------
results<-merge(dtm, stc, by='ID')

#Average percent of the original population contained within the imputed boundary by method
temp1<-results[, list(Dtm_2000_Population_Mean=mean(POPPCT00.x, na.rm=T), 
                      Dtm_2010_Population_Mean=mean(POPPCT10.x, na.rm=T),
                      Stc_2000_Population_Mean=mean(POPPCT00.y, na.rm=T), 
                      Stc_2010_Population_Mean=mean(POPPCT10.y, na.rm=T)) ]

#Cases assigned to the same imputed tract in both methodologies by Year
temp2<-results[, list(Same_Result=sum(Deterministic==Stochastic, na.rm=T)/.N), by="Year"]

#Cases assigned to the same imputed tract in both methodologies by State
temp3<-results[, list(Same_Result=sum(Deterministic==Stochastic, na.rm=T)/.N), by='State']

##################################################################################
#Rerun analysis on cases with known CT values for both years to estimate accuracy
##################################################################################

#Select cases with both 2000 and 2010 census tracts
smp<-dtlist[["FALSE"]][!is.na(CTNO2010) & !is.na(CTNO2000), .(ID, State, CTNO2000, CTNO2010, Year)]

#Keep cases in tracts with major changes to boundaries
smp<-subset(smp, CTNO2000 %in% rf$CTNO2000[rf$Type %in% c('Major', 'Split')] |
                 CTNO2010 %in% rf$CTNO2010[rf$Type %in% c('Major', 'Merged')])

#Generate random number between 0 and 1
set.seed(2)
smp[, randoms := round(runif(nrow(smp), min = 0, max = 1), digits=3)]

#---------------------------------------------------------------------------------
#Impute 2010 values from 2000 tracts
#---------------------------------------------------------------------------------
#Deterministic
smp[rf[Largest10Part==T,], c('Deterministic_10'):=list(i.CTNO2010), on="CTNO2000"]

#Stochastic
smp<-merge(smp, rf[,.(CTNO2000, Stochastic_10=CTNO2010, CPropMin00, CPropMax00)], by='CTNO2000', all.x=T)
smp<-smp[randoms>=CPropMin00 & randoms<=CPropMax00, ]

#---------------------------------------------------------------------------------
#Impute 2000 values from 2010 tracts
#---------------------------------------------------------------------------------
#Deterministic
smp[rf[Largest00Part==T,], c('Deterministic_00'):=list(i.CTNO2000), on="CTNO2010"]

#Stochastic
smp<-merge(smp, rf[,.(CTNO2010, Stochastic_00=CTNO2000, CPropMin10, CPropMax10)], by='CTNO2010', all.x=T)
smp<-smp[randoms>=CPropMin10 & randoms<=CPropMax10, ]

#Percent of cases assigned to correct CT by method
#---------------------------------------------------------------------------------
temp4<-rbind(
smp[, list(Year="All", Cases=.N,
           Deterministic_2010=sum(Deterministic_10==CTNO2010)/.N, 
           Stochastic_2010=sum(Stochastic_10==CTNO2010)/.N, 
           Deterministic_2000=sum(Deterministic_00==CTNO2000)/.N, 
           Stochastic_2000=sum(Stochastic_00==CTNO2000)/.N)],

smp[, list(Cases=.N,
           Deterministic_2010=sum(Deterministic_10==CTNO2010)/.N, 
           Stochastic_2010=sum(Stochastic_10==CTNO2010)/.N, 
           Deterministic_2000=sum(Deterministic_00==CTNO2000)/.N, 
           Stochastic_2000=sum(Stochastic_00==CTNO2000)/.N), by='Year']
)
################################################################################
#Output results
result_list<-list("Percent of Pop in Imputed Tract"=temp1, 
                  "Percent Same Result by Year"=temp2,
                  "Percent Same Result by State"=temp3,
                  "Accuracy Estimation by Year"=temp4)

openxlsx::write.xlsx(result_list, file="//cdc.gov/project/ATS_GIS_Store12/Shigella_SVI/Preliminary Results/Impution Results.xlsx")

#Merge imputed tracts with original dataset and output 
dat$MissCT<-NULL
dat<-merge(dat, results[, c('ID', 'Deterministic', 'Stochastic')], by='ID', all.x=T)
################################################################################

# END
