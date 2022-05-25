library('data.table')
library('stringr')
library('tidyverse')

setwd("//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/FoodNet_NARMS")


#cases=read.csv("N_38568_Merged_No Dups_USING_v02.16.22_FIPScorrected.csv", header=T, 
#             colClasses=c("CTNO2010"="character","CTNO2000"="character"))

#final dataset
cases=read.csv("N_38570_finalset_4.4.2022.csv", header=T, 
               colClasses=c("CTNO2010"="character","CTNO2000"="character"))
                

#View(cases)
#38,570 rows in final

#just records will a tract since we need to link to SVI
summary(cases$CTNO2010)

cases2=cases[which(!is.na(cases$CTNO2000) | !is.na(cases$CTNO2010)),] #not showing up as NA
cases[(cases=="")]=NA
#now it's NA, run code above to make cases2
#View(cases2)

cases2=cases[which(!is.na(cases$CTNO2000) | !is.na(cases$CTNO2010)),] #not showing up as NA
summary(cases2$CTNO2010)

#--------------------------------------------------------------------
#32,691 records have ANY tract info
addmargins(table(is.na(cases2$CTNO2000), is.na(cases2$CTNO2010)))

####only 1274 have CTN2000 and ONLY 32200 have CTN2010
#### 783 have both 2000 and 2010 tract values

setDT(cases2)

#need to get tract info in the cases file into one column so i can merge
# I don't think combining 2010 and 2000 into a single variable is a good idea. 
#cases2$tract=ifelse(is.na(cases2$CTNO2010), cases2$CTNO2000, cases2$CTNO2010)

#ct value length by state
table(cases2$State, nchar(cases2$CTNO2000))
table(cases2$State, nchar(cases2$CTNO2010))
cases2[nchar(cases2$CTNO2010)==1, CTNO2010]
cases2[nchar(cases2$CTNO2010)==10, CTNO2010]

#There are 41 records with a 2010 cT value of "." 

#set . to missing
cases2$CTNO2010[cases2$CTNO2010=="."]<-NA

#THIS HAS BEEN CORRECTED IN RAW DATA
#adding the leading zero for the 1675 records
#cases2$CTNO2010<-str_pad(cases2$CTNO2010, width=11, side='left', pad='0')

#look for invalid ct values
#---------------------------------------------

#first, let's look at the fips state to make sure that all codes are in our target area
table(str_sub(cases2$CTNO2010, 1, 2))
#48 is Texas
cases2$CTNO2010[str_sub(cases2$CTNO2010, 1, 2)=='48']<-NA
table(str_sub(cases2$CTNO2000, 1, 2))

#It looks like the state variable doesn't correspond to resident address
table(str_sub(cases2$CTNO2010, 1, 2), cases2$State, useNA='ifany')

#create a state column based on CT codes instead
cases2$state_fips<-ifelse(!is.na(cases2$CTNO2010), str_sub(cases2$CTNO2010, 1, 2), str_sub(cases2$CTNO2000, 1, 2))
cases2$state_fips<-factor(cases2$state_fips, levels=c('06', '08', '09', '13', '24', '27', '35', '36', '41', '47', '48'),
                          labels=c('CA', 'CO', 'CT', 'GA', 'MD', 'MN', 'NM', 'NY', 'OR', 'TN', 'TX'))
table(cases2$state_fips, useNA='ifany')

#next, lets look to see if there are any census tracts that aren't in the gazetter files
#-------------------------------------------------------------------------------------------
setwd('//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/ACS Data/Supplemental Files')
ctlist1<-read.delim('2019_Gaz_tracts_national.txt')
ctlist2<-read.delim('2010_Gaz_tracts_national.txt')
ctlist3<-read_lines('2000_Gaz_tracts_national.txt')
ctlist3<-read.fwf(textConnection(ctlist3), widths=c(2, 2, 3, 6, 9, 9, 14, 14, 14, 14, 14, 15))
names(ctlist3)<-c('USPS', 'ST', 'CO', 'CT', 'POP', 'HU', 'LArea', 'WArea', 'LAreaMiles', 'WAreaMiles', 'Lat', 'Long')
ctlist3$GEOID<-paste0(str_pad(ctlist3$ST, 2, pad='0'), str_pad(ctlist3$CO, 3, pad='0'), str_pad(ctlist3$CT, 6, pad="0"))

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

addmargins(table(cases2$CTNO2010 %in% ctlist[year>2000,]$GEOID, is.na(cases2$CTNO2010)))
#there are 91 non-missing 2010 census tract values NOT in the 2010-2019 gazetter files

cases2[!(CTNO2010 %in% ctlist[year>2000,]$GEOID) & !is.na(CTNO2010), .N, by=c('state_fips')]

#Let's take a look at the census tracts that aren't in the gazetter files
cases2[!(CTNO2010 %in% ctlist[year>2000,]$GEOID), .N, by=c('CTNO2010', 'state_fips')]

#hmmm...okay these look like a mix of 2000 and 2020 census tract values, not 2010 values
#but their 2000 ct value is missing
cases2[!(CTNO2010 %in% ctlist[year>2000,]$GEOID) & !is.na(CTNO2010), c('CTNO2000', 'CTNO2010')]

temp<-na.omit(cases2[!(CTNO2010 %in% ctlist[year>2000,]$GEOID), c('state_fips', 'CTNO2010')])
table(temp$CTNO2010 %in% ctlist[year==2000,]$GEOID)

#There are 13 ct values that are not in the 2010 or 2010-2019 gazetter files. These are 2020 CT's.
#except for the tract in Colorado, which I cannot find a record of in the 2000, 2010, or 2020 census
temp[!(CTNO2010 %in% ctlist$GEOID), c('state_fips', 'CTNO2010')]


temp$True.Year<-ifelse(!(temp$CTNO2010 %in% ctlist$GEOID), 2020, 2000)
temp$True.Year[temp$state_fips=='CO']<-9999
temp<-temp[, list(Records=.N), by=c('True.Year', 'CTNO2010') ]

write.csv(temp, 'Correct Census Year for CTNO2010 Values.csv', row.names = F)

#let's swap out those values so that the missplaced 2000 CT values are in the CTNO2000 column (if the case had a missing CTNO200)
cases2$CTNO2000<-ifelse((is.na(cases2$CTNO2000) & 
                          !(cases2$CTNO2010 %in% ctlist$GEOID[ctlist$year >2000]) &
                            cases2$CTNO2010 %in% ctlist$GEOID[ctlist$year==2000]),                                 
                            cases2$CTNO2010, 
                            cases2$CTNO2000)

#If the case had a non-missing 2010 value that was NOT in the 2010-2019 population data, set it to NA
# These are the missplaced 2020 CT values
cases2$CTNO2010<-ifelse(!is.na(cases2$CTNO2010) & !(cases2$CTNO2010 %in% ctlist$GEOID[ctlist$year >2000]), 
                        NA,
                        cases2$CTNO2010)

#-------------------------------------------------------------------------------------------

#check
#View(cbind(cases2$tract, cases2$CTNO2010, cases2$CTNO2000))
#there's period in there. get rid
#table(cases2$tract==".") #should be 41
#cases2=cases2[which(cases2$tract!="."),]
#View(cases2)

#Drop records that are missing both CT2000 and CT2010 values
cases2<-subset(cases2, !is.na(cases2$CTNO2010) | !is.na(cases2$CTNO2000))
#I have 32,636 after dropping the texas case and swapping out the invalid ct codes
#32,650 records. good.


#Non-missing CT values by state and year
#--------------------------------------------------------------

t1<-cases2[, list(ct_records_2000=sum(!is.na(CTNO2000)),
                  ct_records_2010=sum(!is.na(CTNO2010))), by=c('State', 'Year')]

#records with valid 2010 ct values by state and year
dcast(t1, State~Year, fun.aggregate = sum, value.var = 'ct_records_2010')

#records with valid 2000 ct values by state and year
dcast(t1, State~Year, fun.aggregate = sum, value.var = 'ct_records_2000')
#--------------------------------------------------------------

#now do some summary stats by year to see what we have to work with.
#names(cases2)
#table(cases2$Year)



#make age ranges (already age group var but it's weird)
#summary(cases2$Age)
#cases2$AgeRange=ifelse(is.na(cases2$Age), "NA",
 #                      ifelse(cases2$Age<5, "0-4", 
  #                     ifelse(cases2$Age>=5& cases2$Age<18, "05-17",
   #                           ifelse(cases2$Age>=18 & cases2$Age<31, "18-30",
    #                                 ifelse(cases2$Age>=31 & cases2$Age<51, "30-50", "50+")))))

#agetest=as.data.frame(table(cases2$AgeRange, cases2$Age))

#View(agetest[which(agetest$Freq!=0),]) 

####frequencies####

#this code will get it in a table so you can easily copy/paste into excel once you transpose, etc.
#copy output into excel, text to columns, then sort by year, then transpose and then you can copy/paste into table shell
#once you create the tbl, run the rbind code to get the full table 
#tbl=table(cases2$Year, cases2$AgeRange)
#rbind(tbl, round((proportions(tbl, margin =1))*100, digits=2))

#tbl=table(cases2$Year, cases2$Sex)
#tbl=table(cases2$Year, cases2$Ethnicity)
#tbl=table(cases2$Year, cases2$Race)
#tbl=table(cases2$Year, cases2$State)

#need to group serotypes
#table(cases2$serotypesummary)
#table(cases2$SpeciesName)

#cases2$species=ifelse(cases2$serotypesummary=="SONNEI", "sonnei",
#                      ifelse(cases2$serotypesummary=="UNKNOWN"| cases2$serotypesummary==
#                               "NOT SPECIATED", "unknown",
#                             ifelse(grepl("FLEXNERI", cases2$serotypesummary, fixed=T), "flexneri",
#                                    ifelse(grepl("BOYDII", cases2$serotypesummary, fixed=T), "boydii",
#                                           "dysenteriae")))) 
#testing it worked. yay good
#table(cases2$species, cases2$serotypesummary)
#tbl=table(cases2$Year, cases2$species)
#rbind(tbl, round((proportions(tbl, margin =1))*100, digits=2))

#table(cases2$subpop1)

#severity
#non-severe = no Fever, no BloodyDiarr, no Hospital, no bact, 

####Incidence####

#get pop data in here. merge with cases2
setwd("//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/ACS Data/Population Estimates")

pop=read.csv("Population_data_combined.csv", header=T, colClasses=c("GEOID"="character"))

head(pop)
table(pop$year)

#population data is 2000, then 2010 on.
#case data is every year 2004-2019 so 2004, 2005 will be 2000. 
#since 2010 ACS is 2006-2010,  those dates can be 2010

#Make sure that all FoodNet CT values are in the population data (i.e. make sure that there are NO ct values that don't have a match in the pop data)
table(is.na(cases2$CTNO2000),  cases2$CTNO2000 %in% pop$GEOID[pop$year==2000])
table(is.na(cases2$CTNO2010),  cases2$CTNO2010 %in% pop$GEOID[pop$year> 2000])
#Good - looks like all the CTNO2000 values are in the 2000 pop data and all the CTNO2010 values are in the 2010+ pop data

#Merge 2004 and 2005 cases to 2000 census tract data IF they have a non-missing CT2000 value
#-------------------------------------------------------------------------------------------------------
table(cases2$Year<2006, !is.na(cases2$CTNO2000)) #There should only be 492 cases merged to 2000 population data

cases0405=merge(subset(cases2, Year<2006 & !is.na(CTNO2000)), subset(pop, year==2000), by.x="CTNO2000", by.y="GEOID", all.x=T)
#View(cases0405)
table(is.na(cases0405$totalpop))
#(999 missing?) - Now none are missing
#View(cases0405[is.na(cases0405$totalpop),])

#Look at the number of cases, unique number of census tracts represented and total population by case year and census data year
cases0405[, list(Cases=.N, Census_Tracts=uniqueN(CTNO2000), Population=sum(totalpop)), by=c('Year', 'year')]

#Merge 2004 and 2005 cases to 2010 census tract data IF they have MISSING CT2000 value AND and non-missing CT2010 value
#-------------------------------------------------------------------------------------------------------
table(cases2$Year<2006, is.na(cases2$CTNO2000) & !is.na(cases2$CTNO2010)) #There should only be 2607 cases merged to 2010 population data

#Since we already dropped cases that were missing BOTH their 2000 and 2010 CT values, we don't have to specify non-missing CT 2010 cases
cases0405_missingct00=merge(subset(cases2, Year<2006 & is.na(CTNO2000)), subset(pop, year==2010), by.x="CTNO2010", by.y="GEOID", all.x=T)
table(is.na(cases0405_missingct00$totalpop))

#Look at the number of cases, unique number of census tracts represented and total population by case year and census data year
cases0405_missingct00[, list(Cases=.N, Census_Tracts=uniqueN(CTNO2010), Population=sum(totalpop)), by=c('Year', 'year')]

#now 2006-2009 is using 2010 data
#Merge 2006-2009 cases to 2010 census tract data IF they have a non-missing CT2010 value
#-------------------------------------------------------------------------------------------------------
table(cases2$Year %in% 2006:2009 , !is.na(cases2$CTNO2010)) #There should only be 7036 cases merged to 2010 population data

cases0609=merge(subset(cases2, Year>2005 & Year<2010 & !is.na(cases2$CTNO2010)), subset(pop, year==2010), by.x="CTNO2010", by.y="GEOID", all.x=T)
#View(cases0609)
table(is.na(cases0609$totalpop))
#[79 missing, 7035 there.] - Now there are 7036 cases. The original 79 missing were cases with missing 2010 CT values +1 case that we fixed earlier

#Look at the number of cases, unique number of census tracts represented and total population by case year and census data year
cases0609[, list(Cases=.N, Census_Tracts=uniqueN(CTNO2010), Population=sum(totalpop)), by=c('Year', 'year')]

#Merge 2006-2009 cases to 2000 census tract data IF they have a MISSING CT2010 value <<< ??? Don't know if you want to do this ????
#----------------------------------------------------------------------------------------------------------------------------------
table(cases2$Year %in% 2006:2009 , is.na(cases2$CTNO2010)) #There should only be 78 cases merged to 2000 population data

cases0609_missingct10=merge(subset(cases2, Year>2005 & Year<2010 & is.na(cases2$CTNO2010)), subset(pop, year==2000), by.x="CTNO2000", by.y="GEOID", all.x=T)
#View(cases0609)
table(is.na(cases0609_missingct10$totalpop)) # There are 78 cases with 2006-2009 dx year that had a missing CT2010 value, so were merged to 2000 data

#Look at the number of cases, unique number of census tracts represented and total population by case year and census data year
cases0609_missingct10[, list(Cases=.N, Census_Tracts=uniqueN(CTNO2000), Population=sum(totalpop)), by=c('Year', 'year')]


#now 2010-2019
#Merge 2010-2019 cases to 2010-2019 census tract data IF they have a non-missing CT2010 value
#----------------------------------------------------------------------------------------------------------------------------------
table(cases2$Year %in% 2010:2019 , !is.na(cases2$CTNO2010)) #There should only be 21932 cases merged to 2010 population data

cases1019=merge(x=subset(cases2, Year>2009 & !is.na(CTNO2010)), y=pop, by.x=c("CTNO2010", "Year"), by.y=c("GEOID","year"), all.x=T)

#View(cases1019)
table(is.na(cases1019$totalpop)) #[1851 missing] Now none are missing
#View(cases1019[is.na(cases1019$totalpop),])

cases1019$year<-cases1019$Year #putting census population year back in dataset (merging dropped it out because it was treated as key)

#Look at the number of cases, unique number of census tracts represented and total population by case year and census data year
cases1019[order(Year), list(Cases=.N, Census_Tracts=uniqueN(CTNO2010), Population=sum(totalpop)), by=c('Year')]


#Merge 2010-2019 cases to 2000 census tract data IF they have a MISSING CT2010 value <<< ??? Don't know if you want to do this ????
#----------------------------------------------------------------------------------------------------------------------------------
table(cases2$Year %in% 2010:2019 , is.na(cases2$CTNO2010)) #There should only be 491 cases merged to 2000 population data

cases1019_missingct10=merge(x=subset(cases2, Year>2009 & is.na(CTNO2010)), y=subset(pop, year==2000), by.x=c("CTNO2000"), by.y=c("GEOID"), all.x=T)

table(is.na(cases1019_missingct10$totalpop)) # none are missing

#Look at the number of cases, unique number of census tracts represented and total population by case year and census data year
cases1019_missingct10[order(Year), list(Cases=.N, Census_Tracts=uniqueN(CTNO2000), Population=sum(totalpop)), by=c('Year', 'year')]


#Verify that the final dataset contains the right cases 
#----------------------------------------------------------------------------------------------------------------------------------

#How many records are in orignal dataset
nrow(cases2)

#Number of unique people
length(unique(cases2$PersonID))

#Number of unique tracts
cases2[, list(CT2000=uniqueN(CTNO2000), CT2010=uniqueN(CTNO2010))]

#length(unique(cases2$tract)) #[Original: 8539 that should be fine.there's 10k some in the acs data] 
#Update: there are 7916 unique 2010 CT's and 359 unique 2000 CT's (combined 8275)


#to put them all together, need to move year up in df

#names(cases0405)
#names(cases0609)
#cases0409=rbind(cases0405,cases0609)
#names(cases1019)
#cases0409.2=cases0409[,c(1,44,2:43, 45:291,293:326)]
fullcase.pop=rbind(cases0405, cases0405_missingct00, cases0609, cases0609_missingct10, cases1019, cases1019_missingct10)
#Final dataset should have 32636 rows and 326 columns
dim(fullcase.pop) #Hooray!

#Number of people in dataset
length(unique(fullcase.pop$PersonID)) #Good

#Number of unique tracts in full dataset
fullcase.pop[, list(CT2000=uniqueN(CTNO2000), CT2010=uniqueN(CTNO2010))] #Good

#Any duplicated records?
sum(duplicated(fullcase.pop)) #NOPE

#Are any records missing population data?
sum(is.na(fullcase.pop$totalpop)) #NOPE

#View number of cases, number of tracts, and total population by FoodNet Year and Population data year
fullcase.pop[order(Year), list(Cases=.N, Census_Tracts=uniqueN(CTNO2000), Population=sum(totalpop)), by=c('Year', 'year')]


################################################################################
#Okay, this is where I stopped. 
################################################################################


#[ORIGINAL: 2929 missing total]
#nopop=fullcase.pop[is.na(fullcase.pop$totalpop),]
#View(nopop)

#tab=as.data.frame(table(nopop$Year, nopop$County, nopop$State))

#tab2=tab[which(tab$Freq>0),]
#View(tab2)
#write.csv(tab2, "Missingpop.csv", row.names=F)

####old code####
rpop=read.csv("2010_2019_ACS_5yr_B2001_Race.csv", header=T, colClasses=c("GEOID"="character"))
View(rpop)
str(rpop)


#incidence for race by tract by year
race.tract.year=as.data.frame(table(cases2$Race, cases2$tract, cases2$Year), stringsAsFactors = F)
View(race.tract.year)

colnames(race.tract.year)<-c("Race", "Tract", "Year", "Cases")
table(race.tract.year$Year)
race.tract.year$Year=as.integer(race.tract.year$Year)
str(race.tract.year)
str(rpop)

#merge in pop data for race. this will be for 2010-2019, but since we only have case data until 2015, 
#race.case df will have 2010-2015
race.case=merge(race.tract.year, rpop, by.x=c("Year","Tract"), by.y=c("year", "GEOID"), all=F)
#rpop only has 2010-2019
View(race.case)
table(race.case$Year)

#thinking of most efficient way to get median cases/pop (across tracts) for each race
#pull out one race at a time
library(tidyverse)
library(dplyr)
names(race.case)

filter(race.case, Race=="B",
       est.Black_Alone!=0)%>%
 mutate(rate=Cases/est.Black_Alone * 100000)%>%
  group_by(Year)%>%
  summarize(med=median(rate), 
            Q1=quantile(rate, probs=0.25),
            Q3=quantile(rate, probs=0.75),
            max=max(rate))
 
filter(race.case, Race=="W",
       est.White_Alone!=0)%>%
  mutate(rate=Cases/est.White_Alone * 100000)%>%
  group_by(Year)%>%
  summarize(med=median(rate), 
            Q1=quantile(rate, probs=0.25),
            Q3=quantile(rate, probs=0.75),
            max=max(rate))
#take out year
filter(race.case, Race=="B",
       est.Black_Alone!=0)%>%
  mutate(rate=Cases/est.Black_Alone * 100000)%>%
  summarize(med=median(rate), 
            Q1=quantile(rate, probs=0.25),
            Q3=quantile(rate, probs=0.75),
            max=max(rate),
            n=length(rate))


brate=filter(race.case, Race=="B",
       est.Black_Alone!=0)%>%
  mutate(rate=Cases/est.Black_Alone * 100000)

proportions(table(brate$rate==0))

filter(race.case, Race=="W",
       est.White_Alone!=0)%>%
  mutate(rate=Cases/est.White_Alone * 100000)%>%
  summarize(med=median(rate), 
            Q1=quantile(rate, probs=0.25),
            Q3=quantile(rate, probs=0.75),
            max=max(rate))



