library('data.table')
library('stringr')
library('RODBC')
library('openxlsx')
library('ggplot2')
library('ggthemes')
library('kableExtra')
library('dplyr')

col_palette<-c('black', '#D55E00', '#0072B4', '#009E73', '#E69F00', '#56B4E9')
#--------------------------------------------------------------------------------
#Import county-level dataset 
#--------------------------------------------------------------------------------
setwd('//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/FoodNet_NARMS/')
dat<-setDT(read.csv("analytic_file_V7.csv", stringsAsFactors = F,
        colClasses = c('CTNO2000'='character', 'CTNO2010'='character', 'Deterministic'='character')))

dat<-dat[, c('ID', 'CTNO2000', 'CTNO2010', 'GenusName', 'SpeciesClean', 'SubPopNew', 'HospClean', 
             'Bacteremia','Death', 'Severe', 'Deterministic', 'Stochastic', 'Year', 'State', 'Sex', 
             'Race', 'Ethnicity', 'Fever', 'BloodyDiarr', 'AgeClean', 'AgeRangeNew')]

#--------------------------------------------------------------------------------
#Data cleaning
#--------------------------------------------------------------------------------
#Select fips code to join svi data to based on diagnosis year
dat[, FIPS := ifelse(Year<2006, CTNO2000, CTNO2010)]
dat[is.na(FIPS), FIPS := Deterministic]

#Classify cases by which year of svi we want to join to 
svibreaks<-c(2000, 2006, 2011, 2015, 2017, 2020)
svilabels<-c('2000', '2010', '2014', '2016', '2018')
dat[,'sviyear' := as.numeric(as.character(cut(Year, breaks=svibreaks, right=F, labels=svilabels)))]

#Classify race/ ethnicity and Sex
dat[, c('LabelRaceEth', 'Sex') := list(
                    factor(ifelse(Ethnicity %in% c('N', 'U') & Race == 'W', 1, 
                     ifelse(Ethnicity %in% c('N', 'U') & Race == 'B', 2, 
                       ifelse(Ethnicity %in% c('N', 'U') & Race == 'I', 3,
                         ifelse(Ethnicity %in% c('N', 'U') & Race %in% c('A', 'P'), 4,
                           ifelse(Ethnicity=='H', 5, 6))))),
                           levels=1:6, 
                           labels=c('White, NH', 'Black, NH', 'American Indian\n or AK Native, NH', 
                                    'Asian or\n Pacific Islander, NH', 'Hispanic', '
                                     Other, multiracial,\n unknown')),
                    factor(Sex, levels=c('F', 'M'), labels=c('Female', 'Male')))]

#-------------------------------------------------------------------------------
# Import SVI datasets, calculate total SVI quartile, and left-join to cases
#-------------------------------------------------------------------------------
ch0 <- odbcDriverConnect("DSN=GRASP_MSSQL;UID=ppk8;DATABASE=sde_grasp_svi", rows_at_time = 1)
ch1 <- odbcDriverConnect("DSN=GRASP_MSSQL;UID=ppk8;DATABASE=sde_grasp_svi_2010", rows_at_time = 1)
ch2 <- odbcDriverConnect("DSN=GRASP_MSSQL;UID=ppk8;DATABASE=sde_grasp_svi_2014", rows_at_time = 1)
ch3 <- odbcDriverConnect("DSN=GRASP_MSSQL;UID=ppk8;DATABASE=sde_grasp_svi_2016", rows_at_time = 1)
ch4 <- odbcDriverConnect("DSN=GRASP_MSSQL;UID=ppk8;DATABASE=sde_grasp_svi_2018", rows_at_time = 1)

svi00 <- sqlQuery(ch0, "SELECT STCOFIPS AS STCNTY, TRACT AS FIPS, USTP AS RPL_THEMES, sviyear=2000 FROM sdeadmin.US_NATIONAL_2000_SVI", as.is=TRUE)
svi10 <- sqlQuery(ch1, "SELECT STCOFIPS AS STCNTY, TRACT AS FIPS, R_PL_THEMES AS RPL_THEMES, sviyear=2010  FROM sdeadmin.SVI2010_US", as.is=TRUE)
svi14 <- sqlQuery(ch2, "SELECT STCNTY, FIPS, RPL_THEMES, sviyear=2014 FROM sdeadmin.SVICompiled_Tract_National", as.is=TRUE)
svi16 <- sqlQuery(ch3, "SELECT STCNTY, FIPS, RPL_THEMES, sviyear=2016 FROM sdeadmin.SVI2016_US_tract", as.is=TRUE)
svi18 <- sqlQuery(ch4, "SELECT STCNTY, FIPS, RPL_THEMES, sviyear=2018 FROM sde.SVI2018_US_tract", as.is=TRUE)

on.exit(RODBC::odbcClose(ch0, ch1, ch2, ch3, ch4))

#Combine 2000-2018 svi datasets
svi_all<-setDT(rbind(svi00, svi10, svi14, svi16, svi18))
rm(svi00, svi10, svi14, svi16, svi18)

#Format fips for years <2014
svi_all[, "FIPS" := ifelse(sviyear<=2010, paste0(STCNTY, FIPS), FIPS)]

#Calculate quartile
svi_all$RPL_THEMES<-as.numeric(svi_all$RPL_THEMES)
svi_all[svi_all==-999]<-NA
svi_all[, quartile := cut(RPL_THEMES, breaks=c(0, .25, .5, .75, 1), include.lowest=T, labels=F), by='sviyear']

dat<-merge(dat, svi_all, by=c('FIPS', 'sviyear'), all.x=T)

#-------------------------------------------------------------------------------
#Import NCHS Urban-Rural classification and merge to data by STCNTY
#-------------------------------------------------------------------------------
folder<-'//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/NCHS/'
nchs<-read.xlsx(paste0(folder, 'NCHSURCodes2013.xlsx'))
nchs$County<-str_pad(nchs$FIPS.code, 5, "left", "0")
nchs$UrCode<-nchs$`2013.code`

dat[, County := str_sub(FIPS, 1, 5)]
dat<-merge(dat, nchs[, c('County', 'UrCode')], by='County', all.x=T)
dat[, 'UrCode' := factor(UrCode, levels=c(1:6), 
                  labels=c(rep('Large metro', 2), 'Medium metro', 'Small metro', rep('Rural',2)))]

#-------------------------------------------------------------------------------
# Demographic characteristics of severe and non-severe cases
#-------------------------------------------------------------------------------
setwd('//cdc.gov/project/ATS_GIS_Store12/Shigella_SVI/Preliminary Results/Charts')

#Group years into periods
dat[, Period := cut(Year, breaks=seq(2004, 2020, by=4), right=F, 
                    labels=c('2004-2007', '2008-2011', '2012-2015', '2016-2019'))]

dem.vars<-c('LabelRaceEth', 'Sex', 'Period', 'UrCode')
d<-data.frame()
cs<-data.frame()
for (i in dem.vars){
  cs<-rbind(cs, cbind(Group=i, p=with(dat, chisq.test(table(get(i), Severe))$p.value)))
  d<-rbind(d, cbind(Group=i, with(dat, table(get(i), Severe))))
}

d$Values<-row.names(d)
row.names(d)<-NULL
d[, c('TRUE', 'FALSE')]<-lapply(d[, c('TRUE', 'FALSE')], as.numeric)
d$Severe<-paste0(d$`TRUE`, " (", sprintf("%0.1f", d$`TRUE`/(d$`TRUE`+d$`FALSE`)*100), ")")
d$`Non-Severe`<-paste0(d$`FALSE`, " (", sprintf("%0.1f", d$`FALSE`/(d$`TRUE`+d$`FALSE`)*100), ")")

d<-setDT(d[, !names(d) %in% c('TRUE', 'FALSE')])

cs<-d[cs, mult="first", on="Group", nomatch=0L]
cs[, 'p' := cut(as.numeric(p), breaks=c(0, 0.001, 0.01, 0.05, 1), right=F,
                       labels=c("<0.001", "<0.01", '<0.05', ' '))]
d$RowNum<-seq.int(nrow(d))
d<-merge(d, cs, by=c('Group', 'Values', 'Severe', 'Non-Severe'), all.x=T)
d<-d[order(RowNum), ]
d$p[is.na(d$p)]<-" "

d<-d[, c('Values', 'Severe', 'Non-Severe', 'p')]
names(d)<-c(' ', 'N (Percent)', 'N (Percent)', 'p')

kable(d) %>%
  kable_classic(full_width=F, font_size = 12) %>%
  add_header_above(c(" "=1, 'Severe'=1, 'Non-Severe'=1, ' '=1), bold = T) %>%
  pack_rows('Race and Ethnicity', 1, 6) %>%
  pack_rows('Sex', 7, 8) %>%
  pack_rows('Year of Diagnosis', 9, 12) %>%
  pack_rows('Urban-Rural Designation', 13, 16) %>%
  save_kable('severe_demographics_table3_4.png')
  

cont.vars<-c('AgeClean', 'RPL_THEMES')d
c<-data.frame()
for (i in cont.vars){
  tt<-with(dat, t.test(get(i) ~ Severe))
  sd<-with(dat, aggregate(get(i), by=list(Severe), FUN=sd, na.rm = TRUE))
  c<-rbind(c, cbind(Values=i, 
                    NonSevere=tt$estimate[1], SD_NS=sd$x[sd$Group.1==F], 
                    Severe=tt$estimate[2], SD_SR=sd$x[sd$Group.1==T], 
                    p=tt$p.value))
}

row.names(c)<-NULL
c[, 2:6]<-lapply(c[, 2:6], as.numeric)
c[, 2:5]<-lapply(c[, 2:5], round, 2)
c$p<-ifelse(c$p<0.05, as.character(cut(c$p, breaks=c(0, 0.001, 0.01, 0.05), right=F,
         labels=c("<0.001", "<0.01", '<0.05'))), 
         as.character(round(c$p, 4)))
c$Severe<-paste0(c$Severe, " (", c$SD_SR, ")")
c$`Non-Severe`<-paste0(c$NonSevere, " (", c$SD_NS, ")")
c$Values[c$Values=='AgeClean']<-'Age (Yrs)'
c$Values[c$Values=='RPL_THEMES']<-'SVI Score'

c<-c[, c('Values', 'Severe', 'Non-Severe', 'p')]
names(c)<-c(' ', 'Mean (SD)', 'Mean (SD)', 'p')

kable(c) %>%
  kable_classic(full_width=F, font_size = 12) %>%
  add_header_above(c(" "=1, 'Severe'=1, 'Non-Severe'=1, ' '=1), bold = T) %>%
  save_kable('severe_demographics_continuous.png')
  

kable(c(dkbl, ckbl), "html") %>% cat(., file = "table3.4.html")
#-------------------------------------------------------------------------------
# Proportion of cases having severe shigella by race/ethnicity and SVI Quartile
#-------------------------------------------------------------------------------


t1<-dat[!is.na(quartile), list(Total=.N, Severe=sum(Severe==T), Percent=sum(Severe==T)/.N), 
                                by=list(LabelRaceEth, quartile)]

quartile.labs <- c('1: Least Vulnerable', '2', '3', '4: Most Vulnerable')
names(quartile.labs) <- c("1", "2", "3", "4")

#Figure 3.4A
ggplot(t1, aes(x=LabelRaceEth, y=Percent))+
  geom_bar(stat='identity', fill='#0072B2') + 
  facet_grid(rows=vars(quartile), labeller=labeller(quartile=quartile.labs))+
  geom_text(aes(label=scales::percent(round(Percent,2)), y=Percent), stat="identity", vjust=-.5) +
  scale_y_continuous(limits=c(0,1), labels = scales::percent) +
  ylab("Percent of Cases") +
  labs(title='Percent of Shigella Cases Classified as Severe by Race, Ethnicity, and SVI Quartile')+
  theme_light()+
  theme(axis.title.x = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background =element_rect(fill='gray25'))
ggsave('Percent_Severe_by_Race_figure3_4A.png', width = 8, height = 7, units='in')

#Table 3.4A
t1<-dcast(t1[, 'Percent' := list(Percent=sprintf("%0.1f", Percent*100))], 
          LabelRaceEth ~ quartile, value.var = c('Total', 'Severe', 'Percent'))

t1<-t1[, c('LabelRaceEth', paste0(rep(c('Total_', 'Severe_', 'Percent_'), 4), rep(1:4, each=3)))]
colnames(t1)<-c("", rep(c('Total (N)', 'Severe Cases (N)', '%'), times=4))

kbl(t1, align =c("l",  rep('r', 12))) %>%
  kable_classic(full_width=F, font_size = 12) %>%
  add_header_above(c(" "=1, "Quartile 1" = 3, "Quartile 2" = 3, "Quartile 3" = 3, "Quartile 4"=3), bold=T) %>%
  #as_image(width = 8, height = 4)
  save_kable('percent_severe_table3_4A.png')

#-------------------------------------------------------------------------------
# ( Univariate models of odds of having severe shigella 
#-------------------------------------------------------------------------------
um0<-glm(Severe ~ 1, data=dat, family = 'binomial')
um1<-glm(Severe ~ Sex, data=dat, family = 'binomial')
um2<-glm(Severe ~ LabelRaceEth, data=dat, family = 'binomial')
um3<-glm(Severe ~ AgeClean, data=dat, family = 'binomial')
um4<-glm(Severe ~ Year, data=dat, family = 'binomial')
um5<-glm(Severe ~ RPL_THEMES, data=dat, family = 'binomial')
#um6<-glm(Severe ~ State, data=dat, family = 'binomial')

model.list<-list(um1, um2, um3, um4, um5)
names(model.list)<-c('Sex', 'Race/Ethnicity', 'Age', 'Year', 'SVI')

uni.estimates<-setDT(data.frame())

for (i in 1:length(model.list)){
  temp<-cbind(
  data.frame(Parameters=names(model.list[[i]]$coefficients)),
  data.frame(OR=exp(coef(model.list[[i]]))), 
  data.frame(CI=exp(confint(model.list[[i]], method='Wald'))),
  data.frame(p=round(summary(model.list[[i]])$coefficients[, 4], 4)))
  uni.estimates<-rbind(uni.estimates, temp)
}

uni.estimates$Model<-'Univariate'

#-------------------------------------------------------------------------------
# Model odds of having severe shigella (Multivariate)
#-------------------------------------------------------------------------------
nmdat<-na.omit(dat[, .(Severe, HospClean, AgeClean, Sex, LabelRaceEth, RPL_THEMES, Year)])

m0<-glm(Severe ~ 1, data=nmdat, family = 'binomial')
m1<-update(m0, . ~ . + AgeClean)
anova(m0, m1, test='Chisq')

m2<-update(m1, . ~ . + Sex)
anova(m1, m2, test='Chisq')

m3<-update(m2, . ~ . + LabelRaceEth)
anova(m2, m3, test='Chisq')

m4<-update(m3, . ~ . + Year)
anova(m3, m4, test='Chisq')

m5<-update(m4, . ~ . + RPL_THEMES)
anova(m4, m5, test='Chisq')

#m6<-update(m5, . ~ . + State)
#anova(m5, m6, test='Chisq')

#Summary and output from model #5
summary(m5)
mlv.estimates<-data.frame(Parameters=names(m5$coefficients),
                          OR=exp(coef(m5)), 
                          CI=exp(confint(m5)), 
                          p=round(summary(m5)$coefficients[,4], 4),
                          Model='Multivariate')

#-------------------------------------------------------------------------------
# Table of univariate and multivariate effects
#-------------------------------------------------------------------------------
t2<-subset(rbind(uni.estimates, mlv.estimates), grepl('Intercept', Parameters)==F)
t2[, c('OR', 'CI.2.5..', 'CI.97.5..')]<-lapply(t2[, c('OR', 'CI.2.5..', 'CI.97.5..')], round, 2)
t2[, '95% CI' := paste0("(", sprintf("%0.2f", CI.2.5..), ' - ', sprintf("%0.2f", CI.97.5..), ")")]

t2<-dcast(t2, Parameters ~ Model, value.var=c('OR', '95% CI', 'p'))
t2<-t2[, c(1, 3, 5, 7, 2, 4, 6)]

t2[, Parameters := gsub("LabelRaceEth|Clean|Sex", "", Parameters)]
t2$Parameters[t2$Parameters=='RPL_THEMES']<-'SVI Score'

names(t2)[names(t2)=='OR_Multivariate']<-'aOR'
names(t2)<-gsub('_Univariate|_Multivariate', "", names(t2))

kbl(t2, align =c("l",  rep('r', 6))) %>%
  kable_classic(full_width=F, font_size = 12) %>%
  add_header_above(c(" "=1, "Unadjusted Odds Ratio for Severe Shigella" = 3, 
                            "Adjusted Odds Ratio for Severe Shigella"=3), bold=T) %>%
  pack_rows('Race and Ethnicity (Ref: White, NH)', 2, 6) %>%
  pack_rows('Sex (Ref: Female)', 8, 8) %>%
  save_kable('severity_ORestimates_table3_4B.png')

#-------------------------------------------------------------------------------
#Visualize predicted probabilities of Severe Shigella over values of SVI score by race
#-------------------------------------------------------------------------------
newdata1 <- with(nmdat, 
                 data.frame(RPL_THEMES = rep(seq(from = 0.0, to=1.0, length.out=100), times=12), 
                            LabelRaceEth = rep(unique(LabelRaceEth), each=200),
                            Sex = rep(c('Male', 'Female'), each=100, times=6),
                            AgeClean = median(AgeClean),
                            Year = median(Year)))

#-----------------------------------------------------------------------------
#To delete: this code is just included so I know I created by dataset correctly
newdata1 %>%
  group_by(Sex, LabelRaceEth) %>%
  summarise(length(RPL_THEMES), min=min(RPL_THEMES), max=max(RPL_THEMES))

table(newdata1$LabelRaceEth, newdata1$Sex)
#-----------------------------------------------------------------------------

newdata1$PredProb<-predict(m5, newdata=newdata1, type='response')
newdata1<-cbind(newdata1, predict(m5, newdata = newdata1, type='link', se=T))
newdata1$LL<-plogis(newdata1$fit - (1.96 *  newdata1$se.fit))
newdata1$UL<-plogis(newdata1$fit + (1.96 *  newdata1$se.fit))

ggplot(newdata1, aes(x = RPL_THEMES, y = PredProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill=LabelRaceEth), alpha = 0.4) + 
  geom_line(aes(color=LabelRaceEth), size=0.9) +
  facet_grid(cols=vars(LabelRaceEth), rows=vars(Sex))+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  scale_y_continuous(limits = c(0,1), name='Prediceted Probability of Severe Shigella') +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels=(function(x) sprintf("%.1f", x)), name='SVI Score') +
  labs(title='Predicted Probability of Severe Shigella by SVI Score') +
  theme_light()+
  theme(legend.position = 'none', panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill='gray25'))
ggsave('predicted_probs_severity_svi.png', width=8, height=6, units='in')

#-------------------------------------------------------------------------------
# Proportion of cases hospitalized by race/ethnicity
#-------------------------------------------------------------------------------
dat[!is.na(quartile), list(Percent=sum(HospClean==T)/.N), by=list(LabelRaceEth, quartile)] %>%
  ggplot(aes(x=LabelRaceEth, y=Percent))+
  geom_bar(stat='identity', fill='#D55E00') + 
  facet_grid(rows=vars(quartile), labeller=labeller(quartile=quartile.labs))+
  geom_text(aes(label=scales::percent(round(Percent,2)), y=Percent), stat="identity", vjust=-.5) +
  scale_y_continuous(limits=c(0,1),labels = scales::percent) +
  ylab("Percent of Cases") +
  labs(title='Percent of Shigella Cases Hospitalized by Race and SVI Quartile')+
  theme_light()+
  theme(axis.title.x = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background =element_rect(fill='gray25'))

                                                                                                