library('openxlsx')
library('ggplot2')
library('kableExtra')
library('dplyr')
library('colorspace')
library('tidycensus')
library('tidyr')
#--------------------------------------------------------------------------------
#Import final, cleaned FoodNet dataset 
#--------------------------------------------------------------------------------
folder<-"//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/Final Datasets/"
dat<-read.csv(paste0(folder, 'analytic_file_final_06062023.csv'), stringsAsFactors = F,
              colClasses = c('CNTYFIPS'='character'))

#--------------------------------------------------------------------------------
#Data cleaning
#--------------------------------------------------------------------------------
dat <- dat %>%
  mutate(LabelRace = factor(RacEthGroupA, 
                            levels=c('White-NH', 'AmInd-AKNat', 'Asian-NH', 'Black-NH', 'Hispanic', 
                                     'Multiracial-NH', 'NatHwn-PI',   'Other-NH',  'Unknown'),
                            labels=c('White, non-Hispanic', 
                                     'American Indian and AK Native', 
                                     'Asian, non-Hispanic', 
                                     'Black, non-Hispanic', 
                                     'Hispanic',
                                     'Multiracial, non-Hispanic', 
                                     'Native Hawaiian and Pacific Islander',
                                     'Other and unknown', 'Other and unknown')),
         Sex = factor(Sex, levels=c('F', 'M'), labels=c('Female', 'Male')),
         AMR = AbxResSum>0) 

#-------------------------------------------------------------------------------
# Update CNTYFIPS Code based on reported County if missing
#-------------------------------------------------------------------------------
data(fips_codes)
fips_codes$county<-toupper(gsub(' County',"", fips_codes$county))
dat$County[dat$County=='PRINCE GEORGES']<-"PRINCE GEORGE'S"

dat <- dat %>%
  left_join(fips_codes, join_by(State==state, County==county)) %>%
  mutate(CNTYFIPS=ifelse(is.na(CNTYFIPS), paste0(state_code, county_code), CNTYFIPS))

#-------------------------------------------------------------------------------
#Import NCHS Urban-Rural classification and merge to data by STCNTY
#-------------------------------------------------------------------------------
folder<-'//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/NCHS/'
nchs<-read.xlsx(paste0(folder, 'NCHSURCodes2013.xlsx'))
nchs$CNTYFIPS<-stringr::str_pad(nchs$FIPS.code, 5, "left", "0")
nchs$UrCode<-nchs$`2013.code`

dat <- dat  %>%
  left_join(nchs[, c('CNTYFIPS', 'UrCode')], join_by(CNTYFIPS)) %>%
  mutate(Urbanicity = factor(UrCode, levels=c(6:1), 
                        labels=c(rep('Rural',2), 'Small metro','Medium metro', rep('Large metro', 2))
                        ))

#-------------------------------------------------------------------------------
#Keep eligible cases
#-------------------------------------------------------------------------------
dat <- dat %>% filter(SubPop==T)

#-------------------------------------------------------------------------------
# Univariate models of odds of having antimicrobial resistant shigella 
#-------------------------------------------------------------------------------
um0<-glm(AMR ~ 1, data=dat, family = 'binomial')
um1<-glm(AMR ~ Year, data=dat, family = 'binomial')
um2<-glm(AMR ~ Sex, data=dat, family = 'binomial')
um3<-glm(AMR ~ Age, data=dat, family = 'binomial')
um4<-glm(AMR ~ LabelRace, data=dat, family = 'binomial')
um5<-glm(AMR ~ rpl, data=dat, family = 'binomial')
um6<-glm(AMR ~ LabelRace*rpl, data=dat, family = 'binomial')
um7<-glm(AMR ~ Urbanicity, data=dat, family = 'binomial')


model.list<-list(um1, um2, um3, um4, um5, um6, um7)
names(model.list)<-c('Year', 'Sex', 'Age', 'Race/Ethnicity', 'Overall SVI', 
                     'Race/Ethnicity-SVI Interaction', 'Urban-Rural Classification')

uni.estimates<-data.frame()

for (i in 1:length(model.list)){
  temp<-cbind(
  data.frame(Parameters=names(model.list[[i]]$coefficients)),
  data.frame(OR=exp(coef(model.list[[i]]))), 
  data.frame(CI=exp(confint(model.list[[i]], method='Wald'))),
  data.frame(p=round(summary(model.list[[i]])$coefficients[, 4], 4)))
  uni.estimates<-rbind(uni.estimates, temp)
}

uni.estimates$Model<-'Univariate'
uni.estimates <- subset(uni.estimates, grepl('(LabelRace|rpl).*[1]', row.names(uni.estimates))==F)

#-------------------------------------------------------------------------------
# Model odds of having severe shigella (Multivariate)
#-------------------------------------------------------------------------------
nmdat<-na.omit(dat %>% 
      select(AMR, Year, Age, Urbanicity, Sex, LabelRace, t1rpl, t2rpl, t3rpl, t4rpl, rpl))

m0<-glm(AMR ~ 1, data=nmdat, family = 'binomial')
m1<-update(m0, . ~ . + Year)
anova(m0, m1, test='Chisq')

m2<-update(m1, . ~ . + Sex)
anova(m1, m2, test='Chisq')

m3<-update(m2, . ~ . + Age)
anova(m2, m3, test='Chisq')

m4<-update(m3, . ~ . + LabelRace)
anova(m3, m4, test='Chisq')

m5<-update(m4, . ~ . + rpl) 
anova(m4, m5, test='Chisq')

m6<-update(m5, . ~ . + LabelRace*rpl) 
anova(m5, m6, test='Chisq')

m7<-update(m6, . ~ . + Urbanicity) 
anova(m6, m7, test='Chisq')

#Summary and output from best fit model 
finmod<-m7
summary(finmod)
mlv.estimates<-data.frame(Parameters=names(finmod$coefficients),
                          OR=exp(coef(finmod)), 
                          CI=exp(confint(finmod)), 
                          p=round(summary(finmod)$coefficients[,4], 4),
                          Model='Multivariate')

#-------------------------------------------------------------------------------
# Table of univariate and multivariate effects
#-------------------------------------------------------------------------------
t2<-subset(rbind(uni.estimates, mlv.estimates), grepl('Intercept', Parameters)==F)
t2[, c('OR', 'CI.2.5..', 'CI.97.5..')]<-lapply(t2[, c('OR', 'CI.2.5..', 'CI.97.5..')], round, 2)
rownames(t2)<-NULL
t2<-t2 %>%
  mutate(na = rowSums(is.na(select(., CI.2.5.., CI.97.5..)))) %>%
  mutate(`95% CI`= ifelse(na==0, 
                          paste0("(", sprintf("%0.2f", CI.2.5..), ' - ', sprintf("%0.2f", CI.97.5..), ")"),
                          "-"), 
         OR = ifelse(na>0, "-", sprintf("%0.2f", OR)), 
         p=cut(p, breaks=c(0, 0.001, 0.01, 0.05, 1), labels=c('***', '**', '*', ''), right=F),
         #Parameters = gsub("^Race|Ethnicity(?=Hispanic|Unknown)|Sex|UrCode", "", Parameters, perl=T), 
         Parameters = gsub("^LabelRace|Sex|Urbanicity", "", Parameters, perl=T),
         Parameters = gsub('rpl', 'SVI Score', Parameters)) %>%
  select(Parameters, OR, `95% CI`, p, Model) %>%
  pivot_wider(names_from = Model, values_from = c(OR, `95% CI`, p), names_vary="slowest") %>%
  rename(aOR=OR_Multivariate) 

names(t2)<-gsub('_Univariate|_Multivariate', "", names(t2))

t2$Parameters=cell_spec(t2$Parameters, bold=ifelse(t2$Parameters%in% c("Age", "Year", "SVI Score"), T, F))

kbl(t2, align =c("l",  rep('r', 6)), escape = F) %>%
  kable_classic(full_width=F, font_size = 14) %>%
  add_header_above(c(" "=1, "Unadjusted Odds Ratio
                     for AMR" = 3, 
                            "Adjusted Odds Ratio
                     for AMR"=3), 
                   bold=T, font_size = 16) %>%
  column_spec(1, width = '3in') %>%
  column_spec(c(2, 5), width = '0.6in') %>%
  column_spec(c(3, 6), width = '1in') %>%
  column_spec(c(4, 7), width = '0.5') %>%
  pack_rows('Sex (Ref: Female)', 2, 2) %>%
  pack_rows('Race and Ethnicity (Ref: White, non-Hispanic)', 4, 10) %>%
  pack_rows('SVI Score/Race and Ethnicity Interaction', 12, 18) %>%
  pack_rows('Urbanicity (Ref: Rural)', 19, 21) %>%
  footnote(general = "*** p < 0.001, ** p < 0.01, * p < 0.05
Undefined estimates are suppressed (-).
AMR = antimicrobial resistance")  %>%
  save_kable('charts/amr_ORestimates_table3_4A.png')

#-------------------------------------------------------------------------------
# Theme-specific SVI Scores
#-------------------------------------------------------------------------------
tsmod1<-update(finmod, . ~ . -rpl -LabelRace*rpl + t1rpl+ LabelRace*t1rpl) 
tsmod2<-update(finmod, . ~ . -rpl -LabelRace*rpl + t2rpl+ LabelRace*t2rpl) 
tsmod3<-update(finmod, . ~ . -rpl -LabelRace*rpl + t3rpl+ LabelRace*t3rpl) 
tsmod4<-update(finmod, . ~ . -rpl -LabelRace*rpl + t4rpl+ LabelRace*t4rpl) 

model.list<-list(tsmod1, tsmod2, tsmod3, tsmod4)
names(model.list)<-c('Theme 1', 'Theme 2', 'Theme 3', 'Theme 4')

tsmod<-data.frame()

for (i in 1:length(model.list)){
  temp<-cbind(
    data.frame(Theme=names(model.list)[[i]]),
    data.frame(Parameters=names(model.list[[i]]$coefficients)),
    data.frame(aOR=exp(coef(model.list[[i]]))), 
    data.frame(CI=exp(confint(model.list[[i]], method='Wald'))),
    data.frame(p=round(summary(model.list[[i]])$coefficients[, 4], 4)))
  tsmod<-rbind(tsmod, temp)
}

rownames(tsmod)<-NULL

tsmod<-tsmod %>%
  filter(grepl('LabelRace|t[1-4]rpl', Parameters)==T) %>%
  mutate(na = rowSums(is.na(select(., CI.2.5.., CI.97.5..)))) %>%
  mutate(aOR = ifelse(na==0, sprintf("%0.2f", aOR), "-"),
         `95% CI`= ifelse(na==0, 
                      paste0("(", sprintf("%0.2f", CI.2.5..), ' - ', sprintf("%0.2f", CI.97.5..), ")"),
                      '-'),
         p=cut(p, breaks=c(0, 0.001, 0.01, 0.05, 1), labels=c('***', '**', '*', ''), right=F),
         Parameters = gsub("LabelRace|t[1-4]", "", Parameters, perl=T),
         Parameters = gsub('rpl', 'Theme SVI Score', Parameters)) %>%
  select(Parameters, aOR, `95% CI`, p, Theme) %>%
  pivot_wider(names_from = Theme, values_from = c(aOR, `95% CI`, p), names_vary="slowest") 

names(tsmod)<-gsub('_Theme\\s[1-4]', "", names(tsmod))

tsmod$Parameters=cell_spec(tsmod$Parameters, bold=ifelse(tsmod$Parameters=="Theme SVI Score", T, F))

kbl(tsmod, align =c("l",  rep('r', 12)), escape = F) %>%
  kable_classic(full_width=F, font_size = 14) %>%
  add_header_above(c(" "=1, "Theme 1 [note]" = 3, "Theme 2 [note]"=3, "Theme 3 [note]"=3, "Theme 4 [note]"=3), 
                   bold=T, font_size = 16) %>%
  column_spec(1, width = '4in') %>%
  column_spec(c(2, 5, 8, 11), width = '0.6in') %>%
  column_spec(c(3, 6, 9, 12), width = '1in') %>%
  column_spec(c(4, 7, 10, 13), width = '0.6') %>%
  pack_rows('Race and Ethnicity (Ref: White, non-Hispanic)', 2, 8) %>%
  pack_rows('SVI Score/Race and Ethnicity Interaction', 9, 15) %>%
  footnote(general = "All models are adjusted for year, age, sex, and urban-rural classification of residence county.
*** p < 0.001, ** p < 0.01, * p < 0.05") %>%
  add_footnote(c('Socioeconomic status', 'Household composition', 
                 'Minority status and language', 'Housing and transportation'),  notation='symbol') %>%
  save_kable('charts/amr_themeORs_table3_4B.png')

#-------------------------------------------------------------------------------
#Visualize predicted probabilities of Severe Shigella over values of SVI score by race
#-------------------------------------------------------------------------------
newdata <- with(nmdat, 
                data.frame(rpl = rep(seq(from = 0.0, to=1.0, length.out=100), times=8), 
                           t1rpl = rep(seq(from = 0.0, to=1.0, length.out=100), times=8), 
                           t2rpl = rep(seq(from = 0.0, to=1.0, length.out=100), times=8), 
                           t3rpl = rep(seq(from = 0.0, to=1.0, length.out=100), times=8),
                           t4rpl = rep(seq(from = 0.0, to=1.0, length.out=100), times=8), 
                          LabelRace = rep(unique(LabelRace), each=100),
                          Sex = 'Male',
                          Age = median(Age),
                          Urbanicity = factor('Large metro', levels=c('Large metro','Medium metro', 'Small metro', 'Rural')),
                          Year = median(Year)))

#-----------------------------------------------------------------------------
#To delete: this code is just included so I know I created by dataset correctly
newdata %>%
  group_by(Sex, LabelRace) %>%
  summarise(length(rpl), min=min(rpl), max=max(rpl))

#-----------------------------------------------------------------------------
model.list<-list(finmod, tsmod1, tsmod2, tsmod3, tsmod4)
names(model.list)<-c('Overall', 'Theme 1', 'Theme 2', 'Theme 3', 'Theme 4')

Out<-data.frame()
for ( i in 1:length(model.list)) {
  Out<-rbind(Out,
        cbind(
          Score = newdata[, i],
          data.frame(
              Theme=names(model.list[i]),
              LabelRace=newdata$LabelRace,  
              PredProb=predict(model.list[[i]], newdata=newdata, type='response'),
         predict(model.list[[i]], newdata = newdata, type='link', se=T))    
        )
  )
}  

Out$LL<-plogis(Out$fit - (1.96 *  Out$se.fit))
Out$UL<-plogis(Out$fit + (1.96 *  Out$se.fit))

ggplot(Out, aes(x = Score, y = PredProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill=LabelRace), alpha = 0.25) + 
  geom_line(aes(color=LabelRace), linewidth=0.9) +
  facet_grid(cols=vars(LabelRace), rows=vars(Theme), labeller=label_wrap_gen(width=5, multi_line=T)) +
  scale_color_discrete_qualitative(palette = "Dark 3")+
  scale_fill_discrete_qualitative(palette = "Dark 3")+
  scale_y_continuous(limits = c(0,1), breaks = c(0, 0.5, 1),
                     name='Predicted Probability of Severe Shigella') +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels=(function(x) sprintf("%.1f", x)), 
                     name='SVI Score') +
  labs(title='Predicted Probability of Severe Shigella by SVI Score',
       caption = "SVI = Social Vulnerability Score
Theme 1 = Socioeconomic status
Theme 2 = Household composition
Theme 3 = Minority status and language
Theme 4 = Housing and transportation") +
  theme_light()+
  theme(legend.position = 'none', 
        panel.grid = element_blank(),
        strip.text = element_text(face='bold', colour = 'white'),
        strip.background =element_rect(fill='gray25'),
        plot.caption = element_text(hjust = 0)
        ) 
ggsave('charts/predicted_probs_amr_svi.png', width=9, height=7, units='in')

#-------------------------------------------------------------------------------
# Proportion of cases with AMR by race/ethnicity
#-------------------------------------------------------------------------------
dat %>%
  filter(!is.na(qrtile)) %>%
  group_by(LabelRace, qrtile)  %>%
  summarise(Percent=sum(AMR==T)/n()) %>%
  ggplot(aes(x=LabelRace, y=Percent))+
  #facet_wrap(~Ethnicity, nrow=3) + 
  geom_bar(stat='identity', position = position_dodge(), aes(fill=LabelRace)) + 
  facet_grid(rows=vars(qrtile))+
  geom_text(aes(label=scales::percent(round(Percent,2)), y=Percent), stat="identity", vjust=-.5) +
  scale_y_continuous(limits=c(0,1),labels = scales::percent) +
  ylab("Percent of Cases") +
  labs(title='Percent of Shigella Cases With Antimicrobial Resistance by Race and Ethnicity')+
  theme_light()+
  theme(axis.title.x = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = 'none',
        strip.background =element_rect(fill='gray35'))
ggsave('charts/percent-amr-race-svi.png', width=8, height=8, units='in')

                                                                                                