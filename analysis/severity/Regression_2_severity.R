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
folder<-"//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/Final Datasets/"
file<-'analytic_file_final_5192023.csv'

dat<-read.csv(paste0(folder, file), stringsAsFactors = F,
              colClasses = c('CTNO2000'='character', 'CTNO2010'='character', 'Deterministic'='character',
                             'CNTYFIPS'= 'character'))

#--------------------------------------------------------------------------------
#Data cleaning
#--------------------------------------------------------------------------------
svibreaks<-c(2000, 2006, 2011, 2015, 2017, 2020)
svilabels<-c('2000', '2010', '2014', '2016', '2018')

dat <- dat %>%
  mutate(FIPS =  ifelse(Year<2006, CTNO2000, CTNO2010), 
         sviyear = as.numeric(as.character(cut(Year, breaks=svibreaks, right=F, labels=svilabels))),
         Race = factor(Race, levels=c('W', 'I', 'A', 'B', 'M', 'P', 'O', 'U' ),
                            labels=c('White', 'American Indian and AK Native', 
                                     'Asian and Pacific Islander', 'Black', 'Multiracial',
                                     'Pacific Islander', 'Other Race', 'Unknown Race')),
         #LabelRace = factor(RaceGroupB, 
         #                  levels=c('White', 'Am Ind-AK Native', 'Asian-Pac Isldr', 'Black', 'Mltracial', 'Otr and Unkwn'),
         #                  labels=c('White', 'American Indian and AK Native', 'Asian and Pacific Islander', 
         #                           'Black', 'Multiracial', 'Other and unknown')),
         Ethnicity = factor(Ethnicity, levels=c('N', 'H', 'U'), labels=c('non-Hispanic', 'Hispanic', 'Unknown Ethnicity')),
         Sex = factor(Sex, levels=c('F', 'M'), labels=c('Female', 'Male'))) %>%
  mutate(FIPS = ifelse(is.na(FIPS), Deterministic, FIPS))

#-------------------------------------------------------------------------------
# Left-join region-specific SVI scores to cases
#-------------------------------------------------------------------------------
source('helper funcs/calc-region-specific-svi-score-tract.R')

dat<-dat %>% left_join(svi_all, join_by(FIPS, sviyear))

#-------------------------------------------------------------------------------
#Import NCHS Urban-Rural classification and merge to data by STCNTY
#-------------------------------------------------------------------------------
folder<-'//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/NCHS/'
nchs<-read.xlsx(paste0(folder, 'NCHSURCodes2013.xlsx'))
nchs$CNTYFIPS<-str_pad(nchs$FIPS.code, 5, "left", "0")
nchs$UrCode<-nchs$`2013.code`

dat <- dat  %>%
  left_join(nchs[, c('CNTYFIPS', 'UrCode')], join_by(CNTYFIPS)) %>%
  mutate(UrCode = factor(UrCode, levels=c(1:6), 
                         labels=c(rep('Large metro', 2), 'Medium metro', 'Small metro', rep('Rural',2))))

#-------------------------------------------------------------------------------
# Univariate models of odds of having severe shigella 
#-------------------------------------------------------------------------------
um0<-glm(Severe ~ 1, data=dat, family = 'binomial')
um1<-glm(Severe ~ Sex, data=dat, family = 'binomial')
um2<-glm(Severe ~ Race*Ethnicity, data=dat, family = 'binomial')
um3<-glm(Severe ~ Age, data=dat, family = 'binomial')
um4<-glm(Severe ~ Year, data=dat, family = 'binomial')
um5<-glm(Severe ~ rpl, data=dat, family = 'binomial')
um6<-glm(Severe ~ UrCode, data=dat, family = 'binomial')


model.list<-list(um1, um2, um3, um4, um5, um6)
names(model.list)<-c('Sex', 'Race/Ethnicity', 'Age', 'Year', 'SVI', 'Urban-Rural Classification')

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

#-------------------------------------------------------------------------------
# Model odds of having severe shigella (Multivariate)
#-------------------------------------------------------------------------------
nmdat<-na.omit(dat %>% select(Severe, UrCode, Sex, Race, Ethnicity, rpl, Year, Age))

m0<-glm(Severe ~ 1, data=nmdat, family = 'binomial')
m1<-update(m0, . ~ . + Age)
anova(m0, m1, test='Chisq')

m2<-update(m1, . ~ . + Sex)
anova(m1, m2, test='Chisq')

m3<-update(m2, . ~ . + Race*Ethnicity)
anova(m2, m3, test='Chisq')

m4<-update(m3, . ~ . + Year)
anova(m3, m4, test='Chisq')

m5<-update(m4, . ~ . + rpl)
anova(m4, m5, test='Chisq')

m6<-update(m5, . ~ . + UrCode)
anova(m5, m6, test='Chisq')

#Summary and output from best fit model 
summary(m6)
mlv.estimates<-data.frame(Parameters=names(m6$coefficients),
                          OR=exp(coef(m6)), 
                          CI=exp(confint(m6)), 
                          p=round(summary(m6)$coefficients[,4], 4),
                          Model='Multivariate')

#-------------------------------------------------------------------------------
# Table of univariate and multivariate effects
#-------------------------------------------------------------------------------
t2<-subset(rbind(uni.estimates, mlv.estimates), grepl('Intercept', Parameters)==F)
t2[, c('OR', 'CI.2.5..', 'CI.97.5..')]<-lapply(t2[, c('OR', 'CI.2.5..', 'CI.97.5..')], round, 2)
rownames(t2)<-NULL
t2<-t2 %>%
  mutate(`95% CI`= paste0("(", sprintf("%0.2f", CI.2.5..), ' - ', sprintf("%0.2f", CI.97.5..), ")"),
         p=cut(p, breaks=c(0, 0.001, 0.01, 0.05, 1), labels=c('***', '**', '*', ''), right=F),
         Parameters = gsub("^Race|Ethnicity(?=Hispanic|Unknown)|Sex|UrCode", "", Parameters, perl=T), 
         Parameters = gsub('rpl', 'SVI Score', Parameters)) %>%
  select(Parameters, OR, `95% CI`, p, Model) %>%
  pivot_wider(names_from = Model, values_from = c(OR, `95% CI`, p), names_vary="slowest") %>%
  rename(aOR=OR_Multivariate) 

names(t2)<-gsub('_Univariate|_Multivariate', "", names(t2))

t2$Parameters=cell_spec(t2$Parameters, bold=ifelse(t2$Parameters%in% c("Age", "Year", "SVI Score"), T, F))

kbl(t2, align =c("l",  rep('r', 6)), escape = F) %>%
  kable_classic(full_width=F, font_size = 14) %>%
  add_header_above(c(" "=1, "Unadjusted Odds Ratio for Severe Shigella" = 3, 
                            "Adjusted Odds Ratio for Severe Shigella"=3), bold=T) %>%
  pack_rows('Sex (Ref: Female)', 1, 1) %>%
  pack_rows('Race (Ref: White)', 2, 8) %>%
  pack_rows('Ethnicity (Ref: non-Hispanic)', 9, 10) %>%
  pack_rows('Race-Ethnicity Interaction (Ref: non-Hispanic White)', 11, 24) %>%
  pack_rows('Urbanicity (Ref: Large Metro)', 28, 30) %>%
  footnote(general = "*** p < 0.001, ** p < 0.01, * p < 0.05")  %>%
  save_kable('charts/severity_ORestimates_table3_4B.png')

#-------------------------------------------------------------------------------
#Visualize predicted probabilities of Severe Shigella over values of SVI score by race
#-------------------------------------------------------------------------------
newdata1 <- with(nmdat, data.frame(rpl = rep(seq(from = 0.0, to=1.0, length.out=100), times=24), 
                            Race = rep(unique(Race), each=300),
                            Ethnicity = rep(unique(Ethnicity), times=800),
                            Sex = rep('Male', times=1200),
                            Age = median(Age),
                            UrCode = factor('Large metro', 
                                            levels=c('Large metro','Medium metro', 'Small metro', 'Rural')),
                            Year = median(Year)))

#-----------------------------------------------------------------------------
#To delete: this code is just included so I know I created by dataset correctly
newdata1 %>%
  group_by(Sex, Race, Ethnicity) %>%
  summarise(length(rpl), min=min(rpl), max=max(rpl))

table(newdata1$Race, newdata1$Sex)
#-----------------------------------------------------------------------------

newdata1$PredProb<-predict(m6, newdata=newdata1, type='response')
newdata1<-cbind(newdata1, predict(m6, newdata = newdata1, type='link', se=T))
newdata1$LL<-plogis(newdata1$fit - (1.96 *  newdata1$se.fit))
newdata1$UL<-plogis(newdata1$fit + (1.96 *  newdata1$se.fit))

ggplot(newdata1, aes(x = rpl, y = PredProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill=Race), alpha = 0.4) + 
  geom_line(aes(color=Race), linewidth=0.9) +
  facet_grid(cols=vars(Race), rows=vars(Ethnicity), labeller=label_wrap_gen(width=5, multi_line=T))+
  scale_color_colorblind()+
  scale_fill_colorblind()+
  scale_y_continuous(limits = c(0,1), name='Prediceted Probability of Severe Shigella') +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels=(function(x) sprintf("%.1f", x)), name='SVI Score') +
  labs(title='Predicted Probability of Severe Shigella by SVI Score') +
  theme_light()+
  theme(legend.position = 'none', panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill='gray25'))
ggsave('charts/predicted_probs_severity_svi.png', width=8, height=6, units='in')

#-------------------------------------------------------------------------------
# Proportion of cases hospitalized by race/ethnicity
#-------------------------------------------------------------------------------
dat %>%
  group_by(Race, Ethnicity)  %>%
  summarise(Percent=sum(HospClean==T)/n()) %>%
  ggplot(aes(x=Race, y=Percent, Group=Ethnicity))+
  facet_wrap(~Ethnicity, nrow=3) + 
  geom_bar(stat='identity', position = position_dodge(), aes(fill=Race)) + 
  #facet_grid(rows=vars(quartile), labeller=labeller(quartile=quartile.labs))+
  geom_text(aes(label=scales::percent(round(Percent,2)), y=Percent), stat="identity", vjust=-.5) +
  scale_y_continuous(limits=c(0,1),labels = scales::percent) +
  ylab("Percent of Cases") +
  labs(title='Percent of Shigella Cases Hospitalized by Race and Ethnicity')+
  theme_light()+
  theme(axis.title.x = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background =element_rect(fill='gray25'))

                                                                                                