library('glmmTMB')
library('DHARMa')
library('ggplot2')
library('ggthemes')
library('tidyverse')
library('kableExtra')
library('ggsci')

#--------------------------------------------------------------------------------
#Import county-level dataset 
#--------------------------------------------------------------------------------
folder<-"//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/Final Datasets/"
dat<-read.csv(paste0(folder, 'Final_County_AgeAdjusted_ByRaceEth.csv'), 
              colClasses = c('County'='character'))

#formatting variables
racelabels<-c('White, NH','American Indian\n or AK Native', 'Asian or\n Pacific Islander, NH',
              'Black, NH', 'Hispanic')

dat <- dat %>%
  mutate(State= substr(County, 1, 2),
         LabelRaceEth = factor(raceeth, 
                        levels=c('White-NH', 'AmInd-AKNat','Asian, NH and PI','Black-NH', 'Hispanic'), 
                        labels=racelabels),
          UrCode= factor(UrCode, levels=c(1:6), 
                    labels=c(rep('Large metro', 2), 'Medium metro', 'Small metro', rep('Rural',2))))

#--------------------------------------------------------------------------------
# Counties excluded due to missing SVI and/or zero population
#--------------------------------------------------------------------------------
dat %>% filter(is.na(rpl)) %>% summarise(n_distinct(County))
dat %>% filter(!is.na(rpl)) %>% group_by(Pop==0) %>% 
  summarise(n_distinct(County), n())

#--------------------------------------------------------------------------------
# Distribution of zero cases and var:mean ratio
#--------------------------------------------------------------------------------
dat %>% filter(!is.na(rpl) & Pop>0 ) %>%
  ggplot(aes(x=Cases)) +
  geom_histogram(bins=50, alpha=0.8) +
  facet_grid(rows=vars(sviyear),  scales = 'free')+
  ggtitle("Counties by the Number of Shigella Cases")

dat %>%
  filter(!is.na(rpl) & Pop>0 ) %>% 
  summarise(mean(AdjCases), var(AdjCases), ratio=var(AdjCases)/mean(AdjCases),
            total_records=n(), zero_cases=sum(AdjCases==0)) %>%
  mutate( percent_zero=zero_cases/total_records*100)
           
#--------------------------------------------------------------------------------
#Multivariate analysis
#--------------------------------------------------------------------------------
#Drop counties without SVI data or population and split dataset by age group
nomis <- dat %>%
  filter(Pop > 0) %>%
  select(AdjCases, Pop, State, County, LabelRaceEth, rpl, t1rpl, t2rpl, t3rpl, 
         t4rpl, UrCode, ESTAB_ChildCare, Prisons, PopDensity, Pop, County, sviyear) %>%
  na.omit() 

# Develop NULL MODELS comparing random effects in both conditional and binomial
# portions of the model
#--------------------------------------------------------------------------------
mod0a<-glmmTMB(AdjCases ~ 1 + offset(log(Pop)), zi=~1, data=nomis, family=truncated_nbinom2)

#Add state/county random effects to conditional portion
mod0a.1<-update(mod0a, . ~ . + (1|State) + (1|State:County))
anova(mod0a, mod0a.1)

#Add state/county random effects to binomial portion
mod0a.2<-update(mod0a.1, . ~ . , ziformula = ~(1|State) + (1|State:County))
anova(mod0a.1, mod0a.2)

#Evaluate covariates individually
#--------------------------------------------------------------------------------
mod1a.1<-update(mod0a.2, . ~ . + LabelRaceEth)
mod1a.2<-update(mod0a.2, . ~ . + rpl)
mod1a.3<-update(mod0a.2, . ~ . + UrCode)
mod1a.4<-update(mod0a.2, . ~ . + ESTAB_ChildCare)
mod1a.5<-update(mod0a.2, . ~ . + PopDensity)
mod1a.6<-update(mod0a.2, . ~ . + Prisons)
mod1a.7<-update(mod0a.2, . ~ . , ziformula = ~.+LabelRaceEth )
mod1a.8<-update(mod0a.2, . ~ . , ziformula = ~.+rpl)
mod1a.9<-update(mod0a.2, . ~ . , ziformula = ~.+UrCode)
mod1a.10<-update(mod0a.2, . ~ . , ziformula = ~.+ESTAB_ChildCare)
mod1a.11<-update(mod0a.2, . ~ . , ziformula = ~.+PopDensity)
mod1a.12<-update(mod0a.2, . ~ . , ziformula = ~.+Prisons)
mod1a.13<-update(mod0a.2, . ~ . , ziformula = ~.+scale(Pop, scale=F))

anova(mod0a.2, mod1a.1)
anova(mod0a.2, mod1a.2)
anova(mod0a.2, mod1a.3) 
anova(mod0a.2, mod1a.4)
anova(mod0a.2, mod1a.5) #No improvement in fit
anova(mod0a.2, mod1a.6)
anova(mod0a.2, mod1a.7)
anova(mod0a.2, mod1a.8)
anova(mod0a.2, mod1a.9)
anova(mod0a.2, mod1a.10)
anova(mod0a.2, mod1a.11)
anova(mod0a.2, mod1a.12)
anova(mod0a.2, mod1a.13)

#Evaluate sequentially (include covariates with univariate association only)
#--------------------------------------------------------------------------------
mod2a<-update(mod1a.1, . ~ . + rpl)
summary(mod2a)
anova(mod1a.2, mod2a)

mod3a<-update(mod2a, . ~ . +rpl*LabelRaceEth) 
summary(mod3a)
anova(mod2a, mod3a) 

mod4a<-update(mod3a, . ~ . + UrCode) 
summary(mod4a)
anova(mod3a, mod4a) 

mod5a<-update(mod4a, . ~ . + ESTAB_ChildCare) 
summary(mod5a)
anova(mod4a, mod5a) 

mod6a<-update(mod5a, . ~ . + Prisons) 
summary(mod6a)
anova(mod5a, mod6a)  # No improvement in fit

mod7a<-update(mod5a, . ~ . , ziformula = ~(1|State) + (1|State:County) + LabelRaceEth )
summary(mod7a)
anova(mod5a, mod7a)

mod8a<-update(mod7a, . ~ . , ziformula = ~(1|State) + (1|State:County) + LabelRaceEth + rpl)
summary(mod8a)
anova(mod7a, mod8a)

mod9a<-update(mod8a, . ~ . , ziformula = ~ (1|State) + (1|State:County) + LabelRaceEth + UrCode)
summary(mod9a)
anova(mod8a, mod9a)

mod10a<-update(mod8a, . ~ . -UrCode, ziformula = ~ (1|State) + (1|State:County) + LabelRaceEth + UrCode)
summary(mod10a)
anova(mod9a, mod10a) #Dropping UrCode from conditional model 

mod11a<-update(mod10a, . ~ . , ziformula = ~ (1|State) + (1|State:County) + LabelRaceEth + UrCode +  ESTAB_ChildCare)
summary(mod11a)
anova(mod10a, mod11a)

mod12a<-update(mod11a, . ~ . , ziformula = ~ (1|State) + (1|State:County) + LabelRaceEth + UrCode +  ESTAB_ChildCare + Prisons)
summary(mod12a)
anova(mod11a, mod12a) 

mod13a<-update(mod12a, . ~ . , ziformula = ~ . + UrCode + Prisons)
summary(mod13a)
anova(mod12a, mod13a) 

mod14a<-update(mod13a, . ~ . , ziformula = ~ . + UrCode + Prisons +  scale(Pop, scale=F))
summary(mod14a)
anova(mod13a, mod14a)

mod15a<-update(mod14a, . ~ . , ziformula = ~ . -LabelRaceEth:rpl + UrCode + Prisons +  scale(Pop, scale=F))
summary(mod15a)
anova(mod14a, mod15a)

mod16a<-update(mod15a, . ~ . , ziformula = ~ . -LabelRaceEth:rpl + UrCode + Prisons +  scale(Pop, scale=F) + PopDensity)
summary(mod16a)
anova(mod16a, mod15a) #No Improvement in fit

# View AIC for all models
hurdle.0to4.comparison<-anova(mod1a.1, mod2a, mod3a, mod4a, mod5a, mod6a, mod7a, 
                              mod8a, mod9a, mod10a, mod11a, mod12a, mod13a, mod14a, 
                              mod15a, mod16a)
hurdle.0to4.comparison[,c(1:5, 8)] %>% arrange(desc(AIC))

# Summary of best fitted model
BestModa<-mod15a
summary(BestModa)
rm(mod0a, mod0a.1, mod0a.2, mod1a.1, mod1a.2, mod1a.3, mod1a.4, mod1a.5, mod1a.6, 
   mod1a.7, mod1a.8, mod1a.9, mod1a.10, mod1a.11, mod1a.12, mod1a.13, mod1a.14, 
   mod1a.15, mod1a.16, mod2a, mod3a, mod4a, mod5a, mod6a, mod7a, mod8a, mod9a, mod10a, 
   mod11a, mod12a, mod13a, mod14a, mod15a, mod16a)

#--------------------------------------------------------------------------------
# Evaluation of model fit
#--------------------------------------------------------------------------------
simulationOutputa <- simulateResiduals(fittedModel = BestModa, plot=F) 
plot(simulationOutputa)

plotResiduals(simulationOutputa, form=nomis$LabelRaceEth)
plotResiduals(simulationOutputa, form=nomis$rpl)
plotResiduals(simulationOutputa, form=nomis$ESTAB_ChildCare)
plotResiduals(simulationOutputa, form=nomis$Prisons)
plotResiduals(simulationOutputa, form=nomis$UrCode)
testDispersion(simulationOutput = simulationOutputa)
testOutliers(simulationOutputa, type='bootstrap')

rm(simulationOutputa)
#--------------------------------------------------------------------------------
# Evaluate SVI sub-themes #1: SES Status
#--------------------------------------------------------------------------------
summary(BestModb<-update(BestModa, . ~ . -rpl +  t1rpl - LabelRaceEth:rpl + LabelRaceEth:t1rpl,
                         ziformula = ~ . -LabelRaceEth:t1rpl + UrCode + Prisons +  scale(Pop, scale=F)))
simulationOutputb <- simulateResiduals(fittedModel = BestModb, plot=F) 
plot(simulationOutputb)

#--------------------------------------------------------------------------------
# Evaluate SVI sub-themes #2: Household Composition and Disability
#--------------------------------------------------------------------------------
summary(BestModc<-update(BestModa, . ~ . -rpl +  t2rpl - LabelRaceEth:rpl + LabelRaceEth:t2rpl,
                         ziformula = ~ . -LabelRaceEth:t2rpl + UrCode + Prisons +  scale(Pop, scale=F)))
simulationOutputc <- simulateResiduals(fittedModel = BestModc, plot=F) 
plot(simulationOutputc)

#--------------------------------------------------------------------------------
# Evaluate SVI sub-themes #3: Minority Status and Language
#--------------------------------------------------------------------------------
summary(BestModd<-update(BestModa, . ~ . -rpl +  t3rpl - LabelRaceEth:rpl + LabelRaceEth:t3rpl,
                         ziformula = ~ . -LabelRaceEth:t3rpl + UrCode + Prisons +  scale(Pop, scale=F)))
simulationOutputd <- simulateResiduals(fittedModel = BestModd, plot=F) 
plot(simulationOutputd)

#--------------------------------------------------------------------------------
# Evaluate SVI sub-themes #4: Housing Type and Transportation
#--------------------------------------------------------------------------------
summary(BestMode<-update(BestModa, . ~ . -rpl +  t4rpl - LabelRaceEth:rpl + LabelRaceEth:t4rpl,
                         ziformula = ~ . -LabelRaceEth:t4rpl + UrCode + Prisons +  scale(Pop, scale=F)))
simulationOutpute <- simulateResiduals(fittedModel = BestMode, plot=F) 
plot(simulationOutpute)

#--------------------------------------------------------------------------------
#Extract Fixed Effect Odds Ratios and 95% Confidence Intervals
#--------------------------------------------------------------------------------
#Write a function to extract fixed effects from list of models
extract_feffects<-function(x) {
  feffects<-data.frame()
    MoA_CI=data.frame(CI=exp(confint(x, method='Wald'))) 
    MoA_CI$Measure<-ifelse(grepl('cond', row.names(MoA_CI))==T, 'IRR', 'OR')
    MoA_CI$Covariate<-gsub("cond\\.|zi\\.", "", row.names(MoA_CI))
    names(MoA_CI)<-c('Lower.CI', 'Upper.CI', 'Estimate', 'Measure', 'Covariate')
    #MoA_CI$Theme<- names(x)
    temp<-summary(x)$coefficients
    sig<-rbind(data.frame(p=temp$cond[,4], Measure='IRR', Covariate=row.names(temp$cond)),
               data.frame(p=temp$zi[, 4],  Measure='OR', Covariate=row.names(temp$zi))) 
    MoA_CI<-merge(MoA_CI, sig, by=c('Covariate', 'Measure'), all.x=T)
    feffects<-rbind(feffects, MoA_CI)
  return(feffects)
}

#Add all models to a list
mod.list<-list(Overall=BestModa, 'Theme 1'=BestModb, 'Theme 2'=BestModc, 'Theme 3'=BestModd, 'Theme 4'=BestMode)

#Iterate through list of models and extract fixed effects
results<-data.frame()
for(i in 1:length(mod.list)) {
  temp<- extract_feffects(mod.list[[i]])
  temp$Theme <- names(mod.list[i])
  results<-rbind(results, temp)
}

# Take inverse of odds ratios for zero component so that we are measuring the 
# odds of having AT LEAST ONE CASE instead of the odds of having ZERO cases
results<-results %>% 
  mutate(across(c(Lower.CI, Upper.CI, Estimate), function(x) ifelse(Measure=='OR', 1/x, x))) %>%
  mutate(NewLowCI = ifelse(Measure=='OR', Upper.CI, NA), 
         Upper.CI = ifelse(Measure=='OR', Lower.CI, Upper.CI), 
         Lower.CI = ifelse(Measure=='OR', NewLowCI, Lower.CI)) %>%
  select(-NewLowCI)
  
#--------------------------------------------------------------------------------
#Format variables for tables and charts
#--------------------------------------------------------------------------------
results <- results %>%
  mutate(Covariate = gsub('LabelRaceEth|UrCode', "", Covariate),
         Covariate = gsub('rpl|t[1-4]rpl', "SVI Score", Covariate),
         Covariate = case_when(Covariate == 'ESTAB_ChildCare' ~ "Child Care Establishments",
                               .default = as.character(Covariate)),
         Significance = as.character(
                        cut(p, 
                            breaks=c(0, 0.001, 0.01, 0.05, 1), 
                            labels=c('***', '**', '*', ''), right=F)),
         Measure=factor(Measure, levels=c('OR', 'IRR')),
         Estimate_CI = paste0(sprintf("%0.2f", Estimate), 
                              " (", sprintf("%0.2f", Lower.CI), 
                              "-", sprintf("%0.2f", Upper.CI), ")"))

# Keep only "interesting" parameters and pivot wider
overall.results <- results %>%
  filter(grepl('Intercept|scale', Covariate)==F & Theme=='Overall') %>%
  arrange(Measure) %>%
  pivot_wider(id_cols = c(Covariate), 
              names_from=c(Measure), 
              values_from = c(Estimate_CI, Significance),
              names_vary = "slowest",
              values_fill = '-')

theme.results <- results %>%
  filter(grepl('Intercept|scale', Covariate)==F & Theme!='Overall') %>%
  arrange(Measure) %>%
  pivot_wider(id_cols = c(Covariate), 
              names_from=c(Theme, Measure), 
              values_from = c(Estimate_CI, Significance),
              names_vary = c("slowest"), 
              names_sort = T,
              values_fill = '-')

#Factor "Covariate" column so table is sorted properly
covariate.levels<-c('SVI Score',
                    'American Indian\n or AK Native', 
                    'Asian or\n Pacific Islander, NH',
                    'Black, NH',
                    'Hispanic',
                    'American Indian\n or AK Native:SVI Score',
                    'Asian or\n Pacific Islander, NH:SVI Score',
                    'Black, NH:SVI Score',
                    'Hispanic:SVI Score',
                    'Child Care Establishments',
                    'Prisons',
                    'Medium metro',
                    'Small metro',
                    'Rural')

overall.results <- overall.results %>% 
  mutate(Covariate = factor(Covariate, levels=covariate.levels)) %>%
  arrange(Covariate)

theme.results <- theme.results %>% 
  mutate(Covariate = factor(Covariate, levels=covariate.levels)) %>%
  arrange(Covariate)

#Add conditional formatting              
boldcells<-c('SVI Score', 'Child Care Establishments', 'Prisons')
overall.results$Covariate<-cell_spec(overall.results$Covariate, 
                      bold=ifelse(overall.results$Covariate %in% boldcells, T, F))

theme.results$Covariate<-cell_spec(theme.results$Covariate, 
                            bold=ifelse(theme.results$Covariate %in% boldcells, T, F))

#Remove line breaks from covariate column
overall.results$Covariate<-gsub('<br />', "", overall.results$Covariate)
theme.results$Covariate<-gsub('<br />', "", theme.results$Covariate)

#Create table column header
names(overall.results)<-c(' ', 'OR (95% CI)', 'p', 'IRR (95% CI)', 'p')
names(theme.results)<-c(' ', rep(c('OR (95% CI)', 'p', 'IRR (95% CI)', 'p'), 4))
#--------------------------------------------------------------------------------
# Final model output tables
#--------------------------------------------------------------------------------
kbl(overall.results, align =c("l",  rep('r', 4)), escape = F) %>%
  kable_classic(full_width=F, font_size = 14) %>%
  column_spec(1, width = '7cm') %>%
  column_spec(c(2, 4), width = '3cm') %>%
  add_header_above(c(" "=1, "Logistic Model[note]" = 2,  
                            "Negative Binomial Count Model[note]" = 2), bold=T) %>%
  pack_rows('Race and Ethnicity (Ref: White, NH)', 2, 5, indent = T) %>%
  pack_rows('SVI Interaction with Race and Ethnicity', 6, 9, indent = T) %>%
  pack_rows('Urban-Rural Category (Ref: Large metro)', 12, 14, indent = T) %>%
  footnote(general=c("OR: odds ratio for having at least one incident shigella case",
                     "IRR: indicence rate ratio of shigella among counties with at least one cases",
                     "SVI: social vulnerability index",
                     "*** p < 0.001, ** p < 0.01, * p < 0.05",
                     'American Indian and Alaska Native category includes both Hispanic and non-Hispanic individuals')) %>%
  add_footnote(c("Adjusted for mean-scaled population.", "Offset by log of population."), 
               notation='number') %>%
  save_kable('charts/incidence/Age-adjusted/hurdle_model_svi_overall.png')


kbl(theme.results, align =c("l",  rep('r', 13)), escape = F) %>%
  kable_classic(full_width=F, font_size = 12) %>%
  column_spec(1, width = '4cm') %>%
  add_header_above(c(" "=1, "Theme 1"=4, "Theme 2"=4, "Theme 3"=4, "Theme 4"=4), bold=T)%>%
  pack_rows('Race and Ethnicity (Ref: White, NH)', 2, 5, indent = T) %>%
  pack_rows('SVI Interaction with Race and Ethnicity', 6, 9, indent = T) %>%
  pack_rows('Urban-Rural Category (Ref: Large metro)', 12, 14, indent = T) %>%
  footnote(general=c("OR: odds ratio for having at least one incident shigella case, adjusted for the mean-scaled population in addition to other covariates in model.",
                     "IRR: indicence rate ratio of shigella among counties with at least one cases, offset by log of population.",
                     "SVI: social vulnerability index",
                     "*** p < 0.001, ** p < 0.01, * p < 0.05",
                     'American Indian and Alaska Native category includes both Hispanic and non-Hispanic individuals')) %>%
  save_kable('charts/incidence/Age-adjusted/hurdle_model_svi_themes.png')

#--------------------------------------------------------------------------------
#Predicted effect of SVI by race
#--------------------------------------------------------------------------------
#Create new data 
newdat<-expand.grid(
          list(rpl=seq(0, .99, by = 0.01), LabelRaceEth=unique(nomis$LabelRaceEth), 
             Pop=mean(nomis$Pop), UrCode='Large metro', 
             ESTAB_ChildCare=median(nomis$ESTAB_ChildCare, na.rm=T),
             Prisons=median(nomis$Prisons), State=NA, County = NA))

newdat <- newdat %>%
  mutate(t1rpl=rpl, t2rpl=rpl, t3rpl=rpl, t4rpl=rpl)
                                       
#Calculate predicted incident rate from all models
Overall<-predict(BestModa, newdata = newdat, type='response', se.fit=T, allow.new.levels = T)
Theme1<-predict(BestModb, newdata = newdat, type='response', se.fit=T, allow.new.levels = T)
Theme2<-predict(BestModc, newdata = newdat, type='response', se.fit=T, allow.new.levels = T)
Theme3<-predict(BestModd, newdata = newdat, type='response', se.fit=T, allow.new.levels = T)
Theme4<-predict(BestMode, newdata = newdat, type='response', se.fit=T, allow.new.levels = T)

#Add predicted incidence rates and standard errors to new dataset
newdat <- newdat %>%
  mutate(Overall.Pred=Overall$fit, Theme1.Pred=Theme1$fit, 
         Theme2.Pred=Theme2$fit, Theme3.Pred=Theme3$fit, Theme4.Pred=Theme4$fit)

newdat$Overall.SE<-Overall$se.fit
newdat$Theme1.SE=Theme1$se.fit
newdat$Theme2.SE=Theme2$se.fit
newdat$Theme3.SE=Theme3$se.fit
newdat$Theme4.SE=Theme4$se.fit

#Pivot to long-form data and calculate lower and upper CI's
newdat <- newdat %>%
  select(LabelRaceEth, rpl, t1rpl:Theme4.SE) %>%
  rename(Overall.SVI= rpl, Theme1.SVI=t1rpl, Theme2.SVI=t2rpl, Theme3.SVI=t3rpl, Theme4.SVI=t4rpl) %>%
  pivot_longer(Overall.SVI:Theme4.SE, names_to = c("Model", ".value"), names_sep = "\\.") %>%
  mutate(Lower.CI = Pred - 1.96*SE, Upper.CI = Pred+1.96*SE, Model = gsub('Theme', 'Theme ', Model))

#Graph predicted response var (incidence rate) from each model
ggplot(newdat, aes(x = SVI, y = Pred)) +
    geom_line(linewidth=1) +
    geom_ribbon(aes(ymin=Lower.CI, ymax=Upper.CI), alpha=0.2) +
    facet_grid(cols=vars(LabelRaceEth), rows=vars(Model))+
    labs(x = "Social Vulnerability Index", 
         y = "Predicted Incidence Rate", 
         caption='Results from an adjusted multilevel hurdle model offset by log of population.') +
    scale_color_nejm()+
    ggtitle('Predicted County-Level Shigella Incidence Rate by Social Vulnerability Index Score')+
    theme_classic()+
    theme(panel.grid.major = element_line(color='lightgray'), 
          legend.position = 'top',
          legend.justification = 'left',
          legend.title = element_blank(), 
          legend.text = element_text(size=8),
          plot.caption = element_text(hjust = 0),
          strip.background = element_rect(fill='black'), 
          strip.text = element_text(color='white'))+
    guides(color = guide_legend(nrow = 1, byrow = TRUE))

ggsave('charts/Incidence/Age-adjusted/predicted_rate_by_svi_race.png', width=8, height=8, units='in')   

#--------------------------------------------------------------------------------
#Extra code
#--------------------------------------------------------------------------------
conditional<-predict(BestModa, newdata = newdat, type='conditional', se.fit=T, allow.new.levels = T)
zprob<-predict(BestModa, newdata = newdat, type='zprob', se.fit=T, allow.new.levels = T)

newdat$Conditional <- conditional$fit
newdat$conditional.min = conditional$fit-1.96*conditional$se.fit
newdat$conditional.max = conditional$fit+1.96*conditional$se.fit

newdat$NonZero <- 1-zprob$fit
newdat$nozprob.min = (1-zprob$fit)-1.96*zprob$se.fit
newdat$nozprob.max = (1-zprob$fit)+1.96*zprob$se.fit

#Maths: can delete later in not needed
cond<-(-11.2 + (1.079*1) + (-.1340*0) + (.9041*0) + (.896*0) + (.6188*0) +
         (.001999*7) + (-.64*1) + (-.3095*0) + (-.6513*0) + (-.3501*0)) 

zero<-0.0228 + 2.626*1 + 2.044*0 + 0.4833*0 + 0.1517*0 + (-1.042*0) + (-.00907*7) +
  0.5639*0 + 0.6788*0 + 1.083*0 + (-0.07563*2) + (-0.00002157*4266.5)

offset<-log(4266.5)

exp(zero)/(1+exp(zero))