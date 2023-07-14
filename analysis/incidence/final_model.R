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
dat<-read.csv(paste0(folder, 'Final_County_ByRaceEth_2023-07-03.csv'), colClasses = c('County'='character'))

#formatting variables
racelabels<-c('White, NH','American Indian\n or AK Native', 'Asian or\n Pacific Islander, NH',
              'Black, NH', 'Hispanic')

dat <- dat %>%
  mutate(State= substr(County, 1, 2),
          LabelRaceEth = factor(raceeth, 
                                levels=c('White-NH', 'AmInd-AKNat','Asian, NH and PI', 
                                         'Black-NH', 'Hispanic'), labels=racelabels),
          UrCode= factor(UrCode, levels=c(1:6), 
                    labels=c(rep('Large metro', 2), 'Medium metro', 'Small metro', rep('Rural',2))))

#--------------------------------------------------------------------------------
# Counties excluded due to missing SVI, zero population
#--------------------------------------------------------------------------------
dat %>% filter(is.na(rpl)) %>% summarise(n_distinct(County))
dat%>% filter(Pop==0) %>% count()

#--------------------------------------------------------------------------------
# Distribution of zero cases and var:mean ratio
#--------------------------------------------------------------------------------
dat[!is.na(rpl) & Pop>0, ] %>%
  ggplot(aes(x=Cases, fill=LabelRaceEth)) +
  geom_density( color="#e9ecef", alpha=0.8) +
  facet_grid(rows=vars(sviyear), cols=vars(AgeRange), scales = 'free')+
  ggtitle("Counties by the Number of Shigella Cases")

dat[!is.na(rpl) & Pop>0, ] %>% summarise(mean(Cases), var(Cases), ratio=var(Cases)/mean(Cases))

#--------------------------------------------------------------------------------
#Multivariate analysis
#--------------------------------------------------------------------------------
#Drop counties without SVI data or population and split dataset by age group
overall <- dat %>%
  filter(Pop > 0) %>%
  select(Cases, Pop, State, County, LabelRaceEth, rpl, UrCode, ESTAB_ChildCare, 
         Prisons, Capacity, PopDensity, Pop, County, sviyear, AgeRange) %>%
  na.omit %>%
  split(.$AgeRange)

# Develop NULL MODELS comparing random effects in both conditional and binomial
# portions of the model (starting with Age Group 0-4)
#--------------------------------------------------------------------------------
mod0a<-glmmTMB(Cases ~ 1 + offset(log(Pop)), zi=~1, data=overall$`0-4`, family=truncated_nbinom2)

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
mod1a.6<-update(mod0a.2, . ~ . , ziformula = ~.+LabelRaceEth )
mod1a.7<-update(mod0a.2, . ~ . , ziformula = ~.+rpl)
mod1a.8<-update(mod0a.2, . ~ . , ziformula = ~.+UrCode)
mod1a.9<-update(mod0a.2, . ~ . , ziformula = ~.+ESTAB_ChildCare)
mod1a.10<-update(mod0a.2, . ~ . , ziformula = ~.+PopDensity)
mod1a.11<-update(mod0a.2, . ~ . , ziformula = ~.+scale(Pop, scale=F))

anova(mod0a.2, mod1a.1)
anova(mod0a.2, mod1a.2)
anova(mod0a.2, mod1a.3) #No improvement in fit
anova(mod0a.2, mod1a.4) #No improvement in fit
anova(mod0a.2, mod1a.5) #No improvement in fit
anova(mod0a.2, mod1a.6)
anova(mod0a.2, mod1a.7)
anova(mod0a.2, mod1a.8)
anova(mod0a.2, mod1a.9)
anova(mod0a.2, mod1a.10)
anova(mod0a.2, mod1a.11)

#Evaluate sequentially (include covariates with univariate association only)
#--------------------------------------------------------------------------------
mod2a<-update(mod1a.1, . ~ . + rpl)
summary(mod2a)
anova(mod1a.2, mod2a)

mod3a<-update(mod2a, . ~ . +rpl*LabelRaceEth) 
summary(mod3a)
anova(mod2a, mod3a) # No improvement in fit

mod4a<-update(mod2a, . ~ . , ziformula = ~(1|State) + (1|State:County) + LabelRaceEth )
summary(mod4a)
anova(mod2a, mod4a)

mod5a<-update(mod4a, . ~ . , ziformula = ~.)
summary(mod5a)
anova(mod5a, mod4a)

mod6a<-update(mod5a, . ~ . , ziformula = ~ .+ UrCode)
summary(mod6a)
anova(mod6a, mod5a)

mod7a<-update(mod6a, . ~ . , ziformula = ~ .+ UrCode + ESTAB_ChildCare)
summary(mod7a)
anova(mod7a, mod6a)

mod8a<-update(mod7a, . ~ . , ziformula = ~ .+ UrCode + ESTAB_ChildCare + PopDensity)
summary(mod8a)
anova(mod8a, mod7a) # No significant improvement in fit

mod9a<-update(mod7a, . ~ . , ziformula = ~ .+ UrCode + ESTAB_ChildCare +  scale(Pop, scale=F))
summary(mod9a)
anova(mod9a, mod7a)

mod10a<-update(mod7a, . ~ . , ziformula = ~ .+ UrCode + ESTAB_ChildCare +  scale(Pop, scale=F) + PopDensity)
summary(mod10a)
anova(mod10a, mod8a)
anova(mod10a, mod9a)

# View AIC for all models
hurdle.0to4.comparison<-anova(mod0a, mod0a.1, mod1a.1, mod1a.2, mod1a.3, mod1a.4, mod1a.5, mod1a.6, 
                              mod1a.7, mod2a, mod3a, mod4a, mod5a, mod6a, mod7a, mod8a, mod9a, mod10a)
hurdle.0to4.comparison[,c(1:5, 8)] %>% arrange(desc(AIC))

# Summary of best fitted model
BestModa<-mod10a
summary(BestModa)
rm(mod0a, mod0a.1, mod0a.2, mod1a.1, mod1a.2, mod1a.3, mod1a.4, mod1a.5, mod1a.6, 
   mod1a.7, mod1a.8, mod1a.9, mod1a.10, mod1a.11, mod2a, mod3a, mod4a, mod5a, 
   mod6a, mod7a, mod8a, mod9a, mod10a)

#--------------------------------------------------------------------------------
# Evaluation of model fit
#--------------------------------------------------------------------------------
simulationOutputa <- simulateResiduals(fittedModel = BestModa, plot=F) 
plot(simulationOutputa)

plotResiduals(simulationOutputa, form=na.omit(overall$`0-4`$LabelRaceEth))
plotResiduals(simulationOutputa, form=na.omit(overall$`0-4`$rpl))
plotResiduals(simulationOutputa, form=na.omit(overall$`0-4`$UrCode))
testDispersion(simulationOutput = simulationOutputa)
testOutliers(simulationOutputa, type='bootstrap')

#--------------------------------------------------------------------------------
# Apply final model to all age groups
#--------------------------------------------------------------------------------
BestModb<-update(BestModa, . ~ ., data=overall$`5-14`)
BestModc<-update(BestModa, . ~ ., data=overall$`15-44`)
BestModd<-update(BestModa, . ~ ., data=overall$`45+`)

simulationOutputb <- simulateResiduals(fittedModel = BestModb, plot=F) 
simulationOutputc <- simulateResiduals(fittedModel = BestModc, plot=F) 
simulationOutputd <- simulateResiduals(fittedModel = BestModd, plot=F) 

plot(simulationOutputb, main='5-14 Years')
plot(simulationOutputc, main='15-44 Years')
plot(simulationOutputd, main='45+ Years')

#--------------------------------------------------------------------------------
# Evaluate SVI sub-themes #1: SES Status
#--------------------------------------------------------------------------------
#Drop counties without SVI data or population and split dataset by age group
t1dat <- dat %>%
  filter(Pop > 0) %>%
  select(Cases, Pop, State, County, LabelRaceEth, t1rpl, UrCode, ESTAB_ChildCare, 
         PopDensity, Pop, County, sviyear, AgeRange) %>%
  na.omit %>%
  split(.$AgeRange)

Theme1a<-update(BestModa, . ~ . -rpl +  t1rpl , data=t1dat$`0-4`)
Theme1b<-update(BestModa, . ~ . -rpl +  t1rpl , data=t1dat$`5-14`)
Theme1c<-update(BestModa, . ~ . -rpl +  t1rpl , data=t1dat$`15-44`)
Theme1d<-update(BestModa, . ~ . -rpl +  t1rpl , data=t1dat$`45+`)

#--------------------------------------------------------------------------------
# Evaluate SVI sub-themes #2: Household Composition and Disability
#--------------------------------------------------------------------------------
#Drop counties without SVI data or population and split dataset by age group
t2dat <- dat %>%
  filter(Pop > 0) %>%
  select(Cases, Pop, State, County, LabelRaceEth, t2rpl, UrCode, ESTAB_ChildCare, 
         PopDensity, Pop, County, sviyear, AgeRange) %>%
  na.omit %>%
  split(.$AgeRange)

Theme2a<-update(BestModa, . ~ . -rpl +  t2rpl , data=t2dat$`0-4`)
Theme2b<-update(BestModa, . ~ . -rpl +  t2rpl , data=t2dat$`5-14`)
Theme2c<-update(BestModa, . ~ . -rpl +  t2rpl , data=t2dat$`15-44`)
Theme2d<-update(BestModa, . ~ . -rpl +  t2rpl , data=t2dat$`45+`)

#--------------------------------------------------------------------------------
# Evaluate SVI sub-themes #3: Minority Status and Language
#--------------------------------------------------------------------------------
#Drop counties without SVI data or population and split dataset by age group
t3dat <- dat %>%
  filter(Pop > 0) %>%
  select(Cases, Pop, State, County, LabelRaceEth, t3rpl, UrCode, ESTAB_ChildCare, 
         PopDensity, Pop, County, sviyear, AgeRange) %>%
  na.omit %>%
  split(.$AgeRange)

Theme3a<-update(BestModa, . ~ . -rpl +  t3rpl , data=t3dat$`0-4`)
Theme3b<-update(BestModa, . ~ . -rpl +  t3rpl , data=t3dat$`5-14`)
Theme3c<-update(BestModa, . ~ . -rpl +  t3rpl , data=t3dat$`15-44`)
Theme3d<-update(BestModa, . ~ . -rpl +  t3rpl , data=t3dat$`45+`)

#--------------------------------------------------------------------------------
# Evaluate SVI sub-themes #4: Housing Type and Transportation
#--------------------------------------------------------------------------------
#Drop counties without SVI data or population and split dataset by age group
t4dat <- dat %>%
  filter(Pop > 0) %>%
  select(Cases, Pop, State, County, LabelRaceEth, t4rpl, UrCode, ESTAB_ChildCare, 
         PopDensity, Pop, County, sviyear, AgeRange) %>%
  na.omit %>%
  split(.$AgeRange)

Theme4a<-update(BestModa, . ~ . -rpl +  t4rpl , data=t4dat$`0-4`)
Theme4b<-update(BestModa, . ~ . -rpl +  t4rpl , data=t4dat$`5-14`)
Theme4c<-update(BestModa, . ~ . -rpl +  t4rpl , data=t4dat$`15-44`)
Theme4d<-update(BestModa, . ~ . -rpl +  t4rpl , data=t4dat$`45+`)

#--------------------------------------------------------------------------------
#Extract Fixed Effect Odds Ratios and 95% Confidence Intervals
#--------------------------------------------------------------------------------
#Write a function to extract fixed effects from list of models

extract_feffects<-function(x) {
  feffects<-data.frame()
  for (i in 1:length(x)){
    MoA_CI=data.frame(CI=exp(confint(x[[i]], method='Wald'))) 
    MoA_CI$Measure<-ifelse(grepl('cond', row.names(MoA_CI))==T, 'IRR', 'OR')
    MoA_CI$Covariate<-gsub("cond\\.|zi\\.", "", row.names(MoA_CI))
    names(MoA_CI)<-c('Lower.CI', 'Upper.CI', 'Estimate', 'Measure', 'Covariate')
    MoA_CI$Theme<- names(x[i])
    temp<-summary(x[[i]])$coefficients
    sig<-rbind(data.frame(p=temp$cond[,4], Measure='IRR', Covariate=row.names(temp$cond)),
               data.frame(p=temp$zi[, 4],  Measure='OR', Covariate=row.names(temp$zi))) 
    MoA_CI<-merge(MoA_CI, sig, by=c('Covariate', 'Measure'), all.x=T)
    feffects<-rbind(feffects, MoA_CI)
  }
  return(feffects)
}

#Add all models to a list
mod.list<-list(
      '0-4'=  list(Overall=BestModa, Theme1=Theme1a, Theme2=Theme2a, Theme3=Theme3a, Theme4=Theme4a),
      '5-14'= list(Overall=BestModb, Theme1=Theme1b, Theme2=Theme2b, Theme3=Theme3b, Theme4=Theme4b),
      '15-44'=list(Overall=BestModc, Theme1=Theme1c, Theme2=Theme2c, Theme3=Theme3c, Theme4=Theme4c),
      '45+'=  list(Overall=BestModd, Theme1=Theme1d, Theme2=Theme2d, Theme3=Theme3d, Theme4=Theme4d)
)

#Iterate through list of models and extract fixed effects for each age group and SVI theme
results<-data.frame()
for(i in 1:length(mod.list)) {
  temp<- extract_feffects(mod.list[[i]])
  temp$AgeGroup <- names(mod.list[i])
  results<-rbind(results, temp)
}

#Take inverse of odds ratios for zero component so that we are measuring the odds of 
#having AT LEAST ONE case instead of the odds of having ZERO cases
results<-results %>% 
  mutate(across(c(Lower.CI, Upper.CI, Estimate), function(x) ifelse(Measure=='OR', 1/x, x)))
  
#--------------------------------------------------------------------------------
#Format variables for tables and charts
#--------------------------------------------------------------------------------
results <- results %>%
  mutate(Covariate = gsub('LabelRaceEth|UrCode', "", Covariate),
         Covariate = gsub('.*rpl', "SVI Score", Covariate),
         Covariate = case_when(Covariate == 'ESTAB_ChildCare' ~ "Child Care Establishments",
                               Covariate == 'PopDensity' ~ "Population Density",
                               .default = as.character(Covariate)),
         Theme=gsub('Theme', 'Theme ', Theme),
         Significance = as.character(
                        cut(p, 
                            breaks=c(0, 0.001, 0.01, 0.05, 1), 
                            labels=c('***', '**', '*', ''), right=F)),
         Measure=factor(Measure, levels=c('OR', 'IRR')),
         Estimate_CI = paste0(sprintf("%0.2f", Estimate), 
                              " (", sprintf("%0.2f", Lower.CI), 
                              "-", sprintf("%0.2f", Upper.CI), ")"))

# Keep only "interesting" parameters and pivot wider
result.table <- results %>%
  filter(grepl('Intercept|scale', Covariate)==F) %>%
  pivot_wider(id_cols = c(Covariate, Theme, AgeGroup), 
              names_from=Measure, 
              values_from = c(Estimate_CI, Significance),
              names_vary = "slowest",
              values_fill = '-')

#Factor "Covariate" column so table is sorted properly
result.table <- result.table %>%
  mutate(Covariate = factor(Covariate, 
                            levels=c('American Indian\n or AK Native', 
                                     'Asian or\n Pacific Islander, NH',
                                     'Black, NH',
                                     'Hispanic*',
                                     'SVI Score',
                                     'Medium metro',
                                     'Small metro',
                                     'Rural',
                                     'Child Care Establishments',
                                     'Population Density'))) %>%
  arrange(AgeGroup, Theme, Covariate)

#Add formatting              
result.table$Covariate<-cell_spec(result.table$Covariate, 
                      bold=ifelse(grepl('SVI|Child|Density', result.table$Covariate)==T, T, F))

result.table$Covariate<-gsub('<br />', "", result.table$Covariate)

names(result.table)<-c(' ', 'Theme', 'AgeGroup', 'OR (95% CI)', 'p', 'IRR (95% CI)', 'p')

#--------------------------------------------------------------------------------
# Final model output table
#--------------------------------------------------------------------------------
kbl(result.table[result.table$AgeGroup=='5-14', c(1, 4:7)],
          align =c("l",  rep('r', 4)), escape = F) %>%
  kable_classic(full_width=F, font_size = 14) %>%
  column_spec(1, width = '7cm') %>%
  column_spec(c(2, 4), width = '4cm') %>%
  add_header_above(c(" "=1, "Part I: Logistic Model [note]" = 2,  
                            "Part II: Negative Binomial Count Model [note]" = 2), bold=T) %>%
   pack_rows('Overall SVI', 1, 10, 
             label_row_css="background-color: #666; color: #fff;", indent=F) %>%
      pack_rows('Race and Ethnicity (Ref: White, NH)', 1, 4, indent = T) %>%
      pack_rows('Urban-Rural Category (Ref: Large metro)', 6, 8, indent = T) %>%
  pack_rows('Theme 1 SVI Score: Socioeconomic Status', 11, 20, 
            label_row_css="background-color: #666; color: #fff;", indent=F) %>%
      pack_rows('Race and Ethnicity (Ref: White, NH)', 11, 14) %>%
      pack_rows('Urban-Rural Category (Ref: Large metro)', 16, 18) %>%
  pack_rows('Theme 2 SVI Score: Household Composition and Disability', 21, 30, 
            label_row_css="background-color: #666; color: #fff;", indent=F) %>%
    pack_rows('Race and Ethnicity (Ref: White, NH)', 21, 24) %>%
    pack_rows('Urban-Rural Category (Ref: Large metro)', 26, 28) %>%
  pack_rows('Theme 3 SVI Score: Minority Status and Language', 31, 40, 
            label_row_css="background-color: #666; color: #fff;", indent=F) %>%
    pack_rows('Race and Ethnicity (Ref: White, NH)', 31, 34) %>%
    pack_rows('Urban-Rural Category (Ref: Large metro)', 36, 38) %>%
  pack_rows('Theme 4 SVI Score: Housing Type and Transportation', 41, 50, 
            label_row_css="background-color: #666; color: #fff;", indent=F) %>%
    pack_rows('Race and Ethnicity (Ref: White, NH)', 41, 44) %>%
    pack_rows('Urban-Rural Category (Ref: Large metro)', 46, 48) %>%
  footnote(general=c("OR: odds ratio for having at least one incident Shigella case",
           "IRR: indicence rate ratio of Shigella cases among counties with one or more cases",
           "SVI: social vulnerability index",
           "*** p < 0.001, ** p < 0.01, * p < 0.05",
           '* Excludes Hispanic American Indian and Alaska Native individuals')) %>%
  add_footnote(c("Adjusted for mean-scaled population.", 
              "Offset by log of population."), 
              notation='number') %>%
  save_kable('charts/hurdle_model_results_age0to4.png')
  
#--------------------------------------------------------------------------------
#Chart of adjusted OR and IRR by race
#--------------------------------------------------------------------------------
results %>%
  filter(grepl('Black|American|Asian|Hispanic', Covariate)==T &
         AgeGroup=='15-44') %>%
      ggplot(aes(y=factor(Covariate)))+
      geom_point(aes(x=Estimate, color=factor(Theme)),  size=3.5)+
      geom_linerange(aes(xmin=Lower.CI, xmax=Upper.CI, color=factor(Theme)), linewidth=1.25) +
      geom_vline(xintercept = 1, linetype='dashed', linewidth=1) +
      #facet_wrap(~Measure , ncol=4, scales = 'free')+
      facet_grid(cols=vars(Measure), rows=vars(Theme), scales = 'free')+
      ggtitle("Adjusted Association Between Race and Ethnicity and Shigellosis",
              subtitle = 'Results from Hurdle Model for Ages 15 through 44 Years')+
      scale_color_nejm()+
      labs(color = "SVI Theme")+
      theme_light()+
      theme(axis.title=element_blank(), 
            strip.background =element_rect(fill='gray25'),
            legend.position = 'none')
ggsave('charts/hurdle_model_chart_age5to14.png', width=8, height=6, units='in')   

#--------------------------------------------------------------------------------
#Predicted effect of SVI by race
#--------------------------------------------------------------------------------
newdat<-expand.grid(list(rpl=seq(0, .99, by = 0.01) , 
                         LabelRaceEth='Black, NH', 
                         Pop=median(dat$Pop),
                         UrCode='Large metro', 
                         ESTAB_ChildCare=median(dat$ESTAB_ChildCare, na.rm=T),
                         PopDensity=median(dat$PopDensity),
                         State=NA, 
                         County = NA))
                                       
newdat$pred<-predict(BestModa, newdata = newdat, type='response', allow.new.levels = T)

ggplot(newdat, aes(x = rpl, y = pred)) +
  geom_line() +
  scale_y_continuous(breaks=seq(0, 0.15, by=0.03)) +
  labs(x = "Social Vulnerability Index", 
       y = "Predicted Shigella Cases",
       color = 'Rural-Urban Commuting Area Category') +
  scale_color_brewer(palette = 'Dark2')+
  ggtitle('Predicted Probability of Incident Shigella Cases')+
  theme_classic2() +
  theme(panel.grid.major = element_line(color='lightgray'), 
        legend.position = c(0.25, 0.9),
        legend.title = element_text(size=8), 
        legend.background = element_rect(color='black'),
        legend.text = element_text(size=8))+
  guides(color = guide_legend(nrow = 2, byrow = TRUE))
#--------------------------------------------------------------------------------
#Maps of counties by 
#--------------------------------------------------------------------------------
zp1 <- predict(BestMod, type="zprob")
cm1 <- predict(BestMod, type="conditional")
mu1 <- predict(BestMod, type="response")

dat[, mean(Cases>0), by='AgeGroup']

dat[Cases>0, mean(Cases), by='AgeGroup']
mean(cm1 / (1 - exp(-cm1)))
