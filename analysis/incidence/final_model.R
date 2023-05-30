library('glmmTMB')
library('DHARMa')
library('ggplot2')
library('ggthemes')
library('data.table')
library('tidyverse')
library('kableExtra')

col_palette<-c('black', '#D55E00', '#0072B2', '#009E73')
#--------------------------------------------------------------------------------
#Import county-level dataset 
#--------------------------------------------------------------------------------
folder<-"//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/Final Datasets/"
dat<-read.csv(paste0(folder, 'Final_County_ByRaceEth_2023-05-30.csv'), 
                         colClasses = c('County'='character'))

#formatting variables
racelabels<-c('White, NH','American Indian\n or AK Native', 'Asian or\n Pacific Islander, NH',
              'Black, NH', 'Hispanic*')

setDT(dat)[, c('sviyear','LabelRaceEth', 'UrCode')  := 
                 list(factor(sviyear),
                      
                      factor(raceeth, 
                             levels=c('White-NH', 'AmInd-AKNat', 'Asian, NH and PI', 
                                      'Black-NH', 'Hispanic'),
                             labels=racelabels),
                      
                      factor(UrCode, levels=c(1:6), 
                             labels=c(rep('Large metro', 2), 'Medium metro', 'Small metro', rep('Rural',2))))]

#--------------------------------------------------------------------------------
# Counties excluded due to missing SVI, zero population
#--------------------------------------------------------------------------------
dat[is.na(rpl), (unique(County))]
dat[Pop==0, .N]

#--------------------------------------------------------------------------------
# Distribution of zero cases and var:mean ratio
#--------------------------------------------------------------------------------
dat[!is.na(rpl) & Pop>0, ] %>%
  ggplot(aes(x=Cases, fill=LabelRaceEth)) +
  geom_density( color="#e9ecef", alpha=0.8) +
  facet_grid(rows=vars(sviyear), cols=vars(AgeRange), scales = 'free')+
  ggtitle("Counties by the Number of Shigella Cases")
  

dat[!is.na(rpl) & Pop>0, ] %>% summarise(mean(Cases), 
                                         var(Cases), 
                                         ratio=var(Cases)/mean(Cases))
#--------------------------------------------------------------------------------
#Multivariate analysis
#--------------------------------------------------------------------------------
#Drop counties without SVI data or population and split dataset by age group
dat <- dat[!is.na(rpl) & Pop>0] %>% split(.$AgeRange)

#Hurdle model for Age Group 0 - 4 Years (Null model)
mod0a<-glmmTMB(Cases ~ 1 + offset(log(Pop)), zi=~., data=dat$`0-4`, family=truncated_nbinom2)

#Add state/county random effects
mod0a.1<-update(mod0a, . ~ . + (1|State) + (1|State:County))
anova(mod0a, mod0a.1)

#Evaluate covariates individually
#--------------------------------------------------------------------------------
mod1a.1<-update(mod0a.1, . ~ . + LabelRaceEth, ziformula= ~1)
mod1a.2<-update(mod0a.1, . ~ . + quartile, ziformula= ~1)
mod1a.3<-update(mod0a.1, . ~ . + UrCode, ziformula= ~1)
mod1a.4<-update(mod0a.1, . ~ . , ziformula = ~LabelRaceEth )
mod1a.5<-update(mod0a.1, . ~ . , ziformula = ~rpl)
mod1a.6<-update(mod0a.1, . ~ . , ziformula = ~UrCode)
mod1a.7<-update(mod0a.1, . ~ . , ziformula = ~log(Pop))

anova(mod0a.1, mod1a.1)
anova(mod0a.1, mod1a.2)
anova(mod0a.1, mod1a.3)
anova(mod0a.1, mod1a.4)
anova(mod0a.1, mod1a.5)
anova(mod0a.1, mod1a.6)
anova(mod0a.1, mod1a.7)

#Evaluate sequentially
#--------------------------------------------------------------------------------
mod2a<-update(mod1a.1, . ~ . + rpl)
summary(mod2a)
anova(mod1a.1, mod2a)

mod3a<-update(mod2a, . ~ . + UrCode) #No improvement in fit
summary(mod3a)
anova(mod2a, mod3a)

mod4a<-update(mod2a, . ~ . , ziformula = ~ 1 + log(Pop))
summary(mod4a)
anova(mod2a, mod4a)

mod5a<-update(mod4a, . ~ . , ziformula = ~ 1 + log(Pop) + LabelRaceEth)
summary(mod5a)
anova(mod5a, mod4a)

mod6a<-update(mod5a, . ~ . , ziformula = ~ 1 + log(Pop) + LabelRaceEth + rpl)
summary(mod6a)
anova(mod6a, mod5a)

mod7a<-update(mod6a, . ~ . , ziformula = ~ 1 + log(Pop) + LabelRaceEth + rpl + UrCode)
summary(mod7a)
anova(mod7a, mod6a)


# View AIC for all models
hurdle.0to4.comparison<-anova(mod0a, mod0a.1, mod1a.1, mod1a.2, mod1a.3, 
                              mod1a.4, mod1a.5, mod1a.6, mod1a.7,
                              mod2a, mod3a, mod4a, mod5a, mod6a, mod7a)
hurdle.0to4.comparison[,c(1:5, 8)] %>% arrange(desc(AIC))

# Summary of best fitted model
BestMod<-mod7a
summary(BestMod)
rm(mod0a, mod0a.1, mod1a.1, mod1a.2, mod1a.3, 
   mod1a.4, mod1a.5, mod1a.6, mod1a.7,
   mod2a, mod3a, mod4a, mod5a, mod6a, mod7a)

#--------------------------------------------------------------------------------
# Evaluation of model fit
#--------------------------------------------------------------------------------
simulationOutputa <- simulateResiduals(fittedModel = BestMod, plot=F) 
plot(simulationOutputa)

plotResiduals(simulationOutputa, form=na.omit(dat$`0-4`$LabelRaceEth))
plotResiduals(simulationOutputa, form=na.omit(dat$`0-4`$rpl))
plotResiduals(simulationOutputa, form=na.omit(dat$`0-4`$UrCode))
testDispersion(simulationOutputa = simulationOutput)
testOutliers(simulationOutputa, type='bootstrap')

#--------------------------------------------------------------------------------
# Apply final model to all age groups
#--------------------------------------------------------------------------------
BestModb<-update(BestMod, . ~ ., data=dat$`5-14`)
BestModc<-update(BestMod, . ~ ., data=dat$`15-44`)
BestModd<-update(BestMod, . ~ ., data=dat$`45+`)

simulationOutputb <- simulateResiduals(fittedModel = BestModb, plot=F) 
simulationOutputc <- simulateResiduals(fittedModel = BestModc, plot=F) 
simulationOutputd <- simulateResiduals(fittedModel = BestModd, plot=F) 

plot(simulationOutputb)
plot(simulationOutputc)
plot(simulationOutputd)

#--------------------------------------------------------------------------------
#Extract Fixed Effect Odds Ratios and 95% Confidence Intervals
#--------------------------------------------------------------------------------
mod.list<-list(BestMod, BestModb, BestModc, BestModd)
names(mod.list)<-c('0-4', '5-14', '15-44', '45+')

feffects<-setDT(data.frame())

for (i in 1:length(mod.list)){
  MoA_CI=data.frame(CI=exp(confint(mod.list[[i]], method='Wald'))) 
  MoA_CI$Measure<-ifelse(grepl('cond', row.names(MoA_CI))==T, 'IRR', 'OR')
  MoA_CI$Covariate<-gsub("cond\\.|zi\\.", "", row.names(MoA_CI))
  names(MoA_CI)<-c('Lower.CI', 'Upper.CI', 'Estimate', 'Measure', 'Covariate')
  MoA_CI$AgeGroup<- names(mod.list[i])
  temp<-summary(mod.list[[i]])$coefficients
  sig<-rbind(data.frame(p=temp$cond[,4], Measure='IRR', Covariate=row.names(temp$cond)),
             data.frame(p=temp$zi[, 4],  Measure='OR', Covariate=row.names(temp$zi))) 
  MoA_CI<-merge(MoA_CI, sig, by=c('Covariate', 'Measure'), all.x=T)
  feffects<-rbind(feffects, MoA_CI)
}

#Take inverse of odds ratios for zero component so that we are measuring the odds of 
#having AT LEAST ONE case instead of the odds of having ZERO cases
feffects[Measure=='OR', c('Lower.CI', 'Upper.CI', 'Estimate') := list(1/Upper.CI, 1/Lower.CI, 1/Estimate)]

#--------------------------------------------------------------------------------
#Format variables for tables and charts
#--------------------------------------------------------------------------------
feffects[, c('Covariate', 'AgeGroup', 'Significance', 'Measure') := list(
                  gsub('LabelRaceEth|sviyear|UrCode', "", Covariate),
                  factor(AgeGroup, levels=c('0-4', '5-14', '15-44', '45+')),
                  cut(p, breaks=c(0, 0.001, 0.01, 0.05, 1), labels=c('***', '**', '*', ''), right=F),
                  factor(Measure, levels=c('OR', 'IRR')))]

feffects[, 'Estimate_CI' := paste0(sprintf("%0.2f", Estimate), " (", sprintf("%0.2f", Lower.CI), "-", 
                                   sprintf("%0.2f", Upper.CI), ")")]
                  
##Table of fixed effects: odds ratio or rate ratio (95% CI)**
result.table<-dcast(feffects[grepl('Intercept|Pop', Covariate)==F,], 
                    Covariate + AgeGroup ~ Measure, value.var=c('Estimate_CI', 'Significance'))

result.table$Covariate[result.table$Covariate=='rpl']<-'SVI Score'
result.table$Estimate_CI_IRR[is.na(result.table$Estimate_CI_IRR)]<-'-'
result.table$Significance_IRR<-ifelse(is.na(result.table$Significance_IRR), '-', 
                                      as.character(result.table$Significance_IRR))

result.table$Covariate = cell_spec(result.table$Covariate, 
                                   bold=ifelse(result.table$Covariate=='SVI Score', T, F)) 

result.table$Covariate<-gsub('<br />', "", result.table$Covariate)

names(result.table)<-c(' ', 
                       'AgeGroup', 
                       paste0('OR (95% CI)', footnote_marker_number(1)),
                       paste0('IRR (95% CI)', footnote_marker_number(2)),  
                       ' ', ' ')

#--------------------------------------------------------------------------------
# Final model output table
#--------------------------------------------------------------------------------
kbl(result.table[order(AgeGroup), c(1, 3, 5, 4, 6)], align =c("l",  rep('r', 4)), escape = F)%>%
  kable_classic(full_width=F, font_size = 16) %>%
  column_spec(1, width = '7cm') %>%
  column_spec(c(2, 4), width = '4cm') %>%
  add_header_above(c(" "=1, "Part I: Logistic Model" = 2,  "Part II: Negative Binomial Count Model" = 2), bold=T) %>%
   pack_rows('0-4 Years', 1, 7, label_row_css="background-color: #666; color: #fff;") %>%
    pack_rows('Race and Ethnicity (Ref: White, NH)', 1, 4) %>%
    pack_rows('Urban-Rural Category (Ref: Large metro)', 5, 7) %>%
  pack_rows('5-14 Years', 9, 15, label_row_css="background-color: #666; color: #fff;") %>%
    pack_rows('Race and Ethnicity (Ref: White, NH)', 9, 12) %>%
    pack_rows('Urban-Rural Category (Ref: Large metro)', 13, 15) %>%
  pack_rows('15-44 Years', 17, 23, label_row_css="background-color: #666; color: #fff;") %>%
    pack_rows('Race and Ethnicity (Ref: White, NH)', 17, 20) %>%
    pack_rows('Urban-Rural Category (Ref: Large metro)', 21, 23) %>%
  pack_rows('45+ Years', 25, 31, label_row_css="background-color: #666; color: #fff;") %>%
    pack_rows('Race and Ethnicity (Ref: White, NH)', 25, 28) %>%
    pack_rows('Urban-Rural Category (Ref: Large metro)', 29, 31) %>%
  footnote(general = "OR: odds ratio for having at least one incident Shigella case\nIRR: indicence rate ratio of Shigella cases among counties with one or more cases
           *** p < 0.001, ** p < 0.01, * p < 0.05",
           number = c("Adjusted for log of population.", "Offset by log of population.")) %>%
  save_kable('charts/hurdle_model_results.png')
  
#--------------------------------------------------------------------------------
#Chart of measures of association by race
#--------------------------------------------------------------------------------
feffects %>%
  filter(grepl('Black|American|Asian|Hispanic', Covariate)==T) %>%
      ggplot(aes(y=factor(Covariate)))+
      geom_point(aes(x=Estimate, color=factor(AgeGroup)),  size=2, alpha=0.8)+
      geom_linerange(aes(xmin=Lower.CI, xmax=Upper.CI, color=factor(AgeGroup)), linewidth=0.8) +
      geom_vline(xintercept = 1, linetype='dashed') +
      #facet_wrap(~Measure , ncol=4, scales = 'free')+
      facet_grid(cols=vars(Measure), rows=vars(AgeGroup))+
      ggtitle("Adjusted Hurdle Model Measures of Association")+
      scale_colour_manual(values=col_palette)+
      labs(color = "Age Group")+
      theme_light()+
      theme(axis.title=element_blank(), 
            strip.background =element_rect(fill='gray25'),
            legend.position = 'none')
ggsave('charts/hurdle_model_chart.png', width=8, height=6, units='in')    

#--------------------------------------------------------------------------------
#Maps of counties by 
#--------------------------------------------------------------------------------
zp1 <- predict(BestMod, type="zprob")
cm1 <- predict(BestMod, type="conditional")
mu1 <- predict(BestMod, type="response")

dat[, mean(Cases>0), by='AgeGroup']

dat[Cases>0, mean(Cases), by='AgeGroup']
mean(cm1 / (1 - exp(-cm1)))
