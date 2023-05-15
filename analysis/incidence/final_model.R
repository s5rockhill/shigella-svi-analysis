library('glmmTMB')
library('DHARMa')
library('ggplot2')
library('ggthemes')
library('data.table')
library('tidyverse')
library('kableExtra')

col_palette<-c('black', '#D55E00', '#0072B2', '#009E73')
output.folder<-'//cdc.gov/project/ATS_GIS_Store12/Shigella_SVI/Preliminary Results/Charts/'
#--------------------------------------------------------------------------------
#Import county-level dataset 
#--------------------------------------------------------------------------------
setwd("//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/Final Datasets/")
cntydat<-read.csv('Final_County_ByRaceEth_2023-02-13.csv', colClasses = c('County'='character'))

#formatting variables
setDT(cntydat)[, c('sviyear','LabelRaceEth', 'UrCode')  := 
                 list(factor(sviyear),
                      factor(raceeth, levels=c('nh-white', 'nh-black', 'nh-amerin', 'nh-asian', 'hisp'),
                                      labels=c('White, NH',
                                               'Black, NH',
                                               'American Indian\n or AK Native, NH', 
                                               'Asian or\n Pacific Islander, NH', 
                                               'Hispanic')),
                      factor(UrCode, levels=c(1:6), 
                                     labels=c(rep('Large metro', 2), 'Medium metro', 'Small metro', rep('Rural',2))))]

#--------------------------------------------------------------------------------
#Summary statistics of rates/proportions by race/svi quartile
#--------------------------------------------------------------------------------
sumstats<-data.frame()
for (i in c('LabelRaceEth', 'quartile', 'AgeGroup')) {
      temp<-
        cntydat[Pop>0, list(`One or more cases (%)`= sum(Cases>0)/.N*100,
                             Minimum=min(Cases/Pop*100000),
                              `25th percentile`=quantile(Cases/Pop*100000, 0.25),
                               Median=quantile(Cases/Pop*100000, 0.50),
                               Mean=mean(Cases/Pop*100000, na.rm=T),
                              `75th percentile`=quantile(Cases/Pop*100000, 0.75),
                              `95th percentile`=quantile(Cases/Pop*100000, .95),
                              Maximum=max(Cases/Pop*100000)), by=eval(i)]
      names(temp)[1]<-'Group'
      sumstats<-rbind(sumstats, temp)
}

sumstats[, 2:9]<-sumstats[, lapply(.SD, sprintf, fmt="%0.1f"), .SDcols=names(sumstats)[-1]]
sumstats[,'Group' := factor(Group, levels=c('White, NH', 'Black, NH', 'American Indian\n or AK Native, NH',
                                            'Asian or\n Pacific Islander, NH', 'Hispanic', 1:4, '0-4',
                                            '5-14', '15-44', '45+'))]
sumstats<-sumstats[order(Group),]
names(sumstats)[1]<-''


kbl(na.omit(sumstats), align =c("l",  rep('r', 8))) %>%
  kable_classic(full_width=F, font_size = 12) %>%
  add_header_above(c(" "=2, "Rate per 100,000" = 7), bold=T) %>%
  pack_rows('Race and Ethnicity', 1, 5) %>%
  pack_rows('SVI Quartile', 6, 9) %>%
  pack_rows('Age Group', 10, 13) %>%
save_kable(paste0(output.folder, 'summary_stat_table1.png'))

#--------------------------------------------------------------------------------
#Data visualization
#--------------------------------------------------------------------------------
cntydat[Cases>0, list(Rate=sum(Cases)/sum(Pop)*100000), by=list(County, LabelRaceEth, quartile)] %>%
  ggplot(aes(x=quartile, y=Rate))+
  facet_grid(cols=vars(LabelRaceEth), scales='free')+
  stat_summary(fun.data=mean_cl_boot,size=.7, color='#0072B2')+
  coord_trans(y="log10")+
  theme_light()+
  labs(title = 'Average Shigella Incidence Rate by Social Vulnerability Index Quartile', 
       subtitle= 'among counties with one or more cases') +
  ylab('Log(Rate per 100K Residents)')+
  xlab('Social Vulnerability Index Quartile')+
  theme(legend.position = 'none', 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.text.x=element_text(color='black', size=10, face="bold"))

ggsave(paste0(output.folder, 'Incidence_by_Race_and_Quartile.png'), width=8, height=5, units='in')
#--------------------------------------------------------------------------------
#Multivariate analysis
#--------------------------------------------------------------------------------
#Drop counties without SVI data or population and split dataset by age group
cntydat <- cntydat[!is.na(RPL_THEMES) & Pop>0] %>% split(.$AgeGroup)

#Hurdle model for Age Group 0 - 4 Years (Null model)
mod0a<-glmmTMB(Cases ~ 1 + offset(log(Pop)), zi=~., data=cntydat$`0-4`, family=truncated_nbinom2)

#Add state/county random effects
mod0a.1<-update(mod0a, . ~ . + (1|State) + (1|State:County))
anova(mod0a, mod0a.1)

#Evaluate covariates individually
mod1a.1<-update(mod0a.1, . ~ . + LabelRaceEth)
mod1a.2<-update(mod0a.1, . ~ . + quartile)
mod1a.3<-update(mod0a.1, . ~ . + UrCode)
mod1a.4<-update(mod0a.1, . ~ . , ziformula = ~. + log(Pop))

anova(mod0a.1, mod1a.1)
anova(mod0a.1, mod1a.2)
anova(mod0a.1, mod1a.3)
anova(mod0a.1, mod1a.4)

#Evaluate sequentially
mod2a<-update(mod1a.1, . ~ . + quartile)
anova(mod1a.1, mod2a)

mod3a<-update(mod2a, . ~ . + UrCode)
anova(mod2a, mod3a)

mod4a<-update(mod3a, . ~ . , ziformula = ~ . + log(Pop))
anova(mod3a, mod4a)

# View AIC for all models
hurdle.0to4.comparison<-anova(mod0a, mod0a.1, mod1a.1, mod1a.2, mod1a.3, mod1a.4, mod2a, mod3a, mod4a)
hurdle.0to4.comparison[,c(1:5, 8)] %>% arrange(desc(AIC))

# Summary of best fitted model
summary(mod4a)
rm(mod0a, mod0a.1, mod1a.1, mod1a.2, mod1a.3, mod1a.4, mod2a, mod3a)

#--------------------------------------------------------------------------------
# Evaluation of model fit
#--------------------------------------------------------------------------------
simulationOutputa <- simulateResiduals(fittedModel = mod4a, plot=F) 
plot(simulationOutputa)

plotResiduals(simulationOutputa, form=na.omit(cntydat$`0-4`$LabelRaceEth))
plotResiduals(simulationOutputa, form=na.omit(cntydat$`0-4`$quartile))
plotResiduals(simulationOutputa, form=na.omit(cntydat$`0-4`$UrCode))
testDispersion(simulationOutputa = simulationOutput)
testOutliers(simulationOutputa, type='bootstrap')

#--------------------------------------------------------------------------------
# Apply final model to all age groups
#--------------------------------------------------------------------------------
mod4b<-update(mod4a, . ~ ., data=cntydat$`5-14`)
mod4c<-update(mod4a, . ~ ., data=cntydat$`15-44`)
mod4d<-update(mod4a, . ~ ., data=cntydat$`45+`)

simulationOutputb <- simulateResiduals(fittedModel = mod4b, plot=F) 
simulationOutputc <- simulateResiduals(fittedModel = mod4c, plot=F) 
simulationOutputd <- simulateResiduals(fittedModel = mod4d, plot=F) 

plot(simulationOutputb)
plot(simulationOutputc)
plot(simulationOutputd)

#--------------------------------------------------------------------------------
#Extract Fixed Effect Odds Ratios and 95% Confidence Intervals
#--------------------------------------------------------------------------------
mod.list<-list(mod4a, mod4b, mod4c, mod4d)
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

result.table$Covariate[result.table$Covariate=='quartile']<-'SVI Quartile'
names(result.table)<-c(' ', 
                       'AgeGroup', 
                       paste0('OR (95% CI)', footnote_marker_number(1)),
                       paste0('IRR (95% CI)', footnote_marker_number(2)),  
                       ' ', ' ')

kbl(result.table[order(AgeGroup), c(1, 3, 5, 4, 6)], align =c("l",  rep('r', 4)), escape = F)%>%
  kable_classic(full_width=F, font_size = 12) %>%
  add_header_above(c(" "=1, "Part I: Logistic Model" = 2,  "Part II: Negative Binomial Count Model" = 2), bold=T) %>%
   pack_rows('0-4 Years', 1, 8, label_row_css="background-color: #666; color: #fff;") %>%
    pack_rows('Race and Ethnicity (Ref: White, NH)', 1, 4) %>%
    pack_rows('Urban-Rural Category (Ref: Large metro)', 5, 7) %>%
  pack_rows('5-14 Years', 9, 16, label_row_css="background-color: #666; color: #fff;") %>%
    pack_rows('Race and Ethnicity (Ref: White, NH)', 9, 12) %>%
    pack_rows('Urban-Rural Category (Ref: Large metro)', 13, 15) %>%
  pack_rows('15-44 Years', 17, 24, label_row_css="background-color: #666; color: #fff;") %>%
    pack_rows('Race and Ethnicity (Ref: White, NH)', 17, 20) %>%
    pack_rows('Urban-Rural Category (Ref: Large metro)', 21, 23) %>%
  pack_rows('45+ Years', 25, 32, label_row_css="background-color: #666; color: #fff;") %>%
    pack_rows('Race and Ethnicity (Ref: White, NH)', 25, 28) %>%
    pack_rows('Urban-Rural Category (Ref: Large metro)', 29, 31) %>%
  footnote(general = "OR: odds ratio for having at least one incident Shigella case\nIRR: indicence rate ratio of Shigella cases among counties with one or more cases
           *** p < 0.001, ** p < 0.01, * p < 0.05",
           number = c("Adjusted for log of population.", "Offset by log of population.")) %>%
  save_kable(paste0(output.folder, 'hurdle_model_results.png'))
  
#--------------------------------------------------------------------------------
#Chart of measures of association by race
#--------------------------------------------------------------------------------
feffects %>%
  filter(grepl('Black|American|Asian|Hispanic', Covariate)==T) %>%
      ggplot(aes(y=factor(Covariate)))+
      geom_point(aes(x=Estimate, color=factor(AgeGroup)),  size=2, alpha=0.8)+
      geom_linerange(aes(xmin=Lower.CI, xmax=Upper.CI, color=factor(AgeGroup)), linewidth=0.5) +
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
ggsave(paste0(output.folder, 'hurdle_model_chart.png'), width=8, height=6, units='in')    

#--------------------------------------------------------------------------------
#Maps of counties by 
#--------------------------------------------------------------------------------
zp1 <- predict(mod7a, type="zprob")
cm1 <- predict(mod7a, type="conditional")
mu1 <- predict(mod7a, type="response")

cntydat[, mean(Cases>0), by='AgeGroup']

cntydat[Cases>0, mean(Cases), by='AgeGroup']
mean(cm1 / (1 - exp(-cm1)))
