library('glmmTMB')
library('MASS')
library('bbmle')
library('ggplot2')
library('ggthemes')
library('DHARMa')

#-------------------------------------------------------------------------------
#Because census tract ID's are reused between versions, we need to qualify
#the census tract id number by year
#-------------------------------------------------------------------------------
fin[,GEOID := ifelse(sviyear==2000, paste0('2000', GEOID), paste0('2010', GEOID))]

#-------------------------------------------------------------------------------
#Data quality checks
#-------------------------------------------------------------------------------
#Records with more cases in a specific sex/race/ethnicity group than population 
fin[Cases>Pop, .N, by=c('Race', 'Ethnicity')]

#-------------------------------------------------------------------------------
# Visualize distribution of number of cases for census tract for all race groups
#-------------------------------------------------------------------------------
#This plot shows distribution of tracts by age-adjusted cases for all years combined 
ggplot(fin[Race!='All', list(Cases=sum(Cases)), by=list(GEOID, Race)], 
       aes(x=Cases)) +
  facet_grid(.~Race)+
  geom_histogram(position="identity", bins=10, fill='red', color='black')+
  theme_light()

ggplot(fin[Race=='All', list(Cases=sum(Cases)), by=list(GEOID, Ethnicity)], 
       aes(x=Cases)) +
  facet_grid(.~Ethnicity)+
  geom_histogram(position="identity", bins=10, fill='red', color='black')+
  theme_light()

#For any given race/ethnicity (except for unknown), less than 1% of tracts have 1+ cases
fin[, sum(Cases==0)/.N, by=list(sviyear, Race, Ethnicity)]

#-------------------------------------------------------------------------------
# Ratio of variance to mean of cases by race/ethnicity and year
#-------------------------------------------------------------------------------
dcast(Race + Ethnicity ~ sviyear, value.var = 'Ratio',
      data=fin[, list(Ratio=var(Cases)/mean(Cases)), by = c('Race', 'Ethnicity', 'sviyear')])

#-------------------------------------------------------------------------------
# Part 1: Are there residual effects of race and ethnicity on the incidence of 
# Shigella after controlling for SVI?
#-------------------------------------------------------------------------------
#Factor covariates and set desired reference category
fin[, Race := factor(Race, levels=c('W', 'B', 'A', 'I', 'P', 'M', 'O'))]
fin[, Ethnicity := factor(Ethnicity, levels=c('N', 'H'))]
fin[, UrCode := factor(UrCode, levels=1:6, 
                       labels=c('lrg central metro', 'lrg fringe metro',
                                'med metor', 'sml metro', 'micropolitan', 'non-core'))]
fin[, quartile := factor(quartile, levels=1:4)]

#Create three separate datasets: one for total cases (no race/ethnicity)
#one for all racial groups and one for ethnicity groups

# I have records with zero population so including population  counts as an 
# offset (i.e., log(0)=-inf error). Therefore, I'm dropping zero population 
# counts and including the log of pop as an offset.

all<-fin[Pop>0 & Cases <= Pop & is.na(Race) & is.na(Ethnicity), ]
race<-fin[Pop>0 & Cases <= Pop & !is.na(Race), ]
ethn<-fin[Pop>0 & Cases <= Pop & !is.na(Ethnicity), ]

#Visualize temporary dataset
#--------------------------------------------------------------------
ggplot(all, aes(x = Cases)) + 
  geom_histogram(aes(y=..density..),colour = "black", fill = "light blue", binwidth = 1) +
  stat_function(fun=dpois, args=fitdistr(all$Cases,"poisson")$estimate,  n=50, size=1) +
  theme_bw()

ggplot(all[Cases>0,], aes(RPL_THEMES, log(Cases/Pop*100000))) +
  facet_grid(cols=vars(AgeGroup), rows=vars(Sex), scales='free')+
  geom_point()+
  stat_sum(alpha=0.5) + ## show overlapping/replicated points as circles (semi-transparent)
  scale_size(range=c(2,8)) + ## cosmetic: make minimum size (non-replicated) points larger  
  ## fit and display a flexible smooth curve for point data
  geom_smooth(method="gam",method.args=list(family="quasipoisson"))

ggplot(race[Cases>0 &!is.na(quartile),],aes(x=factor(quartile),y=Cases/Pop*100000, color=Race))+
  facet_wrap(~Race, scales='free')+
  stat_summary(fun.data=mean_cl_boot,size=1)+
  scale_color_calc()+
  theme_bw()+
  theme(legend.position = 'none')

#Hurdle Model Comparison (all race/ethnicities combined)
#--------------------------------------------------------------------
#Truncated Poisson Distribution
m0<-glmmTMB(Cases ~ offset(log(Pop)) + (1|GEOID), zi= ~1, family=truncated_poisson, data=all)
m1<-glmmTMB(Cases ~ quartile + offset(log(Pop)) + (1|GEOID), zi= ~1, family=truncated_poisson, data=all)
m2<-update(m1, Cases ~ quartile + sviyear + offset(log(Pop)) + (1|GEOID))
m3<-update(m2, Cases ~ quartile + sviyear + Sex + offset(log(Pop)) + (1|GEOID))
m4<-update(m3, Cases ~ quartile + sviyear + Sex + AgeGroup + offset(log(Pop)) + (1|GEOID))
m5<-update(m4, ziformula=~quartile + sviyear + Sex + AgeGroup)
m6<-update(m5, ziformula=~.)

#Truncated negative binomial distribution
m7<-update(m0, family=truncated_nbinom2)
m8<-update(m1, family=truncated_nbinom2)
m9<-update(m2, family=truncated_nbinom2)
m10<-update(m3, family=truncated_nbinom2)
m11<-update(m4, family=truncated_nbinom2)
m12<-update(m5, family=truncated_nbinom2)
m13<-update(m6, family=truncated_nbinom2)

#Compare model information
models<-list(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13)
model_specs<-data.frame()

for (i in 1:length(models)){
  model_specs[i, 1]<-as.character(models[[i]]$call)[2]
  model_specs[i, 2]<- as.character(models[[i]]$modelInfo$allForm$ziformula)[[2]]
  model_specs[i, 3]<-as.character(models[[i]]$call)[4]
  model_specs[i, 4]<-AIC(models[[i]])
  model_specs[i, 5]<-ifelse(models[[i]]$fit$convergence==0, 'Yes', 'No')
  model_specs[i, 6]<-length(models[[i]]$fit$par)
  model_specs[i, 7]<-sigma(models[[i]])
}

colnames(model_specs)<-c('Conditional Formula', 'ZI Formula', 'Distribution', 
                         'AIC', 'Covergence', 'DF', 'Dispersion Parameter')

library('kableExtra')
model_specs %>%
  kbl() %>%
  kable_material(c("striped"))


#It appears that the truncated negative binomial distribution results in the lowest AIC

#Visualize simulated residuals from best fit model #this takes > 15 min
#--------------------------------------------------------------------
simulationOutput <- simulateResiduals(fittedModel = m9, n = 1000) 
plot(simulationout)
testDispersion(simulationOutput = simulationOutput, alternative ="less")
testUniformity(simulationOutput = simulationOutput, )
 
#Predicted probabilities (p=p.1*(1-p.2))
#--------------------------------------------------------------------
p<-predict(m9, type='response')
p.1<-predict(m9, type='conditional')
p.2<-predict(m9, type='zprob')


#--------------------------------------------------------------------
#Race-specific Hurdle Models
#--------------------------------------------------------------------
m_white<-glmmTMB(Cases ~ RPL_THEMES + sviyear + Sex + AgeGroup + offset(log(Pop)) 
                 + (1|GEOID) + (1|State), 
                 zi= ~. + UrCode, 
                 family=truncated_nbinom2, data=race[Race=='W',])


summary(m_white)



#Race-specific Hurdle Poisson model (nh-White)
#--------------------------------------------------------------------
m3 <- glmmTMB(Cases ~  RPL_THEMES + sviyear + Sex + offset(log(Pop)) + (1|GEOID) + (1|State),
              zi= ~ log(Pop),
              family=truncated_poisson, 
              data=race[Race=='W',])

summary(m3)

# Dispersion statistic
E3 <- resid(m3, type = "pearson")
p  <- length(coef(m3))  
sum(E3^2) / (nrow(race[Race=='W',]) - p) 


#Race-specific Hurdle Poisson model (nh-Black)
#--------------------------------------------------------------------
m4 <- glmmTMB(Cases ~  RPL_THEMES + sviyear + Sex + offset(log(Pop)) + (1|GEOID) + (1|State),
              zi= ~ log(Pop),
              family=truncated_poisson, 
              data=race[Race=='B',])

summary(m4)

# Dispersion statistic
E4 <- resid(m4, type = "pearson")
p  <- length(coef(m4))  
sum(E4^2) / (nrow(race[Race=='B',]) - p) 


#Model Comparison
#--------------------------------------------------------------------
lrtest(model.zip2, model.zip3) 




#--------------------------------------------------------------------
# Part 2: DETERMINE IF THERE ARE RESIDUAL EFFECTS OF RACE AND ETHNICITY ON 
# SEVERITY OF SHIGELLA AFTER CONTROLLING FOR SVI
#--------------------------------------------------------------------
library(lme4)
folder<-'//cdc.gov/project/ATS_GIS_Store4/Projects/prj06135_Shigella_SVI/Data/FoodNet_NARMS/'
dat<-setDT(read.csv(paste0(folder, "analytic_file_V7.csv"), stringsAsFactors = F,
                    colClasses = c('CTNO2000'='character', 'CTNO2010'='character')))

#Classify age
AgeGroupLabs<-c('0-4', '5-14', '15-44', '45+')
dat$AgeGroup<-cut(dat$AgeClean, breaks=c(0, 5, 15, 45, 999), right=F, labels=AgeGroupLabs)

#Update missing census tract information using deterministic imputation result
dat[is.na(CTNO2000), CTNO2000 := Deterministic]
dat[is.na(CTNO2010), CTNO2010 := Deterministic]

dat$CTNO2000<-str_pad(dat$CTNO2000, width=11, side='left', pad='0')

#Classify cases by which year of svi we want to join to 
svibreaks<-c(2000, 2006, 2011, 2015, 2017, 2020)
svilabels<-c('2000', '2010', '2014', '2016', '2018')
dat[,'sviyear' := cut(Year, breaks=svibreaks, right=F, labels=svilabels)]





# Part X: Are there differences in the risk of abx-resistant Shigella among cases 
# by race after adjusting for SVI quartile?





# Part X: Are there differences in the risk of severe Shigella among cases 
# by race after adjusting for SVI quartile?