Assignment 1  

install.packages("tidyverse")
install.packages("dplyr")
install.packages("gridExtra")
install.packages("psych")
install.packages("car")
install.packages("lmtest")
install.packages("sandwich")
install.packages("boot")
install.packages("lmboot")

#Checking raw data
data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")

View(data_sample_1)

data_sample_1 %>% 
  summary()

#STAI_trait min 4.2


data_sample_1 %>% 
  select(STAI_trait)

data_sample_1 %>% 
  filter(STAI_trait == "4.2")

data_sample_1 %>% 
  filter(ID == "ID_34")

data_sample_1 %>% 
  filter(ID=="ID_88")


data_sample2 = data_sample_1[-c(34, 88),]
data_sample2 %>% 
  summary()

data_sample2 %>% describe()

#Regression models

model2= lm(pain~age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum, data=data_sample2)
model2 %>% 
  summary()

model_1=lm(pain~age+sex, data=data_sample2)
model_1
summary(model_1)

#visualization

data_sample2 %>% 
  mutate(row_names = row.names(data_sample2)) %>% 
  ggplot()+
  aes(x=STAI_trait, y=pain, label = row_names)+
  geom_point()+
  geom_text()

data_sample2 %>% 
  mutate(row_names = row.names(data_sample2)) %>% 
  ggplot()+
  aes(x=pain_cat, y=pain, label = row_names)+
  geom_point()+
  geom_text()

data_sample2 %>% 
  mutate(row_names = row.names(data_sample2)) %>% 
  ggplot()+
  aes(x=mindfulness, y=pain, label = row_names)+
  geom_point()+
  geom_text()

data_sample2 %>% 
  mutate(row_names = row.names(data_sample2)) %>% 
  ggplot()+
  aes(x=cortisol_serum, y=pain, label = row_names)+
  geom_point()+
  geom_text()  

data_sample2 %>% 
  ggplot()+
  aes(x=STAI_trait, y=pain)+
  geom_point()+
  geom_smooth(method ="lm" )

#regression line can not explain outlier 

#Check for errors and leverage with Cooks distance: 

model2 %>% 
  plot(which=4)

#47, 65, 86  are noticable 

data_sample2 %>% slice(c(47,65, 86))

#-> nothing suspicious
4/158
#-> they all exceed that number but the other assumption is >1 not the case at all, so i will keep them 

Normality 

model2 %>% 
  plot(which=2)

Residuals_mod2 = enframe(residuals(model2))

Residuals_mod2 %>% 
  ggplot()+
  aes(x=value)+
  geom_histogram()

#Skewness and kurtosis: 

describe(residuals(model2))

#-> skew: -0.18 and kurtosis: 0.02 -> both don´t violate Normaility

#Linearity

residualPlot(model2)

model2 %>% 
  residualPlots()

# there is a little shape of the curve
# teststatistics also do not indicate a significance -> no violation of linearity

#No Homoscedacity 
model2 %>% 
  plot(which=3)

#-> point are very much shaped in a square no tunnel distribution, but just to make sure: ncv  

model2 %>% 
  ncv.Test()

model2 %>% 
  bptest()

#-> no heteroscadicity

#No  Multicollenarity

#Check correlation

model2 %>% 
  vif()
#-> no score above 3

summary(model2)$adj.r.squared
summary(model_1)$adj.r.squared

#-> model 2 has the higher explainatory power 

#AIC model fit index to check if the differences are significant

AIC(model_1)
AIC(model2)

#-> the difference is widly over 2, so the differences between the models are significant

#It is the two models are nested: anova function

anova(model_1, model2)


confint(model_1)
confint(model2) 

#Standardized Beta score: 
install.packages("lm.beta")
lm.beta(model_1)
lm.beta(model2)


#Assignment 2

#Model diagnostics for the new model: 

Regression_model_new = lm(pain~age +sex+STAI_trait+pain_cat+mindfulness+cortisol_serum+weight+IQ+household_income, data=data_sample2)
Regression_model_new %>% 
  summary()
AIC(Regression_model_new)


Checking cooks Distance: 
  
  Regression_model_new %>% 
  plot(which=4)

#-> 47, 85 and 86 show up but still not high enough for criteria above 1

#Normaility

Regression_model_new %>% 
  plot(which=2)

Residuals_mod_new = enframe(residuals(Regression_model_new))
Residuals_mod_new %>% 
  ggplot()+
  aes(x=value)+
  geom_histogram()


#Skewness and kurtosis: 

describe(residuals(Regression_model_new))

#-> skew: -0.17 and kurtosis: 0.08 -> both don´t violate Normaility

Linearity

residualPlot(Regression_model_new)

Regression_model_new %>% 
  residualPlots()

#-> there is a little shape of the curve
#-> teststatistics also do not indicate a significance -> no violation of linearity

No Homoscedacity 

Regression_model_new %>% 
  plot(which=3)

-> point are very much shaped in a square no tunnel distribution, but just to make sure: ncv  

Regression_model_new %>% 
  bptest()

#-> no significance here either 

#-> no heteroscadicity

#No  Multicollenarity

#Check correlation

Regression_model_new %>% 
  vif()
#-> no score above 3

#No violation of the new Regression model 

Backward regression with new model: 
  
  Regression_model_new_backward = step(Regression_model_new, direction = "backward")
Regression_model_new_backward %>% 
  summary()

lm.beta(Regression_model_new_backward)

confint(Regression_model_new_backward)

AIC(Regression_model_new_backward)

#-> Resulting significant variables: age, pain_cat, mindfulness, cortisol_serum

New Backward regression with the resulting variables: 
  
  Backward_model_1=lm(pain~age+pain_cat+mindfulness+cortisol_serum, data=data_sample2)
Backward_model_1
summary()

AIC(Backward_model_1) 

theory_based_model = model2
theory_based_model

AIC score comparison: 
  
  AIC(Backward_model)
AIC(theory_based_model)
#-> according to those numbers, the backward model is better

Anova comparison: 
  
  anova(Backward_model, theory_based_model)

Test on new dataset: 
  
  home_sample_2 = read.csv("https://tinyurl.com/87v6emky")

View(home_sample_2)

home_sample_2 %>% summary()
#-> no outliers observable 

pred_Backward_model = predict(Backward_model, home_sample_2)
pred_theory_based_model = predict(theory_based_model, home_sample_2)

RSS_backward=sum((home_sample_2[,"pain"]-pred_Backward_model)^2)
RSS_backward
RSS_theory_based_model = sum((home_sample_2[,"pain"]-pred_theory_based_model)^2)
RSS_theory_based_model
#-> theory based, is the better RSS score -> it  is significantly more predictive than the backward model!

#Assignment 3

packages:
  library(cAIC4) 
install.packages("cAIC4")
install.packages("r2glmm")
library(r2glmm) 
install.packages("lme4")
library(lme4)
install.packages("lmerTest")
library(lmerTest) 
library(MuMIn) 
install.packages("MuMIn")

#Checking dew dataset: 

data_file_3 = read.csv ("https://tinyurl.com/b385chpu")
View(data_file_3)
data_file_3 %>% summary()

#-> income_household negative

data_file_3 %>% 
  select(household_income) %>% 
  describe()

data_file_3 %>% 
  filter(household_income==-7884)

#-> ID 91 needs to be excluded

data_file_3.1 = data_file_3[-c(91),]
View(data_file_3.1)

data_file_3.1 %>% 
  select(sex) 

data_file_3.1 = data_file_3.1 %>% 
  mutate(sex = replace(sex, sex=="woman", "female"))
view(data_file_3.1)

#-> ID_25 needs to be mutated

view(data)


View(data_file_3.1)
data_file_3


#Classification of intercept variable


data_file_3.1_factor = data_file_3.1 %>% 
  mutate(hospital=factor(hospital))

data_file_3.1_factor %>% 
  ggplot()+
  aes(y=pain, x= age )+
  geom_point(aes(color=hospital), size = 4)+
  geom_smooth(method="lm", se = F)

data_file_3.1_factor %>% 
  ggplot()+
  aes(y=pain, x= pain_cat)+
  geom_point(aes(color=hospital), size = 4)+
  geom_smooth(method="lm", se = F)

data_file_3.1_factor %>% 
  ggplot()+
  aes(y=pain, x= STAI_trait)+
  geom_point(aes(color=hospital), size = 4)+
  geom_smooth(method="lm", se = F)

data_file_3.1_factor %>% 
  ggplot()+
  aes(y=pain, x= mindfulness)+
  geom_point(aes(color=hospital), size = 4)+
  geom_smooth(method="lm", se = F)

data_file_3.1_factor %>% 
  ggplot()+
  aes(y=pain, x= cortisol_serum)+
  geom_point(aes(color=hospital), size = 4)+
  geom_smooth(method="lm", se = F)

#random intercept model: 
mod_rnd_int1 =lmer(pain~age+sex+pain_cat+mindfulness+STAI_trait+cortisol_serum+(1|hospital), data=data_file_3.1_factor)

coefficients   
mod_rnd_int1 %>% 
  summary()

confint(mod_rnd_int1)

#Visualize for the strongest effect fixed effect predictor (cortisol_serum): 

mod_intercept_cortisol = data_file_3.1_factor %>% 
  ggplot()+
  aes(y=pain, x=cortisol_serum, color = hospital)+
  geom_point(size=4)+
  geom_smooth(method ="lm", se  =F, fullrange = TRUE)
mod_intercept_cortisol 

mod_intercept_pain_cat = data_file_3.1_factor %>% 
  ggplot()+
  aes(y=pain, x=pain_cat, color = hospital)+
  geom_point(size=4)+
  geom_smooth(method ="lm", se  =F, fullrange = TRUE)
mod_intercept_pain_cat

mod_intercept_age = data_file_3.1_factor %>% 
  ggplot()+
  aes(y=pain, x=age, color = hospital)+
  geom_point(size=4)+
  geom_smooth(method ="lm", se  =F, fullrange = TRUE)
mod_intercept_pain_age

mod_intercept_mindfulness = data_file_3.1_factor %>% 
  ggplot()+
  aes(y=pain, x=mindfulness, color = hospital)+
  geom_point(size=4)+
  geom_smooth(method ="lm", se  =F, fullrange = TRUE)
mod_intercept_mindfulness

#-> all lines are very much parralel except, whicbh means,that  effect of predictor within the clusters are roughly the same

#Look more into detail: Must be built for all significant predictors (age, pain_cat, cortisol, mindfulness)

mod_yaxis_intercept_cort=mod_intercept_cortisol+
  xlim(-1,50)+geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)
mod_yaxis_intercept_cort

mod_yaxis_intercept_pain_cat=mod_intercept_pain_cat+
  xlim(-1,50)+geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)
mod_yaxis_intercept_pain_cat

mod_yaxis_intercept_age=mod_intercept_age+
  xlim(-1,50)+geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)
mod_yaxis_intercept_age

mod_yaxis_intercept_mind=mod_intercept_mindfulness+
  xlim(-1,50)+geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)
mod_yaxis_intercept_mind

#-> All regression lines are quite parallel 

#Marginal R2 of fixed effect predictors:

r2beta(mod_rnd_int1, method = "nsj", data = data_file_3.1_factor)	
library(cAIC4) 
install.packages("cAIC4")
install.packages("r2glmm")
library(r2glmm) 
install.packages("lme4")
library(lme4)
install.packages("lme4")
install.packages("lmerTest") 

Conditional R^2: 
  r.squaredGLMM(mod_rnd_int1)

library(MuMIn) 
install.packages("MuMIn")

#Predction for new dataaset: 
#Check data file 4

data_file_4 = read.csv("https://tinyurl.com/4f8thztv")
View(data_file_4)

#R2 for datafile 4


predict1 = predict(mod_rnd_int1, data_file_4, allow.new.levels = TRUE) 
predict1
#R^2 for datafile 4

mod_mean = lm(pain ~ 1, data = data_file_4)
TSS = sum((data_file_4$pain - predict(mod_mean))^2)
TSS
#R2
1-(RSS_mod_rnd1/TSS)


r2beta(mod_rnd_int2, method = "nsj", data=data_file_4)

r.squaredGLMM(mod_rnd_int2) 

#R2 of old model to identify most influential predictor:
r2beta(mod_rnd_int2, method="nsj", data=data_file_3.1_factor)

# most influential predicotor = cortisol_serum 

#New random intercept and slope model on dataset 3: 

mod_rnd_int3 = lmer(pain~cortisol_serum + (1|hospital), data=data_file_3.1_factor)
mod_slope_dataset3 = lmer(pain~cortisol_serum +(cortisol_serum|hospital), data = data_file_3.1_factor)
mod_slope_dataset3_opt =lmer(pain~cortisol_serum +(cortisol_serum|hospital), control=lmerControl(optimizer = "Nelder_Mead"), data= data_file_3)
mod_rnd_int3
r2beta(mod_rnd_int3)
mod_slope_dataset3
r2beta(mod_slope_dataset3)
mod_slope_dataset3_opt
r2beta(mod_slope_dataset3_opt)


RSS_mod_3=sum(residuals(mod_rnd_int3)^2)
RSS_mod_3
RSS_mod_slope = sum(residuals(mod_slope_dataset3)^2)
RSS_mod_slope
RSS_mod_slope_opt = sum(residuals(mod_slope_dataset3_opt)^2)
RSS_mod_slope_opt

#->here RSS slope offers the best fit 
#but AIC usually provides more suitable insights, for random and slope use Caic   
install.packages("cAIC4")

cAIC_mod_3 = cAIC(mod_rnd_int3)$caic
cAIC_mod_3
cAIC_mod_slope = cAIC(mod_slope_dataset3)$caic
cAIC_mod_slope
cAIC_mod_slope_opt = cAIC(mod_slope_dataset3_opt)$caic
cAIC_mod_slope_opt

#Apperantly the model is too complex, allowing the slope with hospital clustering does not improve the predictive value of the whole mode

data_file_3.1_factor = data_file_3.1_factor %>%
  mutate(pred_int1 = predict(mod_rnd_int3),
         pred_int2=predict(mod_slope_dataset3))

#Regression line of the slope model 

data_file_3.1_factor %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, group = hospital)+		
  geom_point(aes(color = hospital), size = 2) +		
  geom_line(color='red', aes(y=pred_int2, x=cortisol_serum))+		
  facet_wrap( ~hospital , ncol = 2)	

