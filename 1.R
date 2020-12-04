#Assignment 1 
library(gridExtra) # for grid.arrange
library(psych) # for describe 
library(tidyverse) # for tidy code and ggplot
library(car) # for residualPlots, vif, pairs.panels, ncvTest
library(lmtest) # bptest
library(sandwich) # for coeftest vcovHC estimator
library(boot) # for bootstrapping
library(lmboot) # for wild bootsrapping
library(lm.beta)
#Data Check
data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")

View(data_sample_1)
data_sample_1

describe(data_sample_1)
data_sample_1 %>% 
  summary() #age max is 444 & STAI min.is 3.90

#Visualization of Data
plot_pain = data_sample_1 %>% 
  ggplot() +
  aes(x = pain,) +
  geom_bar() 
plot_pain

age_plot = data_sample_1 %>% 
  ggplot() +
  aes(x = pain, 
      y = age,
      label = ID) +
  geom_point() + 
  geom_label() 
age_plot # extreme case ID_93 -> influential case, high prediction error

sex_plot = data_sample_1 %>% 
  ggplot() +
  aes(x = sex, 
      y = pain,
      label = ID) +
  geom_point() + 
  geom_label()
sex_plot

STAI_plot = data_sample_1 %>% 
  ggplot() +
  aes(x = STAI_trait, 
      y = pain,
      label = ID) +
  geom_point() + 
  geom_label() 
STAI_plot # case 149 (STAI 3.9, data error), scale from 20 to 80

pain_cat_plot = data_sample_1 %>% 
  ggplot() +
  aes(x = pain_cat, 
      y = pain,
      label = ID) +
  geom_point() + 
  geom_label() 
pain_cat_plot

cortisol_serum_plot = data_sample_1 %>% 
  ggplot() +
  aes(x = cortisol_serum, 
      y = pain,
      label = ID) +
  geom_point() + 
  geom_label() 
cortisol_serum_plot

cortisol_saliva_plot = data_sample_1 %>% 
  ggplot() +
  aes(x = cortisol_saliva, 
      y = pain,
      label = ID) +
  geom_point() + 
  geom_label() 
cortisol_saliva_plot

mindfulness_plot = data_sample_1 %>% 
  ggplot() +
  aes(x = mindfulness, 
      y = pain,
      label = ID) +
  geom_point() + 
  geom_label() 
mindfulness_plot

#Models - Cook's Distance
model1 = lm(pain ~ age + sex, data = data_sample_1)
model1
model1 %>% plot(which = 4) #case 93 has extreme Cook's distance
data_sample_1 %>% slice(93,100,141) #93 should be removed
model2 = lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = data_sample_1)
model2 %>% plot(which = 4)
data_sample_1 %>% slice(114,150)

#Exclusion of ID_93 & ID_150 due to data entry error
new_datasample <- data_sample_1 %>%  slice(c(-93,-150))
View(new_datasample)

#New Model Check
model1 = lm(pain ~ age + sex, data = new_datasample)
model1
summary(model1)
confint(model1)
lm.beta(model1)
#Cook's Distance
model1 %>% plot(which = 4)
new_datasample %>% slice(c(99,127,140)) #no exclusion since they're the cases that scored the minimum & maximum on pain_scale
describe(new_datasample)
new_datasample %>% 
  summary()

#Assumption Check for MODEL 1

#Normality of Residuals
#QQ Plot
model1 %>% plot(which = 2) 
# histogram
residuals_model1 = enframe(residuals(model1)) 
residuals_model1 %>% ggplot() + 
  aes(x = value) +
  geom_histogram()
# skew and kurtosis
describe(residuals(model1)) #normal when bewtween -1 & 1 

#Linearity
model1 %>% residualPlots() #p-value bigger than 0.05, thus no linearity violation, even tough there is a slight curve 

#Homoscedasticty
model1 %>% plot(which = 3)
#NCV test
model1 %>% ncvTest() # smaller than 0.05 indicates HETEROSCADESTICITY 
#Breush-Pagan test
model1 %>% bptest() 

#Multicollinearity
#variance inflation factor -> higher than 3 is problematic
model1 %>% vif()

###### Visualization 


#Model2 
model2 = lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = new_datasample)
model2

#Normality of Residuals
#QQ Plot
model2 %>% plot(which = 2) 
# histogram
residuals_model2 = enframe(residuals(model2 )) 
residuals_model2 %>% ggplot() + 
  aes(x = value) +
  geom_histogram()
# skew and kurtosis
describe(residuals(model2 )) #normal when bewtween -1 & 1 

#Linearity
model2  %>% residualPlots() #p-value smaller than 0.05, thus no linearity violation, even tough there is a slight curve 

#Homoscedasticty
model2  %>% plot(which = 3)
#NCV test
model2  %>% ncvTest() # smaller than 0.05 indicates HETEROSCADESTICITY 
#Breush-Pagan test
model2  %>% bptest() 

#Multicollinearity
#variance inflation factor -> higher than 3 is problematic
model2  %>% vif() # above 3 vif for both cortisol variables  
new_datasample%>% select(age, sex, STAI_trait, pain_cat, cortisol_serum, cortisol_saliva, mindfulness) %>% 
  pairs.panels(col = "red", lm = T)
new_datasample %>% select(cortisol_serum,cortisol_saliva) %>% cor()
#extremely high correlation between the 2, r = 0.89

#removal of cortisol_saliva
new_model2 = lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = new_datasample)
new_model2
summary(new_model2)
confint(new_model2)
lm.beta(new_model2)
#Normality of Residuals
#QQ Plot
new_model2 %>% plot(which = 2) 
# histogram
residuals_new_model2 = enframe(residuals(new_model2)) 
residuals_new_model2 %>% ggplot() + 
  aes(x = value) +
  geom_histogram()
# skew and kurtosis
describe(residuals(new_model2)) #normal when bewtween -1 & 1 

#Linearity
new_model2 %>% residualPlots() #p-value smaller than 0.05, thus no linearity violation, even tough there is a slight curve 

#Homoscedasticty
new_model2 %>% plot(which = 3)
#NCV test
new_model2 %>% ncvTest() # smaller than 0.05 indicates HETEROSCADESTICITY 
#Breush-Pagan test
new_model2 %>% bptest() 

#Multicollinearity
#variance inflation factor -> higher than 3 is problematic
new_model2 %>% vif() # above 3 vif for both cortisol variables  

summary(model1)
summary(new_model2)
#Model Fit Comparison (dif larger than 2 indicates sig.dif in model fit)
AIC(model1)
AIC(new_model2)

#Model Comparison

#ANOVA Comparison (nested models)
anova(model1,new_model2)


 