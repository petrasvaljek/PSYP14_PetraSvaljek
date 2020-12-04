#Assignment 2
library(gridExtra) # for grid.arrange
library(psych) # for describe 
library(tidyverse) # for tidy code and ggplot
library(car) # for residualPlots, vif, pairs.panels, ncvTest
library(lmtest) # bptest
library(sandwich) # for coeftest vcovHC estimator
library(boot) # for bootstrapping
library(lmboot) # for wild bootsrapping
library(lm.beta)
#Data
home_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")
new_datasample <- home_sample_1 %>%  slice(c(-93,-150))

#Data Visualization
weight_plot = new_datasample %>% 
  ggplot() +
  aes(x = weight, 
      y = pain,
      label = ID) +
  geom_point() + 
  geom_label()
weight_plot #ID_10 & ID_100 are a bit unusual 

IQ_plot = new_datasample %>% 
  ggplot() +
  aes(x = IQ, 
      y = pain,
      label = ID) +
  geom_point() + 
  geom_label()
IQ_plot

household_income = new_datasample %>% 
  ggplot() +
  aes(x = household_income, 
      y = pain,
      label = ID) +
  geom_point() + 
  geom_label()
household_income

#Backward Regression
whole_model = lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + IQ + household_income, data = new_datasample)
whole_model

backward_model = step(whole_model, direction = "backward")
backward_model
summary(backward_model)
theory_based_model = lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = new_datasample)

#Cook's Distance
backward_model %>% plot(which = 4) #all lower than 1

#Assumption Check 

#Normality of Residuals
#QQ Plot
backward_model%>% plot(which = 2) 
# histogram
residuals_backwardmodel = enframe(residuals(backward_model)) 
residuals_backwardmodel %>% ggplot() + 
  aes(x = value) +
  geom_histogram()
# skew and kurtosis
describe(residuals(backward_model)) #normal when bewtween -1 & 1 

#Linearity
backward_model%>% residualPlots() #p-value bigger than 0.05, thus no linearity violation, even tough there is a slight curve 

#Homoscedasticty
backward_model%>% plot(which = 3)
#NCV test
backward_model%>% ncvTest() # smaller than 0.05 indicates HETEROSCADESTICITY 
#Breush-Pagan test
backward_model%>% bptest() 

#Multicollinearity
#variance inflation factor -> higher than 3 is problematic
backward_model%>% vif()

#all the assumptions are met
summary(backward_model)
confint(backward_model)
lm.beta(backward_model)

#Comparison
AIC (whole_model)
AIC(backward_model)
AIC(theory_based_model)

summary(whole_model)$adj.r.squared
summary(backward_model)$adj.r.squared
summary(theory_based_model)$adj.r.squared

#ANOVA - backward & whole
anova(backward_model, whole_model)

#New Data
home_sample_2 = read.csv("https://tinyurl.com/ha-dataset2")

# calculate predicted values
pred_test <- predict(theory_based_model, home_sample_2) 
pred_test_back <- predict(backward_model, home_sample_2) 

#the sum of squared residuals
RSS_test = sum((home_sample_2 [,"pain"] - pred_test)^2)
RSS_test
RSS_test_back = sum((home_sample_2[, "pain"] - pred_test_back)^2)
RSS_test_back
