#------------------------------------------------#
#         Home Assignment Zoltan 2
#------------------------------------------------#
rm(list=ls())
setwd("~/Documents/Master/Statistics /Exams")


#Load all necessary packages
library(psych) # for describe	
library(lm.beta) # for lm.beta	
library(tidyverse) # for tidy format	
library(car) #For Homogenity of Variance
library(ggplot2) #For Graphs

################# Data Preparation ################

df_prelim <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv")
describe(df_prelim)
df <- df_prelim[-18,] #excluding same case as in Assignment 1 due to falsely coded data

which(df$household_income <= 0)
df[48,]
#According to Pew Research Center, Household Income can be zero or negative (https://www.pewsocialtrends.org/2018/07/12/methodology-15/)
#Roughly 1% of American households in census have negative income
#Could of course be coding mistake, but will be kept in this data set because that data point is not impossible
# and no experimentor can be asked about it
#Also, that participant is not an influential outlier according to Cook's Distance

n2 <- length(df$pain)

################# Assumptions ######################
mod_full <- mod2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + 
                         mindfulness + weight + IQ + household_income, data=df)

#Cook's Distance
plot(mod_full, which=4) #outliers 55, 74, 88

which(cooks.distance(mod_full) > 1) #none
outliers_mod_full <- which(cooks.distance(mod_full) > (4/n2)) #10 influential points
print(df[outliers_mod_full,])
#Once again no junk data and no theoretical justification for exclusion.

mod_full_corr <- mod2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + 
                         mindfulness + weight + IQ + household_income, data=df, subset= -outliers_mod_full)

summary(mod_full)
summary(mod_full_corr)
#One difference: Household income only significant without the outliers. Nevertheless, if they do not
#influence the assumptions, they will be kept in the data due to a lack of justification for exclusion

#Normality of Residuals
hist(x=residuals(mod_full))
hist(x=residuals(mod_full_corr))

shapiro.test(residuals(mod_full))
shapiro.test(residuals(mod_full_corr)) #Both models' residuals normally distributed, non-corrected better

#Linearity of Relationship
plot(mod_full, which=1) 
plot(mod_full_corr, which=1)

residualPlots(mod_full) 
residualPlots(mod_full_corr) #Catastrophizing pain violates assumption in both models
#But as in Assignment one: no corrections taken due to an overall linearity and Navarro's approach
#(see assignment one for direct quote)

#Multicollinearity
vif(mod_full)
vif(mod_full_corr) #No violations in either model

#Homogeneity of Variance
plot(mod_full, which=3)
plot(mod_full_corr, which=3) 

ncvTest(mod_full)
ncvTest(mod_full_corr) #No violations in either model, but corrected model with a p=.058

#The models with and without influential data points do not differ in the assumptions
#Therefore, the model including influential data will be used for further analysis

################# Elimination & Model Comparison ######################

#Actual backwards elimination
step(object = mod_full, direction = "backward")
#Eliminated factors in correct order: STAI_trait, household_income, IQ

#For the record: also testing the model without influential outliers:
step(mod_full_corr) #household_income would actually be included in final model

#Therefore, this model is the backwards model:
mod_back <- lm(formula = pain ~ age + sex + pain_cat + cortisol_serum + mindfulness + 
                       weight, data = df)

#Evaluating that model individually
summary(mod_back)
confint(mod_back)
lm.beta(mod_back)

#Testing assumptions in the resulting backward model
#Cook's Distance
plot(mod_back, which=4) #three outliers

which(cooks.distance(mod_back) > 1) #none
outliers_mod_back <- which(cooks.distance(mod_back) > (4/n2)) #10 influential points
print(df[outliers_mod_full,]) #Have not been included before, will be kept here too

#Normality of Residuals
hist(x=residuals(mod_back))
shapiro.test(residuals(mod_back)) #normally distributed!

#Linearity of Relationship
plot(mod_back, which=1) 
residualPlots(mod_back) #Same problem with catastrophizing pain, same reasons for keeping

#Multicollinearity
vif(mod_back) #no problems

#Homogeneity of Variance
plot(mod_back, which=3)
ncvTest(mod_back) #no violations

################ Model Comparison ################

#Backward model with full model
summary(mod_back)$adj.r.squared
summary(mod_full)$adj.r.squared

AIC(mod_back)
AIC(mod_full)

anova(mod_back, mod_full)

#Comparison between resulting backward model and theoretical model
mod_theory <- mod2_better <- lm(pain ~ age + sex + STAI_trait + pain_cat + 
                                    cortisol_serum + mindfulness, data=df) #from assignment one

AIC(mod_back) 
AIC(mod_theory) 
AIC(mod_back) - AIC(mod_theory)
#Differences in AIC of 2 or more are often considered to be significant

summary(mod_back)$adj.r.squared * 100
summary(mod_theory)$adj.r.squared * 100 
#Also higher adj. R^2 in the backwards model

#Anova cannot be used since models are not nested

################ Prepping new Data ###############

df2 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_2.csv")

str(df2)
describe(df2) #No flawed data can be discovered, all variables within ranges

#For describing sample
mean(df2$age) #40.43
sd(df2$age) #4.78
table(df2$sex) #79f, 81m
length(df2$pain)

############### Making Predictions for new Data #################

#prediction column for each of the models
df2 <- df2 %>% 
  mutate(pred_mod_back = predict(mod_back, newdata=df2)) %>% 
  mutate(pred_mod_theory = predict(mod_theory, newdata=df2))

#RSS
RSS_back <- sum((df2$pred_mod_back - df2$pain)^2)
RSS_theory <- sum((df2$pred_mod_theory - df2$pain)^2)

#RAS (for comparison, not reported)
RAS_back <- sum(abs(df2$pred_mod_back - df2$pain))
RAS_theory <- sum(abs(df2$pred_mod_theory - df2$pain))

#Sum of Squared Differences for backwards model: 232.8494
#Sum of Squared Differences for theoretical model: 228.5155

#Therefore, the theoretical model can be assumed to be (slightly) better
#The backwards model might be overfitted
#Theoretical model is superior due to its being built on theoretical assumptions (and actual performance)


