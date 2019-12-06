#------------------------------------------------#
#         Home Assignment Zoltan 1
#------------------------------------------------#
rm(list=ls())
graphics.off()
setwd("~/Documents/Master/Statistics /Exams")


#Load all necessary packages
library(psych) # for describe	
library(lm.beta) # for lm.beta	
library(tidyverse) # for tidy format	
library(car) #For Homogenity of Variance
library(ggplot2) #For Graphs

#Reading in data
df_prelim <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv")

#First inspection
str(df_prelim)
describe(df_prelim)

#All measures are inside the given variable boundaries, except for 
#STAI_trait, which has a scale from 20-80 but a minimum value of 3.5

#Find the wrong value
df_prelim[df_prelim$STAI_trait<20,] #ID number 18

#Has to be a coding error but no experimentor can be asked whether actually 35 was meant.
#In order to not influence data in unforseeable ways, the person is excluded from analysis

df <- df_prelim[-18,]

#There is one participant with a negative household income but since that is not a variable used in this
#assignment, it can be disregarded for now

#For overview of the sample in report: age and genders and n
mean(df$age)
sd(df$age)
table(df$sex)
n <- length(df$pain)

################  Model 1: age and sex  #####################
plot(x=df$sex, y=df$pain)
plot(x=df$age, y=df$pain) #First insights

#Specify the model using age and sex to explain pain
mod1 <- lm(pain ~ age + sex, data=df)

#Checking model assumptions

#First: Outliers with Cook's distance
#Visual Inspection
plot(mod1, which=4) #Three outliers: 28, 74, 88
plot(mod1, which=5) #Three outliers: 28, 74, 88

#Calculation of cooks distance
cooks_mod1 <-cooks.distance(mod1)

which(cooks.distance(mod1) > 1) #none
outliers_mod1 <- which(cooks.distance(mod1) > (4/n))
outliers_mod1

#Seven outliers that are across the critical threshold of 4/n
#Navarro suggests 1 as a critical threshold, then: no outliers

#Checking outliers
print(df[outliers_mod1, ])
#Nothing unusual, only some unusual connections between cortisol levels and pain perception
#But no junk data and no theoretical justification for excluding them (all well within the scales)

#Checking whether model performance depends on outliers
mod1_corr <- lm(pain~age + sex, data=df, subset =  -outliers_mod1)

summary(mod1)
summary(mod1_corr) #Checking df: outliers actually removed

#Factors change a bit, and coefficient of sex is "less" significant (but still p = .007), and levels of 
#significance are a disputed subject anyway
#Overall performance is comparable
#Since there are no theoretical justifications to exclude participants, they remain in the data set
#Nevertheless:
#All assumptions will also be tested in a data set without outliers to see whether assumptions are violated

#Second: normality of residuals

plot(mod1, which=2) #visual inspection
plot(mod1_corr, which=2)
hist(x=residuals(mod1)) #second visual inspection
hist(x=residuals(mod1_corr)) 
#Both seem normally distributed, corrected model in a smaller interval (because outliers excluded)

shapiro.test(residuals(mod1)) #statistical test
shapiro.test(residuals(mod1_corr)) 
#Shapiro Test: Not significant and therefore normally distributed, model with outliers even a bit better

#Third: Linearity of Relationship
plot(mod1, which=1) #Outliers: 28, 88, 123
plot(mod1_corr, which=1) #Outliers: 23, 55, 65
#Both look quite linear

residualPlots(mod1) 
residualPlots(mod1_corr)
#All linear, no significant deviations from linearity, in the corrected model: age more curved
#than in model with outliers 

#Fourth: Homogeneity of Variance
plot(mod1, which=3)
plot(mod1_corr, which=3) #Visual inspection ambiguous 

ncvTest(mod1)
ncvTest(mod1_corr)
#Non-constant Variance Score Test: not significant, therefore variances are homogenous
#Corrected model performs better than non-corrected

#Fifth: No Multicollinearity
vif(mod1)
vif(mod1_corr)
#No Intercorrelations!

#Assumptions do not rely on outliers, some assumptions were met "better" with, some without the outliers
#Therefore, mod1 with the outliers will be taken (as to not change the data in an unnecessary way)
summary(mod1) #Significance of the model and estimation of coefficients
confint(mod1) #Confident intervals of coefficients
lm.beta(mod1) #Standardized beta of coefficients

#Plots
gg_mod1_age <- ggplot(df, aes(x=age, y=pain)) +
  geom_point() +
  labs(x = "Age", y="Postoperative Pain") +
  theme_bw() +
  ylim(0,10) +
  geom_smooth(method="lm", se=F, color="deepskyblue2")
gg_mod1_age

ggsave("ZK1_age_on_pain.png", path= "~/Documents/Master/Statistics /Assignment Writing", 
       plot = gg_mod1_age, width = 16, height = 12, units = "cm")


############## Model 2 #############

#age, sex, STAI, pain catastrophizing, mindfulness, and cortisol measures

mod2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + 
             cortisol_saliva + mindfulness, data=df)

#Before checking any other assumptions or outliers, I want to check multicollinearity
#I would expect both cortisol measures to correlate highly since they are merely different methods that
#measure the same thing. 
vif(mod2)
#They do have high measures, around 5. A value of 4 is often considered as the critical threshold
#Also, theoretically, it makes a lot of sense to only leave in one of the variables
#Therefore, a new model with only the superior cortisol measure (serum) will be used
mod2_better <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data=df)

vif(mod2_better) #no collinearity anymore

#With this model: checking other assumptions

#Cook's Distance
plot(mod2_better, which=4) #outliers 74, 88, 123

which(cooks.distance(mod2_better) > 1) #none
outliers_mod2 <- which(cooks.distance(mod2_better) > (4/n))
outliers_mod2
#8 outliers

print(df[outliers_mod2,]) #Same as in model 1: no junk data

mod2_better_corr <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum
                       + mindfulness, data=df, subset=-outliers_mod2)

summary(mod2_better)
summary(mod2_better_corr) #Significances do change a little and better F in corrected model
#But same line of reasoning: no theoretical justification for exclusion, 
#therefore kept if assumptions are still met 

#Normality of Residuals
hist(x=residuals(mod2_better))
hist(x=residuals(mod2_better_corr)) 
plot(mod2_better, which=2)
plot(mod2_better_corr, which=2)
#Visual inspection: residuals seem normally distributed, rather better in non-corrected model

shapiro.test(residuals(mod2_better))
shapiro.test(residuals(mod2_better_corr)) #better for non-corrected and definitely met

#Linearity of Relationship

plot(mod2_better, which=1) #Outliers: 74, 88, 119
plot(mod2_better_corr, which=1) #Outliers: 65, 116, 159
#but both seem linear

residualPlots(mod2_better) 
residualPlots(mod2_better_corr) 

#Two individual predictors are significantly curved but the overall model is linear, therefore
#(and following Navarro: "it still bears remembering that the pattern in Figure 15.12 is pretty
#damn straight: in other words the deviations from linearity are pretty small, and probably not worth
#worrying about.") no corrections will be taken

#Homogeneity of Variance
plot(mod2_better, which=3)
plot(mod2_better_corr, which=3) #Both seem fine
ncvTest(mod2_better)
ncvTest(mod2_better_corr) #non-corrected better and both have homogeneity of variance

#As there are no differences between the corrected model and the one including outliers, the one
#with outliers will be taken for analysis

#Checking model
summary(mod2_better)
confint(mod2_better)
lm.beta(mod2_better)

#One possible graph with the strongest predictor
gg_mod2_cortisol <- ggplot(df, aes(x=cortisol_serum, y=pain)) +
  geom_point() +
  labs(x = "Cortisol", y="Postoperative Pain") +
  theme_bw() +
  ylim(0,10) +
  geom_smooth(method="lm", se=F, color="deepskyblue2")
gg_mod2_cortisol

ggsave("ZK1_cortisol_on_pain.png", path= "~/Documents/Master/Statistics /Assignment Writing", 
       plot = gg_mod2_cortisol, width = 16, height = 12, units = "cm")


########### Comparing Models ##########

summary(mod1)$adj.r.squared * 100
summary(mod2_better)$adj.r.squared * 100 

AIC(mod1, mod2_better) 

anova(mod1, mod2_better)
  
  
  
