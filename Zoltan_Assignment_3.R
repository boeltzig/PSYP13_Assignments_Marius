#------------------------------------------------#
#         Home Assignment Zoltan 3
#------------------------------------------------#
rm(list=ls())
graphics.off()

library(psych) # for describe
library(tidyverse) # for tidy code and ggplot
library(cAIC4) # for cAIC
library(r2glmm) # for r2beta
library(lme4) # for lmer
library(lmerTest) # to get singificance test in lmer
library(MuMIn) # for r.squaredGLMM
library(RColorBrewer) #for color palette
library(influence.ME) #for influential outliers
library(lattice) #for qqmath


stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}

df3_prelim <- read_csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_3.csv")

str(df3_prelim)

#Change three variables into factors
df3_prelim <- df3_prelim %>% 
  mutate(ID = factor(ID)) %>% 
  mutate(sex = factor(sex)) %>% 
  mutate(hospital = factor(hospital))

describe(df3_prelim)

#Three levels for sex: one Female capitalized: change into right coding and drop levels
unique(df3_prelim$sex)
df3_prelim$sex[df3_prelim$sex=="Female"] = "female"
df3_prelim$sex <- droplevels(df3_prelim$sex)

#Through describe: max of mindfulness = 6.05
print(df3_prelim[df3_prelim$mindfulness>6,]) #ID 195

#As in first task, this person will be excluded from analysis (if there is one mistake in this row,
#there could be more)

df3 <- df3_prelim[-195,]

#describe the data set
n3 <- nrow(df3)
mean(df3$age)
sd(df3$age)
table(df3$sex)

############### Model 1 with random intercept ##################

mod_rnd_inter <- lmer(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum
                      + mindfulness + (1|hospital), data=df3)

################ Random Intercept Model: Diagnostics ###############

#Influential outliers for group
influence_group <- influence(mod_rnd_inter, group="hospital")$alt.fixed

data_plot_influence_group = as_tibble(influence_group) %>% 
  gather(colnames(influence_group), value = coefficient, key = predictor)

data_plot_influence_group %>% 
  ggplot() + 
  aes(x = 1, y = coefficient, group = predictor) + 
  geom_violin() + 
  facet_wrap(~ predictor, scales = "free")

#Influence observation
influence_observation <- influence(mod_rnd_inter, obs=T)$alt.fixed
 
data_plot_influence_obs = as_tibble(influence_observation) %>% 
  gather(colnames(influence_observation), value= coefficient, key=predictor)

data_plot_influence_obs %>% 
  ggplot() + 
  aes(x = 1, y = coefficient, group = predictor) + 
  geom_violin() + 
  facet_wrap(~ predictor, scales = "free")

#No outliers can be detected

#Normality:
qqmath(mod_rnd_inter, id = 0.05)
qqmath(ranef(mod_rnd_inter))
#Normality seems to be given

#Linearity:
plot(mod_rnd_inter, arg = "pearson")
ggplot(df3, aes(x=pain, y= residuals(mod_rnd_inter))) +
  geom_point()
ggplot(df3, aes(x=sex, y= residuals(mod_rnd_inter))) +
  geom_point()
ggplot(df3, aes(x=age, y= residuals(mod_rnd_inter))) +
  geom_point()
ggplot(df3, aes(x=STAI_trait, y= residuals(mod_rnd_inter))) +
  geom_point()
ggplot(df3, aes(x=pain_cat, y= residuals(mod_rnd_inter))) +
  geom_point()
ggplot(df3, aes(x=cortisol_serum, y= residuals(mod_rnd_inter))) +
  geom_point()
ggplot(df3, aes(x=mindfulness, y= residuals(mod_rnd_inter))) +
  geom_point()
ggplot(df3, aes(x=hospital, y= residuals(mod_rnd_inter))) +
  geom_point()
#Linearity can be assumed (at least no curvuture in predictors)

#Homeoscedasticity is given
plot(mod_rnd_inter, arg = "pearson")

#Multicollinearity is fine, STAI trait rather high (2.12), but still under cut-off
vif(mod_rnd_inter)

############ Evaluating Random Intercept Model ###############
summary(mod_rnd_inter) #predictors
confint(mod_rnd_inter) #confident intervals
stdCoef.merMod(mod_rnd_inter) #beta

r2beta(mod_rnd_inter, method = "nsj", data = df3) #marginal r^2 with CI

r.squaredGLMM(mod_rnd_inter) #marginal and conditional r^2


################## Prediction on new data ##############

df4_prelim <- read_csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_4.csv")

str(df4_prelim)

df4_prelim <- df4_prelim %>% 
  mutate(ID = factor(ID)) %>% 
  mutate(sex = factor(sex)) %>% 
  mutate(hospital = factor(hospital))

describe(df4_prelim)

#Once again two people with negative household income, which is possible (see script of task 1)
df4_prelim[df4_prelim$household_income<0,]
#other than that: everything fine

df4 <- df4_prelim

n4 <- nrow(df4)
mean(df4$age)
sd(df4$age)
table(df4$sex)

#For TSS: compute null model using only the mean
mod_mean <- lm(pain ~ 1, data=df4)

#Prediction columns for random intercept model and null model
df4 <- df4 %>% 
  mutate(pred_rnd_inter = predict(mod_rnd_inter, newdata=df4, allow.new.levels=T)) %>% 
  mutate(pred_null = predict(mod_mean, newdata=df4, allow.new.levels=T))


TSS <- sum((df4$pred_null - df4$pain)^2) #total sum of squares with the null model
RSS <- sum((df4$pred_rnd_inter - df4$pain)^2) #residual sum of squares with random intercept model
(R2 = 1 - (RSS/TSS)) #calculate R^2



################ New Model with random slope ##############

#strongest predictor of the model with random intercept: serum cortisol

mod_rnd_slope <- lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data=df3)

#Model diagnostics
influence_group_slope <- influence(mod_rnd_slope, group="hospital")$alt.fixed

data_plot_influence_group_slope = as_tibble(influence_group_slope) %>% 
  gather(colnames(influence_group_slope), value = coefficient, key = predictor)

data_plot_influence_group_slope %>% 
  ggplot() + 
  aes(x = 1, y = coefficient, group = predictor) + 
  geom_violin() + 
  facet_wrap(~ predictor, scales = "free")

#Influence observation
influence_observation_slope <- influence(mod_rnd_slope, obs=T)$alt.fixed

data_plot_influence_obs_slope = as_tibble(influence_observation_slope) %>% 
  gather(colnames(influence_observation_slope), value= coefficient, key=predictor)

data_plot_influence_obs_slope %>% 
  ggplot() + 
  aes(x = 1, y = coefficient, group = predictor) + 
  geom_violin() + 
  facet_wrap(~ predictor, scales = "free")
#No outliers can be detected

#Normality:
qqmath(mod_rnd_slope, id = 0.05)
qqmath(ranef(mod_rnd_slope))
#More or less okay (cortisol serum quite good actually)

#Linearity:
plot(mod_rnd_slope, arg = "pearson")
ggplot(df3, aes(x=pain, y= residuals(mod_rnd_slope))) +
  geom_point()
ggplot(df3, aes(x=cortisol_serum, y= residuals(mod_rnd_slope))) +
  geom_point()
#Linearity given

#Homeoscedasticity is given
plot(mod_rnd_slope, arg = "pearson")

#Multicollinearity impossible (only one predictor)

#Assessing model
summary(mod_rnd_slope) #predictors
confint(mod_rnd_slope) #confident intervals
stdCoef.merMod(mod_rnd_slope) #beta

r2beta(mod_rnd_slope, method = "nsj", data = df3) #marginal r^2 with CI
r.squaredGLMM(mod_rnd_slope) #marginal and conditional r^2

#predict values with the random slope model 
df3 <- df3 %>% 
  mutate(pred_rnd_slope = predict(mod_rnd_slope, newdata=df3, allow.new.levels=T))

#Recode to make the headings look nicer
df3$hospital <- recode(df3$hospital, "hospital_1" = "Hospital 01", 
       "hospital_2" = "Hospital 02" , "hospital_3" = "Hospital 03" , "hospital_4" = "Hospital 04",
       "hospital_5" ="Hospital 05",  "hospital_6" = "Hospital 06", "hospital_7" ="Hospital 07",
       "hospital_8" = "Hospital 08", "hospital_9" = "Hospital 09", "hospital_10" = "Hospital 10" )

#Rearrange the order of values so that Hospital 10 does not appear in the plot after 1
levels(df3$hospital) <- c("Hospital 1", "Hospital 2", "Hospital 3", "Hospital 4", "Hospital 5", "Hospital 6",
                         "Hospital 7", "Hospital 8", "Hospital 9", "Hospital 10")

#Prepare the colors for the plot
display.brewer.all()
colors <- brewer.pal(n=10, name="Paired")

#Plot
gg_rnd_slope <- ggplot(df3, aes(x=cortisol_serum, y=pain, group=hospital, color=hospital)) +
  geom_point() +
  geom_line(color = "black", aes(y = pred_rnd_slope, x = cortisol_serum)) +
  facet_wrap(~hospital, ncol=2) +
  theme_bw() +
  theme(legend.position="none") +
  labs(x="Serum Cortisol", y="Reported Pain") +
  scale_color_manual(values = colors)
gg_rnd_slope

#ggsave("ZK3_rnd_slope.png", path= "~/Documents/Master/Statistics /Assignment Writing", 
#       plot = gg_rnd_slope, width = 25, height = 30, units = "cm")

################## For Discussion #################

#Sex and mindfulness differing between hospitals?
hospitals_comp <- df3 %>% 
  group_by(hospital) %>% 
  summarize(female = sum(sex=="female"), male = sum(sex=="male"), mindfulness = mean(mindfulness))

