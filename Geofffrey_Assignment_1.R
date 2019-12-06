#------------------------------------------------#
#         Home Assignment Geoffrey 1
#------------------------------------------------#
rm(list=ls())
graphics.off()

library(tidyr)
library(psych)
library(factoextra)

setwd("~/Documents/Master/Statistics /Exams")

#################### Data Preparation ##################

#Loading in the data
paq_long <- read.csv("~/Documents/Master/Statistics /Exams/PAQ_Marius.txt", sep="")

#Data Inspection
str(paq_long)
unique(paq_long$var) #to see all variables

#Convert from long to wide format
paq_all <- spread(paq_long, var, value)
str(paq_all) #all variables num or int (also sex, but that is not used for PCA anyway)
#Use describe from psych for a quick overview
describe(paq_all)
#NAs in Q2 - Q6, otherwise: all variables within their ranges, means seem okay, too
#Inspecting the items in the task, it becomes clear that they are all coded in the same direction and that
#higher values on each item means a higher anxiety in regard to PSYP13

#Replace NAs with mean
paq_all$Q2_help[is.na(paq_all$Q2_help)] <- mean(paq_all$Q2_help, na.rm=T)
paq_all$Q3_breathe[is.na(paq_all$Q3_breathe)] <- mean(paq_all$Q3_breathe, na.rm=T)
paq_all$Q4_freeze[is.na(paq_all$Q4_freeze)] <- mean(paq_all$Q4_freeze, na.rm=T)
paq_all$Q5_alien[is.na(paq_all$Q5_alien)] <- mean(paq_all$Q5_alien, na.rm=T)
paq_all$Q6_inferior[is.na(paq_all$Q6_inferior)] <- mean(paq_all$Q6_inferior, na.rm=T)

describe(paq_all) #no NAs anymore

#Sex seems to have three levels
table(paq_all$sex) #0:180, 1:113, 2:7
#Codes not explained in task, it is assumed that 0 = Female (there are more female than male students
#in psychology), 1 = Male, 2 = Other/no reply

#For further analysis: id, age and sex not needed
paq <- paq_all[,-c(1:2, 12)]
describe(paq) #control whether variables were deleted

######################## PCA ######################

#For inspection: covariance and correlation matrix
cov(paq)
cor(paq)
#All values bigger than .3, therefore it makes sense to do PCA

#Plotting does not give a lot of information, since all data points need to be between 1 and 5
plot(paq)

#Making the PCA, on the correlation matrix
paq_pca <- princomp(paq, cor=T)
str(paq_pca)

#Checking centering and scaling
paq_pca$center #means on all items
paq_pca$scale #SDs on all items

#Check components and loadings of items on components
summary(paq_pca, loadings=T)
get_eigenvalue(paq_pca)

#Scree Plot
scree <- fviz_eig(paq_pca, barfill = "dodgerblue3", barcolor="dodgerblue3", 
                  xlab = "Component")
scree
ggsave("GP1_Scree.png", path= "~/Documents/Master/Statistics /Assignment Writing", 
       plot = scree, width = 25, height = 16, units = "cm")

mean(paq_pca$sdev^2) #calculating mean eigenvalue, which is 1 because PCA was performed on correlation matrix


#Biplots with only variables and with participants' scores
bi_variables <- fviz_pca_var(paq_pca, col.var = "contrib", gradient.cols = c("#bb2e00", "#002bbb"), repel=T, 
                             select.var = list(name=NULL))
ggsave("GP1_Variables.png", path= "~/Documents/Master/Statistics /Assignment Writing", 
       plot = bi_variables, width = 25, height = 16, units = "cm")
bi_individuals <- fviz_pca_biplot(paq_pca, geom="point", repel=T, col.var="grey39", col.ind="dodgerblue3")
ggsave("GP1_Individuals.png", path= "~/Documents/Master/Statistics /Assignment Writing", 
       plot = bi_individuals, width = 25, height = 16, units = "cm")
