#------------------------------------------------#
#         Home Assignment Geoffrey 2
#------------------------------------------------#
rm(list=ls())
graphics.off()

library(tidyr)
library(psych)
library(ggplot2)
library(smacof) #for converting similarity into dissimilarity
library(MASS) #For Shepard
library(ggrepel) #to avoid a crammed plot

setwd("~/Documents/Master/Statistics /Exams")

nations <- read.delim("~/Documents/Master/Statistics /Exams/Nations.txt")

#Transform into dissimilarity matrix
nations.d <- sim2diss(nations, method = 9, to.dist=T)

#Using non-metric multidimensional scaling since the data is ordinal (p.121) 
nations_mds <- isoMDS(nations.d) #Stress: 18.86
nations_mds_3 <- isoMDS(nations.d, k=3) #Stress: 10.83
nations_mds_4 <- isoMDS(nations.d, k=4) #Stress: 5.42

str(nations_mds)
nations_mds 

#Shepard plot
nations_shep <- Shepard(nations.d, nations_mds$points)

png(file="~/Documents/Master/Statistics /Assignment Writing/GP2_Shepard.png",
    width=25, height=16, units="cm", res=100)
plot(nations_shep, pch = 20 , xlab = "Dissimilarity", ylab = "Distance", 
     xlim = range(nations_shep$x), ylim = range(nations_shep$x)) #plot itself
lines(nations_shep$x, nations_shep$yf, type = "S") #adds lines to the plot above
dev.off()


#Plot the MDS itself

#For the color-aid with groups of nation: specify those three groups
country <- rownames(nations)
group <- as.factor(c(3, 3, 2, 3, 1, 3, 1, 1, 2, 2, 1, 2))
groups <- data.frame(country, group)
countries_group <- cbind(nations, groups)

#Plot
nations_mds_df <- data.frame(nations_mds)
plot_nations <- ggplot(data = nations_mds_df, aes(x = points.1, y = points.2, label = rownames(nations_mds_df), color=countries_group$group)) + 
  geom_text_repel(alpha = 0.8, size = 6) +
  theme_bw() +
  ylim(-3, 3) +
  xlim(-3, 3) +
  labs(x = "Coordinate 1", y="Coordinate 2") +
  scale_color_manual(name = "Country Groups",values=c("orange2", "red3", "blue3"), breaks=c("1", "2", "3"), 
                    labels = c("Western Capitalist", "Socialist/Communist", "Developing"))

ggsave("GP2_MDS.png", path= "~/Documents/Master/Statistics /Assignment Writing", 
       plot = plot_nations, width = 25, height = 16, units = "cm")
