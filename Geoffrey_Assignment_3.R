#------------------------------------------------#
#         Home Assignment Geoffrey 3
#------------------------------------------------#
rm(list=ls())
graphics.off()
setwd("~/Documents/Master/Statistics /Exams")
library(ggplot2)


#write function for simulating data with a given n_group and output only the p-value
aov_simulation <- function(n_group){
      group1 <- rnorm(n = n_group, mean = -.5, sd= 1)
      group2 <- rnorm(n = n_group, mean = 0, sd= 1)
      group3 <- rnorm(n = n_group, mean = 0, sd= 1)
      group4 <- rnorm(n = n_group, mean = .5, sd= 1)
      values <- c(group1, group2, group3, group4)
      subject <- rep(seq(1,n_group),4)
      subject <- factor(subject)
      group <- c(rep("G1", n_group), rep("G2", n_group), rep("G3", n_group), rep("G4",n_group))
      df <- data.frame(subject, group, values)
      return(summary(aov(values ~ group, data=df))[[1]][["Pr(>F)"]][[1]])
  }

n_group <- rep(10:50, each = 100) #vector with each group size 100 times
p_value <- lapply(n_group, aov_simulation) #use lapply to do the simulation for each element in vector (therefore each group size 100 times)
p_vector <- unlist(p_value, use.names=F) #transform output list into vector
results <- data.frame(n_group, p_vector) #get the group size and p-values together

#To see lowest and highest p for each group size
lowest_highest_p <- results %>% 
  group_by(n_group) %>% 
  summarize(low = min(p_vector), high = max(p_vector))

#Calculate noncentral f-distribution's value for each sample size
output <- results %>% 
  group_by(n_group) %>%                                                   #group by sample size for each group
  summarize(sign = sum(p_vector < .05)) %>%                              #how many p<.05 for each sample size
  mutate(lambda = (n_group * 4)*(0.35^2)) %>%                             #lambda: N*f^2
  mutate(df_num = 3) %>%                                                  #in numerator: always df=3
  mutate(df_den = n_group*4 - 4) %>%                                      #in denominator: N-k
  mutate(f_crit = qf(p = .95, df1 = df_num, df2 = df_den)) %>%            #get critical F-value for each group size
  mutate(power = 1-(pf(q = f_crit, df1=df_num, df2=df_den, ncp=lambda)))  #get power through non-central F-distribution
output

#Making the plot
gg_successes <- ggplot(output, aes(x=n_group, y=sign)) +
  geom_bar(stat="identity", fill="dodgerblue3", alpha=.9) +
  theme_bw() +
  labs(x="Participants per Group", y="Count of Significant Results &\nPower*100") +
  geom_line(aes(x=n_group, y = power*100), size=1.1)  +
  scale_y_continuous(breaks=c(0,20,40,60,80,100))
gg_successes  

ggsave("GP3_graph.png", path= "~/Documents/Master/Statistics /Assignment Writing", 
       plot = gg_successes, width = 25, height = 16, units = "cm")
  
