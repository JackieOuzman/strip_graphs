library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
#install.packages("PairedData")
library(PairedData)
### New approach using polygon to pull out raw yield data

##Bring in my data

name_of_path <-
  file.path("W:", "value_soil_testing_prj", "Yield_data", "Tim_McClelland", "Clover",  "Clover_yld_data0_50_100_seg_ID_zone1.csv")
clover_seg_ID <-read_csv(name_of_path)
str(clover_seg_ID)
## remove all the values in the data set that wont be included in the analysis this is when distance on line = 0

clover_seg_ID <- filter(clover_seg_ID,
                        DistOnLine != 0)
clover_seg_ID$P_rate_as_factor <- as.factor(clover_seg_ID$P_Rate)

group_by(clover_seg_ID, P_Rate) %>%
  summarise(
    count = n(),
    mean = mean(Yld_Mass_D, na.rm = TRUE),
    sd = sd(Yld_Mass_D, na.rm = TRUE)
  )
# Plot Yld_Mass_D by P_Rate and color by group
 ggplot( clover_seg_ID, aes(P_rate_as_factor, Yld_Mass_D))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,))+
  labs(x = "P rate",
       y= "Yield t/ha")
 
 # http://www.sthda.com/english/wiki/paired-samples-t-test-in-r
 
 
 #1. Plot paired data:
 # Subset Yield data 0 P rate 
 P_rate0 <- subset(clover_seg_ID,  P_Rate == 0, Yld_Mass_D,
                  drop = TRUE)
 # Subset Yield data 50 P rate 
 P_rate50 <- subset(clover_seg_ID,  P_Rate == 50, Yld_Mass_D,
                   drop = TRUE)
 # Subset Yield data 100 P rate 
 P_rate100 <- subset(clover_seg_ID,  P_Rate == 100, Yld_Mass_D,
                    drop = TRUE)
 
 # Plot paired data
 library(PairedData)
 pd <- paired(P_rate0, P_rate100)
 plot(pd, type = "profile") + theme_bw()
 
 #test assumptions
 
 
 # compute the difference
 d <- with(clover_seg_ID, 
           Yld_Mass_D[P_Rate == 0] - Yld_Mass_D[P_Rate == 50])
 # Shapiro-Wilk normality test for the differences
 shapiro.test(d) # => p-value = 2.2e-16
 
 # I think this means that it is not normally distrubuted
 
 # Compute t-test
 clover_seg_ID_0vs50 <- filter(clover_seg_ID, P_Rate != 100 )
 ggplot( clover_seg_ID_0vs50, aes(P_rate_as_factor, Yld_Mass_D))+
   geom_boxplot(alpha=0.1)+
   geom_point(colour = "blue", alpha = 0.1)+
   theme_bw()+
   theme(axis.text=element_text(size=8),
         axis.title=element_text(size=10,))+
   labs(x = "P rate",
        y= "Yield t/ha")
 
 
res <- t.test(Yld_Mass_D ~ P_Rate, data = clover_seg_ID_0vs50, paired = TRUE)
 res

 
