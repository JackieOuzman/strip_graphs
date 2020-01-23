library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
#install.packages("PairedData")
library(PairedData)
### New approach using polygon to pull out raw yield data

##Bring in my data...

name_of_path <-
  file.path("W:", "value_soil_testing_prj", "Yield_data", "Tim_McClelland", "Clover",  "Clover_yld_data0_50_100_seg_ID_zone1.csv")
clover_seg_ID <-read_csv(name_of_path)
str(clover_seg_ID)
## remove all the values in the data set that won't be included in the analysis this is when distance on line = 0

clover_seg_ID <- filter(clover_seg_ID,
                        DistOnLine != 0)

#The farmer practice wasnt really a true strip but I want to use the data so I need to remove row when we have no yield

clover_seg_ID <- filter(clover_seg_ID,
                        Yld_Mass_D != 0)

clover_seg_ID$P_rate_as_factor <- as.factor(clover_seg_ID$P_Rate)


##### Method 2 Via Andrea ####

#Prep the data so I can check
dim(clover_seg_ID) #5030
clover_seg_ID_0vs50 <- filter(clover_seg_ID, P_Rate != 100 )
clover_seg_ID_100vs50 <- filter(clover_seg_ID, P_Rate != 0 )


# # a small subset that has equal number of values per treatment
# test <- filter(clover_seg_ID_0vs100, SegmentID == 100:103)
# count(filter(test, SegmentID == 101 &  P_Rate == 0))
# count(filter(test, SegmentID == 101 &  P_Rate == 100))
# count(filter(test, SegmentID == 100 &  P_Rate == 0))
# count(filter(test, SegmentID == 100 &  P_Rate == 100))


# Compute t-test - this is done for 0 vs 100 for each segment values in the strip
# Basically what you need to do is subset your data based on segmentID and perform the t-test for each group of data.
# 
# Something like

Output= data.frame() #create empty df for output
max(clover_seg_ID_100vs50$SegmentID)
min(clover_seg_ID_100vs50$SegmentID)


#Now put it in the loop with my test data! - its not looping through everything ? try making a list
for (i in min(clover_seg_ID_100vs50$SegmentID):max(clover_seg_ID_100vs50$SegmentID)){
  #print(paste("segment_number is ", i))
  segment_data = subset(clover_seg_ID_100vs50, SegmentID == i)
  
  # Method 1: The data are saved in two different numeric vectors.
  data_x=subset(segment_data, P_Rate==50, select = Yld_Mass_D, drop = TRUE)
  data_y=subset(segment_data, P_Rate==100, select = Yld_Mass_D, drop = TRUE)
  res_method1 <-t.test(data_x,data_y, var.equal = FALSE)
  
  p_vlaue <- res_method1$p.value
  segment_name <- unique(segment_data$SegmentID)
  result <- data.frame(SegmentID = segment_name, P_value = p_vlaue)
  Output = rbind(Output, result)
} 


#To make this meaningful I need to summaries the input data and join it to the t - test results

clover_seg_ID_100vs50_summary <- group_by(clover_seg_ID_100vs50,
                                          SegmentID, Farmer, Paddock, zone, P_rate_as_factor ) %>% 
  summarise_all(mean) %>% 
  ungroup()
#join output to summary
str(clover_seg_ID_100vs50_summary)
str(Output)
#what comparison did I run? - name the df to rflect this
clover_seg_ID_100vs50_summary <- left_join(clover_seg_ID_100vs50_summary, Output)
clover_seg_ID_100vs50_summary <- mutate(clover_seg_ID_100vs50_summary, comparison = "P50vsP100" )

unique(clover_seg_ID_100vs50_summary$zone)

### Plot the results 
ggplot(clover_seg_ID_100vs50_summary, aes(SegmentID , Yld_Mass_D, group = P_rate_as_factor))+
  geom_line(size=2, alpha=0.4, aes( color = P_rate_as_factor ))+
  scale_color_manual(values=c('darkgrey','green'))+
  #scale_color_manual(values=c('darkgrey','green', "black", "blue"))+
  theme_bw()+
  labs(x= "distance along the strip",
       y = "yield t/ha",
       title = "",
       subtitle = "",
       caption = "")+
  theme(legend.position = "none") +
  geom_point(data = filter(clover_seg_ID_100vs50_summary, zone == "Low"), aes(SegmentID, Yld_Mass_D), shape=1, size =2)#+
  #geom_point(data = filter(clover_seg_ID_100vs50_summary, zone == 2), aes(SegmentID, Yld_Mass_D), shape=1, size =2)









###Break down the loop to make sure it works

# segment_data = subset(test, SegmentID == 100)
# unique(segment_data$P_Rate)
# count(filter(segment_data, P_Rate == 0))
# count(filter(segment_data, P_Rate == 100))

# #Method 1: The data are saved in two different numeric vectors.
#  data_x=subset(segment_data, P_Rate==0, select = Yld_Mass_D, drop = TRUE)
#  data_y=subset(segment_data, P_Rate==100, select = Yld_Mass_D, drop = TRUE)
#  #t_test = t.test(data_x, data_y) #(not working)
#  t.test.paired(data_x, data_y, var.equal = FALSE) #(not working - t.test is only suitable to numeric paired data)
#  res_meth1 <- t.test(data_x,data_y, var.equal = FALSE)
#  p_vlaue <- res_meth1$p.value
#  print(p_vlaue)
#  segment_name <- unique(segment_data$SegmentID)
#  result <- data.frame(segmentID = segment_name, P_value = p_vlaue)
#  print(result)
#  


#  #Method 2: The data are saved in a data frame
# res <- t.test(Yld_Mass_D ~ P_Rate, data = segment_data, paired = TRUE)
# p_vlaue <- res$p.value
# segment_name <- unique(segment_data$SegmentID)
# result <- data.frame(SegmentID = segment_name, P_value = p_vlaue)
# print(result)




##### Paired t test for whole strip ####

##Do I need to average the yield values in each line segment?

clover_seg_ID_av <- group_by(clover_seg_ID,SegmentID, Farmer, Paddock, zone, P_rate_as_factor ) %>% 
  summarise_all(mean)
head(clover_seg_ID_av)
head(clover_seg_ID)

group_by(clover_seg_ID_av, P_Rate) %>%
  summarise(
    count = n(),
    mean = mean(Yld_Mass_D, na.rm = TRUE),
    sd = sd(Yld_Mass_D, na.rm = TRUE)
  ) %>% 
  ungroup()
# Plot Yld_Mass_D by P_Rate and color by group
 ggplot( clover_seg_ID_av, aes(P_rate_as_factor, Yld_Mass_D))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  theme_bw()+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,))+
  labs(x = "P rate",
       y= "Yield t/ha")
 
 # http://www.sthda.com/english/wiki/paired-samples-t-test-in-r
 
 
 
 #test assumptions
 
 
 # compute the difference
 d <- with(clover_seg_ID_av, 
           Yld_Mass_D[P_Rate == 0] - Yld_Mass_D[P_Rate == 100])
 # Shapiro-Wilk normality test for the differences
 shapiro.test(d) # => p-value = 0.2928
 
 # I think this means that it is  normally distrubuted
 
 # Compute t-test - this is done for 0 vs 100 for all values in the strip
 clover_seg_ID_av_0vs100 <- filter(clover_seg_ID_av, P_Rate != 50 )
 ggplot( clover_seg_ID_av_0vs100, aes(P_rate_as_factor, Yld_Mass_D))+
   geom_boxplot(alpha=0.1)+
   geom_point(colour = "blue", alpha = 0.1)+
   theme_bw()+
   theme(axis.text=element_text(size=8),
         axis.title=element_text(size=10,))+
   labs(x = "P rate",
        y= "Yield t/ha")
 
 res <- t.test(Yld_Mass_D ~ P_Rate, data = clover_seg_ID_av_0vs100, paired = TRUE)
 res


 

 
