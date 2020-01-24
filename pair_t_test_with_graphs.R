library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
#install.packages("PairedData")
library(PairedData)
library(cowplot)
library(grid)
### New approach using polygon to pull out raw yield data

##Bring in my data...

name_of_path <-
  file.path("W:", "value_soil_testing_prj", "Yield_data", "Tim_McClelland", "Clover",  "Yld_clover_Trim_join.csv")
seg_ID <-read_csv(name_of_path)

unique(seg_ID$zone)
str(seg_ID)

## remove all the values in the data set that won't be included in the analysis this is when distance on line = 0

seg_ID <- filter(seg_ID,
                        DistOnLine != 0)

#The farmer practice wasnt really a true strip but I want to use the data so I need to remove row when we have no yield

seg_ID <- filter(seg_ID,
                        Yld_Mass_D != 0)




##### t test per segment in the strip Via Andrea method####

#Prep the data so I can check

seg_ID_0vs50 <- filter(seg_ID, P_Rate != 100 )
seg_ID_100vs50 <- filter(seg_ID, P_Rate != 0 ) 


# Compute t-test - this is done for 0 vs 100 for each segment values in the strip
# Basically what you need to do is subset your data based on segmentID and perform the t-test for each group of data.
# 

#Now put it in the loop with my test data! - its not looping through everything ? try making a list

#I want a list of all values in segment ID to uss in the loop
list <- unique(seg_ID_0vs50$SegmentID)
print(list)

#Run as a loop for 0_50
Output_0vs50= data.frame() #create empty df for output
for (i in list){
  segment_data = subset(seg_ID_0vs50, SegmentID == i)
  
  # Method 1: The data are saved in two different numeric vectors.
  data_x=subset(segment_data, P_Rate==50, select = YLDMASSDR, drop = TRUE)
  data_y=subset(segment_data, P_Rate==0, select = YLDMASSDR, drop = TRUE)
  res_method1 <-t.test(data_x,data_y, var.equal = FALSE)
  
  p_vlaue <- res_method1$p.value
  segment_name <- unique(segment_data$SegmentID)
  result <- data.frame(SegmentID = segment_name, P_value = p_vlaue)
  Output_0vs50 = rbind(Output_0vs50, result)
} 

#convert the P value into NS or Sig at 0.05
Output_0vs50 <- mutate(Output_0vs50, 
               ns_sig = case_when(
                 P_value < 0.05 ~ "significant",
                 TRUE ~ "not_significant"
               ))
#To make this meaningful I need to summaries the input data and join it to the t - test results

seg_ID_0vs50_summary <- group_by(seg_ID_0vs50,
                                          SegmentID, Farmer, Paddock, zone, P_Rate ) %>% 
  summarise_all(mean) %>% 
  ungroup()
#join output to summary

#what comparison did I run? - name the df to reflect this
seg_ID_0vs50_summary <- left_join(seg_ID_0vs50_summary, Output_0vs50)
seg_ID_0vs50_summary <- mutate(seg_ID_0vs50_summary, comparison = "P0vsP50" )



#Run as a loop for 100_50
Output_100vs50= data.frame() #create empty df for output
for (i in list){
  segment_data = subset(seg_ID_100vs50, SegmentID == i)
  
  # Method 1: The data are saved in two different numeric vectors.
  data_x=subset(segment_data, P_Rate==50, select = YLDMASSDR, drop = TRUE)
  data_y=subset(segment_data, P_Rate==100, select = YLDMASSDR, drop = TRUE)
  res_method1 <-t.test(data_x,data_y, var.equal = FALSE)
  
  p_vlaue <- res_method1$p.value
  segment_name <- unique(segment_data$SegmentID)
  result <- data.frame(SegmentID = segment_name, P_value = p_vlaue)
  Output_100vs50 = rbind(Output_100vs50, result)
} 

#convert the P value into NS or Sig at 0.05
Output_100vs50 <- mutate(Output_100vs50, 
                       ns_sig = case_when(
                         P_value < 0.05 ~ "significant",
                         TRUE ~ "not_significant"
                       ))

#To make this meaningful I need to summaries the input data and join it to the t - test results
seg_ID_100vs50_summary <- group_by(seg_ID_100vs50,
                                 SegmentID, Farmer, Paddock, zone, P_Rate ) %>% 
  summarise_all(mean) %>% 
  ungroup()
#join output to summary
seg_ID_100vs50_summary <- left_join(seg_ID_100vs50_summary, Output_100vs50)
#what comparison did I run? - name the df to reflect this
seg_ID_100vs50_summary <- mutate(seg_ID_100vs50_summary, comparison = "P100vsP50" )


##Join the two strip data results togther
seg_ID_0vs50_100s50summary <- rbind(seg_ID_0vs50_summary, seg_ID_100vs50_summary)


seg_ID_0vs50_100s50summary$P_Rate_as_factor <- as.factor(seg_ID_0vs50_100s50summary$P_Rate)
str(seg_ID_0vs50_100s50summary)



### Plot the results 
segments <- ggplot(seg_ID_0vs50_100s50summary, aes(SegmentID , YLDMASSDR, group = P_Rate_as_factor))+
  geom_line(size=2, alpha=0.4, aes( color = P_Rate_as_factor ))+
  scale_color_manual(values=c('darkgrey','green', 'blue'))+
  #scale_color_manual(values=c('darkgrey','green', "black", "blue"))+
  theme_bw()+
  labs(x= "distance along the strip",
       y = "yield t/ha",
       title = "",
       subtitle = "",
       caption = "")+
 # theme(legend.position = "none") +
 geom_point(data = filter(seg_ID_0vs50_100s50summary, zone == "Low"), aes(SegmentID, YLDMASSDR), shape=1, size =2)
segments

#create a table that count how many signifcant vs non significant values we have
table_segments <- group_by(seg_ID_0vs50_100s50summary,comparison,   ns_sig ) %>% 
  count(ns_sig) %>% 
  summarise(count = n/2)

##save the results of the segment work
segments
graph_path <- file.path("//FSSA2-ADL/clw-share1/Microlab/value_soil_testing_prj/Yield_data/Tim_McClelland/Clover")
ggsave(path= graph_path, filename = "t-test_segments.png", device = "png" ,
       width = 20, height = 10, units = "cm")

write.csv(seg_ID_0vs50_100s50summary, "//FSSA2-ADL/clw-share1/Microlab/value_soil_testing_prj/Yield_data/Tim_McClelland/Clover/t_test_segments.csv")
write.csv(table_segments, "//FSSA2-ADL/clw-share1/Microlab/value_soil_testing_prj/Yield_data/Tim_McClelland/Clover/table_segments.csv")





##### Paired t test for whole strip ####

##average the yield values in each line segment - this ensure I have the same number of points
str(seg_ID)

strip_av <- group_by(seg_ID,SegmentID, Farmer, Paddock, zone, P_Rate ) %>% 
  summarise_all(mean)


strip_av_av$P_Rate_as_factor <- as.factor(strip_av$P_Rate)
strip_av_av_0vs50 <- filter(strip_av, P_Rate != 100 )
strip_av_av_100vs50 <- filter(strip_av, P_Rate != 0 )
 # http://www.sthda.com/english/wiki/paired-samples-t-test-in-r
 
 #test assumptions
 
 
 # compute the difference
 d <- with(clover_seg_ID_av, 
           YLDMASSDR[P_Rate == 0] - YLDMASSDR[P_Rate == 50])
 # Shapiro-Wilk normality test for the differences
 shapiro.test(d) # => p-value = 0.5161
 
 # I think this means that it is  normally distrubuted
 
 strip_avr_res0vs50 <- t.test(YLDMASSDR ~ P_Rate, data = strip_av_av_0vs50, paired = TRUE)
 strip_av_res100vs50 <- t.test(YLDMASSDR ~ P_Rate, data = strip_av_av_100vs50, paired = TRUE)
 
 #p_vlaue <- signif(res$p.value, digits = 2)
 
 p_vlaue0vs50_strip <- format(strip_avr_res0vs50$p.value, scientific = FALSE)
 p_vlaue0vs100_strip <- format(strip_av_res100vs50$p.value, scientific = FALSE)
 p_vlaue_text_strip <- paste0("P value 0 vs 50 = ", p_vlaue0vs50_strip, "\n",
                        "P value 0 vs 100 = ", p_vlaue0vs100_strip, collapse = "\n")
 print(p_vlaue_text_strip)

 Pvalue_on_graph <- grobTree(textGrob(p_vlaue_text_strip, x=0.1,  y=0.90, hjust=0,
                                      gp=gpar(col="black", fontsize=6, fontface="italic")))
 
 
 str(strip_av)
 strip_av$P_Rate_as_factor <- as.factor(strip_av$P_Rate)
 # Compute t-test - this is done for 0 vs 100 for all values in the strip
 
 ggplot( strip_av, aes(P_Rate_as_factor, YLDMASSDR))+
   geom_boxplot(alpha=0.1)+
   geom_point(colour = "blue", alpha = 0.1)+
   theme_bw()+
   theme(axis.text=element_text(size=8),
         axis.title=element_text(size=10,))+
   labs(x = "P rate",
        y= "Yield t/ha")+
   annotation_custom(Pvalue_on_graph)
 
 
 ##save the results of the whole strip work
 
 graph_path <- file.path("//FSSA2-ADL/clw-share1/Microlab/value_soil_testing_prj/Yield_data/Tim_McClelland/Clover")
 ggsave(path= graph_path, filename = "t-test_whole_strip.png", device = "png" ,
        width = 10, height = 10, units = "cm")
 
 write.csv(strip_av_av, "//FSSA2-ADL/clw-share1/Microlab/value_soil_testing_prj/Yield_data/Tim_McClelland/Clover/t_test_strip_av.csv")

 
 

 ##### Paired t test for zone strip ####
 
 ##average the yield values in each line segment - this ensure I have the same number of points
 str(seg_ID)
 zone <- filter(seg_ID, zone == "Low" )
 zone_av <- group_by(zone,SegmentID, Farmer, Paddock, zone, P_Rate ) %>% 
   summarise_all(mean)
 
 
 zone_av$P_Rate_as_factor <- as.factor(zone_av$P_Rate)
 zone_av_0vs50 <- filter(zone_av, P_Rate != 100 )
 zone_av_100vs50 <- filter(zone_av, P_Rate != 0 )
 # http://www.sthda.com/english/wiki/paired-samples-t-test-in-r
 
 #test assumptions
 
 
 # compute the difference
 d <- with(zone_av, 
           YLDMASSDR[P_Rate == 0] - YLDMASSDR[P_Rate == 50])
 # Shapiro-Wilk normality test for the differences
 shapiro.test(d) # => p-value = 0.5161
 
 # I think this means that it is  normally distrubuted
 
 zone_avres0vs50 <- t.test(YLDMASSDR ~ P_Rate, data = zone_av_0vs50, paired = TRUE)
 zone_avres100vs50 <- t.test(YLDMASSDR ~ P_Rate, data = zone_av_100vs50, paired = TRUE)

 #p_vlaue <- signif(res$p.value, digits = 2)
 
 p_vlaue0vs50_zone <- format(zone_avres0vs50$p.value, scientific = FALSE)
 p_vlaue0vs100_zone <- format(zone_avres100vs50$p.value, scientific = FALSE)
 p_vlaue_text_zone <- paste0("P value 0 vs 50 = ", p_vlaue0vs50_zone, "\n",
                        "P value 0 vs 100 = ", p_vlaue0vs100_zone, collapse = "\n")
 print(p_vlaue_text)
 library(grid)
 Pvalue_on_graph <- grobTree(textGrob(p_vlaue_text_zone, x=0.1,  y=0.90, hjust=0,
                                      gp=gpar(col="black", fontsize=8, fontface="italic")))
 
 
 
 
 # Compute t-test - this is done for 0 vs 100 for all values in the strip
 
 ggplot( zone_av, aes(P_Rate_as_factor, YLDMASSDR))+
   geom_boxplot(alpha=0.1)+
   geom_point(colour = "blue", alpha = 0.1)+
   theme_bw()+
   theme(axis.text=element_text(size=8),
         axis.title=element_text(size=10,))+
   labs(x = "P rate",
        y= "Yield t/ha")+
   annotation_custom(Pvalue_on_graph)
 
 ##save the results of the zone strip work
 
 graph_path <- file.path("//FSSA2-ADL/clw-share1/Microlab/value_soil_testing_prj/Yield_data/Tim_McClelland/Clover")
 ggsave(path= graph_path, filename = "t-test_zone_strip.png", device = "png" ,
        width = 10, height = 10, units = "cm")
 
 write.csv(zone_av, "//FSSA2-ADL/clw-share1/Microlab/value_soil_testing_prj/Yield_data/Tim_McClelland/Clover/t_testzone_av.csv")
 
