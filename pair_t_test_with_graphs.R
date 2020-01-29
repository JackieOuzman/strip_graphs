library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
#install.packages("PairedData")
library(PairedData)
library(cowplot)
library(grid)

#install.packages("RGraphics")
#install.packages("gridExtra")
library(RGraphics)
library(gridExtra)
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


###########################################################################################################################

##### t test per segment in the strip Via Andrea method####

#Prep the data so I can check what am I testing (look at Harms list)

seg_ID_0vs50 <- filter(seg_ID, P_Rate != 100 )
seg_ID_100vs50 <- filter(seg_ID, P_Rate != 0 ) 


# Compute t-test - this is done for 0 vs 100 for each segment values in the strip
# Basically what you need to do is subset your data based on segmentID and perform the t-test for each group of data.
# 

#Now put it in the loop with my test data! - its not looping through everything ? try making a list

#I want a list of all values in segment ID to uss in the loop
list <- unique(seg_ID_0vs50$SegmentID)

############################################################################################################
#Run as a loop for 0_50 test 1
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
                 Significant = case_when(
                 P_value < 0.05 ~ "significant",
                 TRUE ~ "not significant"
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


#####################################################################################################
#Run as a loop for 100_50 test 2
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
                       Significant = case_when(
                         P_value < 0.05 ~ "significant",
                         TRUE ~ "not significant"
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

###############################################################################################################

##Join the two strip data results togther join info from test 1 to test 2
seg_ID_0vs50_100s50summary <- rbind(seg_ID_0vs50_summary, seg_ID_100vs50_summary)

###remove some of the data from my workspace
rm(list = c("Output_0vs50", 
            "Output_100vs50", 
            "seg_ID_100vs50_summary", 
            "res_method1",
            "result", 
            "seg_ID_0vs50", 
            "seg_ID_100vs50", 
            "seg_ID_100vs50_summary",
            "seg_ID_0vs50_summary")) 


##############################################################################################################
#########    plot results  of t.test ########################################################################
seg_ID_0vs50_100s50summary$P_Rate_as_factor <- as.factor(seg_ID_0vs50_100s50summary$P_Rate)
str(seg_ID_0vs50_100s50summary)

#what is the area of the zones this can be added to the graph?
filter(seg_ID_0vs50_100s50summary, zone == "Low") %>% 
  summarise(min_zone = min(SegmentID),
            max_zone = max(SegmentID))
#create a table that count how many signifcant vs non significant values we have
table_segments <- group_by(seg_ID_0vs50_100s50summary,comparison,   ns_sig ) %>% 
  count(ns_sig) %>% 
  summarise(count = n/2)


str(seg_ID_0vs50_100s50summary)
### Plot the results 
segments <- ggplot(seg_ID_0vs50_100s50summary, aes(SegmentID , YLDMASSDR, group = P_Rate_as_factor))+
  geom_line(size=1, alpha=0.4, aes( color = P_Rate_as_factor ))+
  scale_color_manual(values=c('darkgrey','green', 'blue'), name  ="P Rates")+
  theme_bw()+
  ylim(0.0,5)+
  labs(x= "distance along the strip",
       y = "yield t/ha",
       title = "",
       subtitle = "",
       caption = "")+
   annotate("rect", xmin = 98, xmax = 108, ymin = 0, ymax = 5,
           alpha = .2)


##save the results of the segment work
segments #this is the graph

graph_path <- file.path("//FSSA2-ADL/clw-share1/Microlab/value_soil_testing_prj/Yield_data/Tim_McClelland/Clover")
ggsave(path= graph_path, filename = "t-test_segments.png", device = "png" ,
       width = 20, height = 10, units = "cm")

write.csv(seg_ID_0vs50_100s50summary, "//FSSA2-ADL/clw-share1/Microlab/value_soil_testing_prj/Yield_data/Tim_McClelland/Clover/t_test_segments.csv")
write.csv(table_segments, "//FSSA2-ADL/clw-share1/Microlab/value_soil_testing_prj/Yield_data/Tim_McClelland/Clover/table_segments.csv")



##########################################################################################################################################
##### Paired t test for whole strip ####

##average the yield values in each line segment - this ensure I have the same number of points
str(seg_ID)

strip_av <- group_by(seg_ID,SegmentID, Farmer, Paddock, zone, P_Rate ) %>% 
  summarise_all(mean)
 

group_by(strip_av, P_Rate) %>% 
  summarise(mean(YLDMASSDR))

strip_av_av$P_Rate_as_factor <- as.factor(strip_av$P_Rate)
strip_av_av_0vs50 <- filter(strip_av, P_Rate != 100 )
strip_av_av_100vs50 <- filter(strip_av, P_Rate != 0 )
 # http://www.sthda.com/english/wiki/paired-samples-t-test-in-r
 
 #test assumptions
# group_by(strip_av_av_0vs50, P_Rate) %>% 
#   summarise(mean(YLDMASSDR))
 
 # compute the difference
 d <- with(clover_seg_ID_av, 
           YLDMASSDR[P_Rate == 0] - YLDMASSDR[P_Rate == 50])
 # Shapiro-Wilk normality test for the differences
 shapiro.test(d) 
 
strip_avr_res0vs50 <- t.test(YLDMASSDR ~ P_Rate, data = strip_av_av_0vs50, paired = TRUE)
strip_av_res100vs50 <- t.test(YLDMASSDR ~ P_Rate, data = strip_av_av_100vs50, paired = TRUE)
 
 #Report values from the t.test
strip_avr_res0vs50
strip_avres0vs50_sig <-
   data.frame(P_value = as.double(strip_avr_res0vs50$p.value),
              Mean_diff = (strip_avr_res0vs50$estimate)) %>%
   mutate(
     rounded = round(Mean_diff, 2),
     significant = case_when(P_value < 0.05 ~ "significant",
                        TRUE ~ "not significant"))
strip_avres0vs50_sig

strip_avres100vs50_sig <-
   data.frame(P_value = as.double(strip_av_res100vs50$p.value),
              Mean_diff = as.double(strip_av_res100vs50$estimate)) %>%
   mutate(
     rounded = round(Mean_diff, 2),
     significant = case_when(P_value < 0.05 ~ "significant",
                        TRUE ~ "not significant")) 
strip_av_res100vs50 
strip_avres100vs50_sig
 
 p_vlaue_text_strip <- paste0("P value 0 vs 50 Mean difference = ", strip_avres0vs50_sig$rounded, " ", 
                              strip_avres0vs50_sig$significant, "\n",
                             "P value 100 vs 50  Mean difference = ", strip_avres100vs50_sig$rounded, " ", 
                             strip_avres100vs50_sig$significant, collapse = "\n")
 print(p_vlaue_text_strip)

 Pvalue_on_graph <- grobTree(textGrob(p_vlaue_text_strip, x=0.1,  y=0.90, hjust=0,
                                      gp=gpar(col="black", fontsize=6, fontface="italic")))
 
 
 str(strip_av)
 strip_av$P_Rate_as_factor <- as.factor(strip_av$P_Rate)
 # Compute t-test - this is done for 0 vs 100 for all values in the strip

 strip <- ggplot( strip_av, aes(P_Rate_as_factor, YLDMASSDR))+
   geom_boxplot(alpha=0.1)+
   geom_point(colour = "blue", alpha = 0.1)+
   theme_bw()+
   ylim(0.0,5)+
   theme(axis.text=element_text(size=8),
         axis.title=element_text(size=10,))+
   labs(x = "P rate",
        y= "Yield t/ha",
        title = "Whole strip")+
   annotation_custom(Pvalue_on_graph)
 
 
 ##save the results of the whole strip work
 strip
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
 
group_by(zone_av, P_Rate) %>% 
  summarise(mean(YLDMASSDR))
 
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
 
 zone_avres0vs50
 #Report values from the t.test
 zone_avres0vs50_sig <-
   data.frame(P_value = as.double(zone_avres0vs50$p.value),
              Mean_diff = (zone_avres0vs50$estimate)) %>%
   mutate(
     rounded = round(Mean_diff, 2),
     Significant = case_when(P_value < 0.05 ~ "significant",
                        TRUE ~ "not significant"))
 zone_avres0vs50_sig 
 zone_avres100vs50_sig <-
   data.frame(P_value = as.double(zone_avres100vs50$p.value),
              Mean_diff = as.double(zone_avres100vs50$estimate)) %>%
   mutate(
     rounded = round(Mean_diff, 2),
     Significant = case_when(P_value < 0.05 ~ "significant",
                             TRUE ~ "not significant")) 
 
 zone_avres100vs50
 
 p_vlaue_text_zone <- paste0("P value 0 vs 50 Mean difference = ", zone_avres0vs50_sig$rounded, " ", 
                             zone_avres0vs50_sig$Significant, "\n",
                        "P value 100 vs 50  Mean difference = ", zone_avres100vs50_sig$rounded, " ", 
                        zone_avres100vs50_sig$Significant, collapse = "\n")
 print(p_vlaue_text_zone)
 library(grid)
 Pvalue_on_graph <- grobTree(textGrob(p_vlaue_text_zone, x=0.1,  y=0.90, hjust=0,
                                      gp=gpar(col="black", fontsize=6, fontface="italic")))
 
 
 
 
 # Compute t-test - this is done for 0 vs 100 for all values in the strip
 
 zone <- ggplot( zone_av, aes(P_Rate_as_factor, YLDMASSDR))+
   geom_boxplot(alpha=0.1)+
   geom_point(colour = "blue", alpha = 0.1)+
   theme_bw()+
   ylim(0.0,5)+
   theme(axis.text=element_text(size=8),
         axis.title=element_text(size=10,))+
   labs(x = "P rate",
        y= "Yield t/ha",
        title = "Zone 1")+
   annotation_custom(Pvalue_on_graph)
 
 ##save the results of the zone strip work
 zone
 graph_path <- file.path("//FSSA2-ADL/clw-share1/Microlab/value_soil_testing_prj/Yield_data/Tim_McClelland/Clover")
 ggsave(path= graph_path, filename = "t-test_zone_strip.png", device = "png" ,
        width = 10, height = 10, units = "cm")
 
 write.csv(zone_av, "//FSSA2-ADL/clw-share1/Microlab/value_soil_testing_prj/Yield_data/Tim_McClelland/Clover/t_testzone_av.csv")
 
 
 
#####################################################################################################################################
### should get this from harms database
 
 
 soil_test_Results <- data.frame(Colwell = 15,
                                DGT = 5,
                                PBI = 199, row.names = "Low")

 
 
table1 <- tableGrob(head(soil_test_Results ))
table2 <- tableGrob(head(table_segments))
 ####################################################################################################################################
 ## Arrange the outputs onto one page
 
 
 collection <- grid.arrange(segments, zone, strip, table1,table2, nrow = 5, 
              layout_matrix = cbind(c(2,2,5,1,1), c(3,3,4,1,1)))
             
collection
graph_path <- file.path("//FSSA2-ADL/clw-share1/Microlab/value_soil_testing_prj/Yield_data/Tim_McClelland/Clover")
ggsave(path= graph_path, filename = "collection.png", device = "png", 
       width = 21, height = 15, units = "cm", collection)
 
