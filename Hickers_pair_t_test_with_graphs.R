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

## New approach using polygon to pull out raw yield data

##Bring in my data...

name_of_path <-
  file.path("W:", "value_soil_testing_prj", "Yield_data", "Craig_Muir", "Huckers",  "Hucker_Yld_Seg_ID_zone.csv")
seg_ID <-read_csv(name_of_path)

graph_path <- file.path("W:", "value_soil_testing_prj", "Yield_data", "Craig_Muir", "Huckers")

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
head(seg_ID)
unique(seg_ID$P_Rates)
seg_ID_110vs55 <- filter(seg_ID, P_Rates == 110 | P_Rates== 55 )
seg_ID_27vs55 <- filter(seg_ID, P_Rates == 27 | P_Rates== 55 )
seg_ID_0vs55 <- filter(seg_ID, P_Rates == 0 | P_Rates== 55 )



# Compute t-test - this is done for 0 vs 100 for each segment values in the strip
# Basically what you need to do is subset your data based on segmentID and perform the t-test for each group of data.
# 

#Now put it in the loop with my test data! - its not looping through everything ? try making a list

#I want a list of all values in segment ID to uss in the loop
list <- unique(seg_ID_110vs55$SegmentID)

############################################################################################################
#Run as a loop for 20_40 test 1
head(seg_ID_110vs55)
Output_110vs55= data.frame() #create empty df for output
for (i in list){
  segment_data = subset(seg_ID_110vs55, SegmentID == i)
  
  # Method 1: The data are saved in two different numeric vectors.
  data_x=subset(segment_data, P_Rates==110, select = YldMassDry, drop = TRUE)
  data_y=subset(segment_data, P_Rates==55, select = YldMassDry, drop = TRUE)
  res_method1 <-t.test(data_x,data_y, var.equal = FALSE)
  
  p_vlaue <- res_method1$p.value
  segment_name <- unique(segment_data$SegmentID)
  result <- data.frame(SegmentID = segment_name, P_value = p_vlaue)
  Output_110vs55 = rbind(Output_110vs55, result)
} 

#convert the P value into NS or Sig at 0.05
Output_110vs55 <- mutate(Output_110vs55, 
                 Significant = case_when(
                 P_value < 0.05 ~ "significant",
                 TRUE ~ "not significant"
               ))
#To make this meaningful I need to summaries the input data and join it to the t - test results
head(Output_110vs55)
seg_ID_20vs40_summary <- group_by(seg_ID_110vs55,
                                          SegmentID, zone, P_Rates ) %>% 
  summarise_all(mean) %>% 
  ungroup()
#join output to summary

#what comparison did I run? - name the df to reflect this
seg_ID_110vs55_summary <- left_join(seg_ID_20vs40_summary, Output_110vs55)
seg_ID_110vs55_summary <- mutate(seg_ID_110vs55_summary, comparison = "P100vsP55" )




head(seg_ID_110vs55_summary)


#### up to here



seg_ID_0vs55 


#####################################################################################################
#Run as a loop for 27_55 test 2

head(seg_ID_27vs55)
Output_27vs55= data.frame() #create empty df for output
for (i in list){
  segment_data = subset(seg_ID_27vs55, SegmentID == i)
  
  # Method 1: The data are saved in two different numeric vectors.
  data_x=subset(segment_data, P_Rates==27, select = YldMassDry, drop = TRUE)
  data_y=subset(segment_data, P_Rates==55, select = YldMassDry, drop = TRUE)
  res_method1 <-t.test(data_x,data_y, var.equal = FALSE)
  
  p_vlaue <- res_method1$p.value
  segment_name <- unique(segment_data$SegmentID)
  result <- data.frame(SegmentID = segment_name, P_value = p_vlaue)
  Output_27vs55 = rbind(Output_27vs55, result)
} 

#convert the P value into NS or Sig at 0.05
Output_27vs55 <- mutate(Output_27vs55, 
                         Significant = case_when(
                           P_value < 0.05 ~ "significant",
                           TRUE ~ "not significant"
                         ))
#To make this meaningful I need to summaries the input data and join it to the t - test results
head(Output_27vs55)
seg_ID_27vs55_summary <- group_by(seg_ID_27vs55,
                                  SegmentID, zone, P_Rates ) %>% 
  summarise_all(mean) %>% 
  ungroup()
#join output to summary

#what comparison did I run? - name the df to reflect this
seg_ID_27vs55_summary <- left_join(seg_ID_27vs55_summary, Output_27vs55)
seg_ID_27vs55_summary <- mutate(seg_ID_27vs55_summary, comparison = "P27vsP55" )



seg_ID_27vs55_summary 


#####################################################################################################
#Run as a loop for 0_55 test 3

head(seg_ID_0vs55)
Output_0vs55= data.frame() #create empty df for output
for (i in list){
  segment_data = subset(seg_ID_0vs55, SegmentID == i)
  
  # Method 1: The data are saved in two different numeric vectors.
  data_x=subset(segment_data, P_Rates==0, select = YldMassDry, drop = TRUE)
  data_y=subset(segment_data, P_Rates==55, select = YldMassDry, drop = TRUE)
  res_method1 <-t.test(data_x,data_y, var.equal = FALSE)
  
  p_vlaue <- res_method1$p.value
  segment_name <- unique(segment_data$SegmentID)
  result <- data.frame(SegmentID = segment_name, P_value = p_vlaue)
  Output_0vs55 = rbind(Output_0vs55, result)
} 

#convert the P value into NS or Sig at 0.05
Output_0vs55 <- mutate(Output_0vs55, 
                        Significant = case_when(
                          P_value < 0.05 ~ "significant",
                          TRUE ~ "not significant"
                        ))
#To make this meaningful I need to summaries the input data and join it to the t - test results
head(Output_0vs55)
seg_ID_0vs55_summary <- group_by(seg_ID_0vs55,
                                  SegmentID, zone, P_Rates ) %>% 
  summarise_all(mean) %>% 
  ungroup()
#join output to summary

#what comparison did I run? - name the df to reflect this
seg_ID_0vs55_summary <- left_join(seg_ID_0vs55_summary, Output_0vs55)
seg_ID_0vs55_summary <- mutate(seg_ID_0vs55_summary, comparison = "P0vsP55" )



###############################################################################################################

##Join the two strip data results togther join info from test 1 to test 2 and test 3
head(seg_ID_110vs55_summary)
head(seg_ID_27vs55_summary)
head(seg_ID_0vs55_summary)

seg_ID_0_127_110vs55_summary <- rbind(seg_ID_110vs55_summary, seg_ID_27vs55_summary, seg_ID_0vs55_summary)

###remove some of the data from my workspace
rm(list = c("Output_0vs55", 
            "Output_110vs55", 
            "Output_27vs55",
            
            "seg_ID_0vs55_summary",
            "seg_ID_110vs55_summary",
            "seg_ID_27vs55_summary",
            
            "res_method1",
            "result", 
            
            "seg_ID_0vs55", 
            "seg_ID_110vs55",
            "seg_ID_27vs55"
            )) 


##############################################################################################################
#########    plot results  of t.test ########################################################################
seg_ID_0_127_110vs55_summary$P_Rate_as_factor <- as.factor(seg_ID_0_127_110vs55_summary$P_Rates)
str(seg_ID_0_127_110vs55_summary)
unique(seg_ID_0_127_110vs55_summary$zone)

#what is the area of the zones this can be added to the graph?
filter(seg_ID_0_127_110vs55_summary, zone == "east") %>% 
  summarise(min_zone = min(SegmentID),
            max_zone = max(SegmentID))

filter(seg_ID_0_127_110vs55_summary, zone == "west") %>% 
  summarise(min_zone = min(SegmentID),
            max_zone = max(SegmentID))
head(seg_ID_0vs50_100s50summary)
#create a table that count how many signifcant vs non significant values we have
table_segments <- group_by(seg_ID_0_127_110vs55_summary,comparison,   Significant ) %>% 
  count(Significant) %>% 
  summarise(count = (n/2)) 
table_segments$count <- round(table_segments$count, 0)



str(seg_ID_0_127_110vs55_summary)
### Plot the results 
segments <- ggplot(seg_ID_0_127_110vs55_summary, aes(SegmentID , YldMassDry, group = P_Rate_as_factor))+
  geom_line(size=1, alpha=0.4, aes( color = P_Rate_as_factor ))+
  scale_color_manual(values=c('darkgrey','green', 'blue', 'red'), name  ="P Rates")+
  theme_bw()+
  ylim(0.0,4)+
  labs(x= "Distance along the strip",
       y = "Yield t/ha",
       title = "",
       subtitle = "",
       caption = "")+
   annotate("rect", xmin = 30, xmax = 39, ymin = 0, ymax = 4,
           alpha = .2) +
  annotate("text", x = 35, y= 1,label = "West")+
  
  annotate("rect", xmin = 2, xmax = 11, ymin = 0, ymax = 4,
            alpha = .2)+
  annotate("text", x = 6, y= 1,label = "East")#+
  #annotate("text", x = 32, y= 1,label = "Missing yield data")


##save the results of the segment work
segments #this is the graph


ggsave(path= graph_path, filename = "t-test_segments.png", device = "png" ,
       width = 20, height = 10, units = "cm")

write.csv(seg_ID_0vs50_100s50summary, paste0(graph_path,"/t_test_segments.csv"))
write.csv(table_segments, paste0(graph_path,"/table_segments.csv"))



##########################################################################################################################################
##### Paired t test for whole strip ####

##average the yield values in each line segment - this ensure I have the same number of points
str(seg_ID)

strip_av <- group_by(seg_ID,SegmentID,  P_Rates ) %>% 
  summarise_all(mean)
 

group_by(strip_av, P_Rates) %>% 
  summarise(mean(YldMassDry))

strip_av_110vs55 <- filter(strip_av, P_Rates == 110 | P_Rates== 55 )
strip_av_27vs55 <- filter(strip_av, P_Rates == 27 | P_Rates== 55 )
strip_av_0vs55 <- filter(strip_av, P_Rates == 0 | P_Rates== 55 )

 # http://www.sthda.com/english/wiki/paired-samples-t-test-in-r
 
#  #test assumptions
# # group_by(strip_av_av_0vs50, P_Rate) %>% 
# #   summarise(mean(YLDMASSDR))
#  
#  # compute the difference
#  d <- with(clover_seg_ID_av, 
#            YLDMASSDR[P_Rate == 0] - YLDMASSDR[P_Rate == 50])
#  # Shapiro-Wilk normality test for the differences
#  shapiro.test(d) 
 

strip_av_110vs55_res <- t.test(YldMassDry ~ P_Rates, data = strip_av_110vs55, paired = TRUE)
strip_av_27vs55_res <- t.test(YldMassDry ~ P_Rates, data = strip_av_27vs55, paired = TRUE)
strip_av_0vs55_res <- t.test(YldMassDry ~ P_Rates, data = strip_av_0vs55, paired = TRUE)

 #Report values from the t.test

### test 1
strip_av_110vs55_res
strip_av_110vs55_res_sig <-
   data.frame(P_value = as.double(strip_av_110vs55_res$p.value),
              Mean_diff = (strip_av_110vs55_res$estimate)) %>%
   mutate(
     rounded = abs(round(Mean_diff, 2)),
     significant = case_when(P_value < 0.05 ~ "significant",
                        TRUE ~ "not significant"))
strip_av_110vs55_res_sig
#### test 2
strip_av_27vs55_res
strip_av_27vs55_res_sig <-
   data.frame(P_value = as.double(strip_av_27vs55_res$p.value),
              Mean_diff = as.double(strip_av_27vs55_res$estimate)) %>%
   mutate(
     rounded = abs(round(Mean_diff, 2)),
     significant = case_when(P_value < 0.05 ~ "significant",
                        TRUE ~ "not significant")) 
strip_av_27vs55_res_sig
### test 3
strip_av_0vs55_res
strip_av_0vs55_res_sig <-
  data.frame(P_value = as.double(strip_av_0vs55_res$p.value),
             Mean_diff = as.double(strip_av_0vs55_res$estimate)) %>%
  mutate(
    rounded = abs(round(Mean_diff, 2)),
    significant = case_when(P_value < 0.05 ~ "significant",
                            TRUE ~ "not significant")) 

strip_av_0vs55_res_sig


p_vlaue_text_strip <- paste0("Yield at P 55 is P 0 plus ", strip_av_0vs55_res_sig$rounded, " and is ", 
                             strip_av_0vs55_res_sig$significant, "\n",
                                 "Yield at P 55 is P 27 plus ", strip_av_27vs55_res_sig$rounded, " and is ", 
                             strip_av_27vs55_res_sig$significant,  "\n",
                              "Yield at P 110 is P 55 minus ", strip_av_110vs55_res_sig$rounded, " and is ", 
                             strip_av_110vs55_res_sig$significant, collapse = "\n")
 print(p_vlaue_text_strip)

 Pvalue_on_graph <- grobTree(textGrob(p_vlaue_text_strip, x=0.1,  y=0.90, hjust=0,
                                      gp=gpar(col="black", fontsize=6, fontface="italic")))
 
 
 str(strip_av)
 strip_av$P_Rate_as_factor <- as.factor(strip_av$P_Rates)
 # Compute t-test - this is done for 0 vs 100 for all values in the strip

 strip <- ggplot( strip_av, aes(P_Rate_as_factor, YldMassDry))+
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
 ggsave(path= graph_path, filename = "t-test_whole_strip.png", device = "png" ,
        width = 20, height = 10, units = "cm")
 write.csv(strip_av, paste0(graph_path,"/t_test_strip_av.csv"))
 
 
 
 
 

 ##### Paired t test for zone strip WEST ####
 
 ##average the yield values in each line segment - this ensure I have the same number of points
 str(seg_ID)
 zone_west <- filter(seg_ID, zone == "west" )
 zone_west_av <- group_by(zone_west,SegmentID, P_Rates ) %>% 
   summarise_all(mean)

group_by(zone_west_av, P_Rates) %>% 
  summarise(mean(YldMassDry))
 
zone_west_av$P_Rate_as_factor <- as.factor(zone_west_av$P_Rates)
 
zone_west_av_110vs55 <- filter(zone_west_av, P_Rates == 110 | P_Rates== 55 )
zone_west_av_27vs55 <- filter(zone_west_av, P_Rates == 27 | P_Rates== 55 )
zone_west_av_0vs55 <- filter(zone_west_av, P_Rates == 0 | P_Rates== 55 )

 # http://www.sthda.com/english/wiki/paired-samples-t-test-in-r
 
 #test assumptions
 
 
 # # compute the difference
 # d <- with(zone_av, 
 #           YLDMASSDR[P_Rate == 0] - YLDMASSDR[P_Rate == 50])
 # # Shapiro-Wilk normality test for the differences
 # shapiro.test(d) # => p-value = 0.5161
 # 
 # I think this means that it is  normally distrubuted
 
zone_west_av_110vs55_res <- t.test(YldMassDry ~ P_Rates, data = zone_west_av_110vs55, paired = TRUE)
zone_west_av_27vs55_res <- t.test(YldMassDry ~ P_Rates, data = zone_west_av_27vs55, paired = TRUE)
zone_west_av_0vs55_res <- t.test(YldMassDry ~ P_Rates, data = zone_west_av_0vs55, paired = TRUE)

#test 1 results
zone_west_av_110vs55_res
 #Report values from the t.test
zone_west_av_110vs55_res_sig <-
   data.frame(P_value = as.double(zone_west_av_110vs55_res$p.value),
              Mean_diff = (zone_west_av_110vs55_res$estimate)) %>%
   mutate(
     rounded = abs(round(Mean_diff, 2)),
     Significant = case_when(P_value < 0.05 ~ "significant",
                        TRUE ~ "not significant"))
zone_west_av_110vs55_res_sig 
 
#test 2 results
zone_west_av_27vs55_res
zone_west_av_27vs55_res_sig <-
  data.frame(P_value = as.double(zone_west_av_27vs55_res$p.value),
             Mean_diff = (zone_west_av_27vs55_res$estimate)) %>%
  mutate(
    rounded = abs(round(Mean_diff, 2)),
    Significant = case_when(P_value < 0.05 ~ "significant",
                            TRUE ~ "not significant"))
zone_west_av_27vs55_res_sig 

#test 3 results
zone_west_av_0vs55_res
zone_west_av_0vs55_res_sig <-
  data.frame(P_value = as.double(zone_west_av_0vs55_res$p.value),
             Mean_diff = (zone_west_av_0vs55_res$estimate)) %>%
  mutate(
    rounded = abs(round(Mean_diff, 2)),
    Significant = case_when(P_value < 0.05 ~ "significant",
                            TRUE ~ "not significant"))

zone_west_av_0vs55_res_sig 


p_vlaue_text_west <- paste0("Yield at P 55 is P 0 plus ", zone_west_av_0vs55_res_sig$rounded, " and is ", 
                            zone_west_av_0vs55_res_sig$Significant, "\n",
                             "Yield at P 55 is P 27 plus ", zone_west_av_27vs55_res_sig$rounded, " and is ", 
                            zone_west_av_27vs55_res_sig$Significant,  "\n",
                             "Yield at P 110 is P 55 minus ", zone_west_av_110vs55_res_sig$rounded, " and is ", 
                            zone_west_av_110vs55_res_sig$Significant, collapse = "\n")





 print(p_vlaue_text_west)
 
 
 
 library(grid)
 Pvalue_on_graph <- grobTree(textGrob(p_vlaue_text_west, x=0.1,  y=0.10, hjust=0,
                                      gp=gpar(col="black", fontsize=6, fontface="italic")))
 
 
 
 
 # Compute t-test - this is done for 0 vs 100 for all values in the strip
 zone_west_av
 zone_west <- ggplot( zone_west_av, aes(P_Rate_as_factor, YldMassDry))+
   geom_boxplot(alpha=0.1)+
   geom_point(colour = "blue", alpha = 0.1)+
   theme_bw()+
   ylim(0.0,4)+
   theme(axis.text=element_text(size=8),
         axis.title=element_text(size=10,))+
   labs(x = "P rate",
        y= "Yield t/ha",
        title = "West")+
   annotation_custom(Pvalue_on_graph)
 
 ##save the results of the zone strip work
 zone_west
 ggsave(path= graph_path, filename = "t-test_zone_west_strip.png", device = "png" ,
        width = 20, height = 10, units = "cm")
 write.csv(zone_west_av, paste0(graph_path,"/t_testzone_west_av.csv"))
 
 
 ###########################################################################################################################################
 ##### Paired t test for zone strip east ####
 
 ##average the yield values in each line segment - this ensure I have the same number of points
 str(seg_ID)
 zone_east <- filter(seg_ID, zone == "east" )
 zone_east_av <- group_by(zone_east,SegmentID, P_Rates ) %>% 
   summarise_all(mean)
 
 group_by(zone_east_av, P_Rates) %>% 
   summarise(mean(YldMassDry))
 
 zone_east_av$P_Rate_as_factor <- as.factor(zone_east_av$P_Rates)
 
 zone_east_av_110vs55 <- filter(zone_east_av, P_Rates == 110 | P_Rates== 55 )
 zone_east_av_27vs55 <- filter(zone_east_av, P_Rates == 27 | P_Rates== 55 )
 zone_east_av_0vs55 <- filter(zone_east_av, P_Rates == 0 | P_Rates== 55 )
 
 # http://www.sthda.com/english/wiki/paired-samples-t-test-in-r
 
 #test assumptions
 
 
 # # compute the difference
 # d <- with(zone_av, 
 #           YLDMASSDR[P_Rate == 0] - YLDMASSDR[P_Rate == 50])
 # # Shapiro-Wilk normality test for the differences
 # shapiro.test(d) # => p-value = 0.5161
 # 
 # I think this means that it is  normally distrubuted
 
 zone_east_av_110vs55_res <- t.test(YldMassDry ~ P_Rates, data = zone_east_av_110vs55, paired = TRUE)
 zone_east_av_27vs55_res <- t.test(YldMassDry ~ P_Rates, data = zone_east_av_27vs55, paired = TRUE)
 zone_east_av_0vs55_res <- t.test(YldMassDry ~ P_Rates, data = zone_east_av_0vs55, paired = TRUE)
 
 #test 1 results
 zone_east_av_110vs55_res
 #Report values from the t.test
 zone_east_av_110vs55_res_sig  <-
   data.frame(P_value = as.double(zone_east_av_110vs55_res$p.value),
              Mean_diff = (zone_east_av_110vs55_res$estimate)) %>%
   mutate(
     rounded = abs(round(Mean_diff, 2)),
     Significant = case_when(P_value < 0.05 ~ "significant",
                             TRUE ~ "not significant"))
 zone_east_av_110vs55_res_sig 
 
 #test 2 results
 zone_east_av_27vs55_res
 zone_east_av_27vs55_res_sig <-
   data.frame(P_value = as.double(zone_east_av_27vs55_res$p.value),
              Mean_diff = (zone_east_av_27vs55_res$estimate)) %>%
   mutate(
     rounded = abs(round(Mean_diff, 2)),
     Significant = case_when(P_value < 0.05 ~ "significant",
                             TRUE ~ "not significant"))
 zone_east_av_27vs55_res_sig 
 
 #test 3 results
 zone_east_av_0vs55_res
 zone_east_av_0vs55_res_sig <-
   data.frame(P_value = as.double(zone_east_av_0vs55_res$p.value),
              Mean_diff = (zone_east_av_0vs55_res$estimate)) %>%
   mutate(
     rounded = abs(round(Mean_diff, 2)),
     Significant = case_when(P_value < 0.05 ~ "significant",
                             TRUE ~ "not significant"))
 
 zone_east_av_0vs55_res_sig 
 
 
 p_vlaue_text_east <- paste0("Yield at P 55 is P 0 plus ", zone_east_av_0vs55_res_sig$rounded, " and is ", 
                             zone_east_av_0vs55_res_sig$Significant, "\n",
                             "Yield at P 55 is P 27 plus ", zone_east_av_27vs55_res_sig$rounded, " and is ", 
                             zone_east_av_27vs55_res_sig$Significant,  "\n",
                             "Yield at P 110 is P 55 minus ", zone_east_av_110vs55_res_sig$rounded, " and is ", 
                             zone_east_av_110vs55_res_sig$Significant, collapse = "\n")
 
 
 
 
 
 print(p_vlaue_text_east)
 
 
 
 library(grid)
 Pvalue_on_graph <- grobTree(textGrob(p_vlaue_text_east, x=0.1,  y=0.10, hjust=0,
                                      gp=gpar(col="black", fontsize=6, fontface="italic")))
 
 
 
 
 # Compute t-test - this is done for 0 vs 100 for all values in the strip
 zone_east_av
 zone_east <- ggplot( zone_east_av, aes(P_Rate_as_factor, YldMassDry))+
   geom_boxplot(alpha=0.1)+
   geom_point(colour = "blue", alpha = 0.1)+
   theme_bw()+
   ylim(0.0,4)+
   theme(axis.text=element_text(size=8),
         axis.title=element_text(size=10,))+
   labs(x = "P rate",
        y= "Yield t/ha",
        title = "East")+
   annotation_custom(Pvalue_on_graph)
 
 ##save the results of the zone strip work
 zone_east
 ggsave(path= graph_path, filename = "t-test_zone_east_strip.png", device = "png" ,
        width = 20, height = 10, units = "cm")
 write.csv(zone_east_av, paste0(graph_path,"/t_testzone_east_av.csv"))
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
#####################################################################################################################################
### should get this from harms database
 #bring in data from the most current database
 "\\FSSA2-ADL\clw-share1\Microlab\value_soil_testing_prj\data_base\N&P 2019 data for analysis Vic and SA latest.xlsx"
 name_of_path_database <-
   file.path("W:", "value_soil_testing_prj", "data_base", "NP 2019 data for analysis Vic and SA latest.xlsx")
 
 harm_database <- read_excel(name_of_path_database, sheet = "2019 full data", range = "A1:N480")
 str(harm_database)
 #fix up some names
 harm_database<-
   dplyr::select(harm_database,
                 "Paddock_code" =  `Paddock code`,     
                 Contact, Farmer,
                 "Paddock_tested" = `Paddock tested`,
                 Zone ,
                 Colwell,
                 DGT,
                 PBI ,
                 `Total N`,
                 `Colwell rec rate`,
                 `DGT rec rate`)
 #remove the row that is missing..
 harm_database <-filter(harm_database, Paddock_code != "NA")
 
 ## Pull out the infor for the paddock I am testing..Craig Muir   1	Postlethwaite	Arthurs
 str(harm_database)
 
 Craig_Muir_Huckers <- filter(harm_database,
                                   Paddock_tested == "Huckers") %>% 
   dplyr::select(5: 11)


 Craig_Muir_Huckers
 
 
  TSpecial <- ttheme_minimal(base_size = 8)
table1 <- tableGrob(Craig_Muir_Huckers , rows = NULL, theme=TSpecial )
#table2 <- tableGrob(table_segments, rows = NULL, theme=TSpecial)

#get the name of the paddock...

paddock <- "Huckers"
test <- textGrob(paddock)
 ####################################################################################################################################
 ## Arrange the outputs onto one page
segments
zone_east
zone_west
paddock
Craig_Muir_Huckers

 collection <- grid.arrange(zone_east, zone_west, table1, segments, test,  nrow = 5, 
              layout_matrix = cbind(c(1,1,5,4,4), c(2,2,3,4,4)))
             
collection

ggsave(path= graph_path, filename = paste0(paddock, "_collection.png"), device = "png", 
       width = 21, height = 15, units = "cm", collection)
 
