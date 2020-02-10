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
  file.path("W:", "value_soil_testing_prj", "Yield_data", "Craig_Muir", "Dunns", "new_format",  "Dunns_Yld_Seg_ID_zone.csv")
seg_ID <-read_csv(name_of_path)

graph_path <- file.path("W:", "value_soil_testing_prj", "Yield_data", "Craig_Muir", "Dunns", "new_format")

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
seg_ID_0vs50 <-   filter(seg_ID, P_Rates == 0 | P_Rates== 50 )
seg_ID_25vs50 <-  filter(seg_ID, P_Rates == 25 | P_Rates== 50 )
seg_ID_100vs50 <- filter(seg_ID, P_Rates == 100 | P_Rates== 50 )



# Compute t-test - this is done for 0 vs 100 for each segment values in the strip
# Basically what you need to do is subset your data based on segmentID and perform the t-test for each group of data.
# 

#Now put it in the loop with my test data! - its not looping through everything ? try making a list

#I want a list of all values in segment ID to uss in the loop
list <- unique(seg_ID_0vs50$SegmentID)

############################################################################################################
#Run as a loop for 20_40 test 1
head(seg_ID_0vs50)
Output_0vs50= data.frame() #create empty df for output
for (i in list){
  segment_data = subset(seg_ID_0vs50, SegmentID == i)
  
  # Method 1: The data are saved in two different numeric vectors.
  data_x=subset(segment_data, P_Rates==0, select = YldMassDry, drop = TRUE)
  data_y=subset(segment_data, P_Rates==50, select = YldMassDry, drop = TRUE)
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
head(Output_0vs50)
seg_ID_0vs50_summary <- group_by(seg_ID_0vs50,
                                          SegmentID, zone, P_Rates ) %>% 
  summarise_all(mean) %>% 
  ungroup()
#join output to summary

#what comparison did I run? - name the df to reflect this
seg_ID_0vs50_summary <- left_join(seg_ID_0vs50_summary, Output_0vs50)
seg_ID_0vs50_summary <- mutate(seg_ID_0vs50_summary, comparison = "P0vsP50" )
head(seg_ID_0vs50_summary)






 


#####################################################################################################
#Run as a loop for 25vs50 test 2

head(seg_ID_25vs50)
Output_25vs50= data.frame() #create empty df for output
for (i in list){
  segment_data = subset(seg_ID_25vs50, SegmentID == i)
  
  # Method 1: The data are saved in two different numeric vectors.
  data_x=subset(segment_data, P_Rates==25, select = YldMassDry, drop = TRUE)
  data_y=subset(segment_data, P_Rates==50, select = YldMassDry, drop = TRUE)
  res_method1 <-t.test(data_x,data_y, var.equal = FALSE)
  
  p_vlaue <- res_method1$p.value
  segment_name <- unique(segment_data$SegmentID)
  result <- data.frame(SegmentID = segment_name, P_value = p_vlaue)
  Output_25vs50 = rbind(Output_25vs50, result)
} 

#convert the P value into NS or Sig at 0.05
Output_25vs50 <- mutate(Output_25vs50, 
                       Significant = case_when(
                         P_value < 0.05 ~ "significant",
                         TRUE ~ "not significant"
                       ))
#To make this meaningful I need to summaries the input data and join it to the t - test results
head(Output_25vs50)
seg_ID_25vs50_summary <- group_by(seg_ID_25vs50,
                                 SegmentID, zone, P_Rates ) %>% 
  summarise_all(mean) %>% 
  ungroup()
#join output to summary

#what comparison did I run? - name the df to reflect this
seg_ID_25vs50_summary <- left_join(seg_ID_25vs50_summary, Output_0vs50)
seg_ID_25vs50_summary <- mutate(seg_ID_25vs50_summary, comparison = "P25vsP50" )
head(seg_ID_25vs50_summary)



#####################################################################################################
#Run as a loop for 100vs50 test 3

head(seg_ID_100vs50)
Output_100vs50= data.frame() #create empty df for output
for (i in list){
  segment_data = subset(seg_ID_100vs50, SegmentID == i)
  
  # Method 1: The data are saved in two different numeric vectors.
  data_x=subset(segment_data, P_Rates==100, select = YldMassDry, drop = TRUE)
  data_y=subset(segment_data, P_Rates==50, select = YldMassDry, drop = TRUE)
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
head(Output_100vs50)
seg_ID_100vs50_summary <- group_by(seg_ID_100vs50,
                                  SegmentID, zone, P_Rates ) %>% 
  summarise_all(mean) %>% 
  ungroup()
#join output to summary

#what comparison did I run? - name the df to reflect this
seg_ID_100vs50_summary <- left_join(seg_ID_100vs50_summary, Output_0vs50)
seg_ID_100vs50_summary <- mutate(seg_ID_100vs50_summary, comparison = "P100vsP50" )
head(seg_ID_100vs50_summary)



###############################################################################################################

##Join the two strip data results togther join info from test 1 to test 2 and test 3
head(seg_ID_100vs50_summary)
head(seg_ID_0vs50_summary)
head(seg_ID_25vs50_summary)

seg_ID_0_25_100vs50_summary <- rbind(seg_ID_100vs50_summary, seg_ID_0vs50_summary, seg_ID_25vs50_summary)

###remove some of the data from my workspace
rm(list = c("Output_0vs50", 
            "Output_100vs50", 
            "Output_25vs50",
            
            "seg_ID_0vs50_summary",
            "seg_ID_100vs50_summary",
            "seg_ID_25vs50_summary",
            
            "res_method1",
            "result", 
            
            "seg_ID_0vs50", 
            "seg_ID_100vs50",
            "seg_ID_25vs50"
            )) 


##############################################################################################################
#########    plot results  of t.test ########################################################################
seg_ID_0_25_100vs50_summary$P_Rate_as_factor <- as.factor(seg_ID_0_25_100vs50_summary$P_Rates)
str(seg_ID_0_25_100vs50_summary)
unique(seg_ID_0_25_100vs50_summary$zone)

#what is the area of the zones this can be added to the graph?
filter(seg_ID_0_25_100vs50_summary, zone == "North") %>% 
  summarise(min_zone = min(SegmentID),
            max_zone = max(SegmentID))

filter(seg_ID_0_25_100vs50_summary, zone == "South") %>% 
  summarise(min_zone = min(SegmentID),
            max_zone = max(SegmentID))
head(seg_ID_0_25_100vs50_summary)
#create a table that count how many signifcant vs non significant values we have
table_segments <- group_by(seg_ID_0_25_100vs50_summary,comparison,   Significant ) %>% 
  count(Significant) %>% 
  summarise(count = (n/2)) 
table_segments$count <- round(table_segments$count, 0)



str(seg_ID_0_127_110vs55_summary)
### Plot the results 
segments <- ggplot(seg_ID_0_25_100vs50_summary, aes(SegmentID , YldMassDry, group = P_Rate_as_factor))+
  geom_line(size=1, alpha=0.4, aes( color = P_Rate_as_factor ))+
  scale_color_manual(values=c('darkgrey','green', 'blue', 'red'), name  ="P Rates")+
  theme_bw()+
  ylim(0.0,4)+
  labs(x= "Distance along the strip",
       y = "Yield t/ha",
       title = "",
       subtitle = "",
       caption = "")+
   annotate("rect", xmin = 5, xmax = 24, ymin = 0, ymax = 4,
           alpha = .2) +
  annotate("text", x = 16, y= 1,label = "North")+
  
  annotate("rect", xmin = 49, xmax = 59, ymin = 0, ymax = 4,
            alpha = .2)+
  annotate("text", x = 55, y= 1,label = "South")+
  annotate("text", x = 16, y= 0.5,label = "North is large beacuse the zone is at an angle", size = 1.5)


##save the results of the segment work
segments #this is the graph


ggsave(path= graph_path, filename = "t-test_segments.png", device = "png" ,
       width = 20, height = 10, units = "cm")

write.csv(seg_ID_0_25_100vs50_summary, paste0(graph_path,"/t_test_segments.csv"))
write.csv(table_segments, paste0(graph_path,"/table_segments.csv"))



##########################################################################################################################################
##### Paired t test for whole strip ####

##average the yield values in each line segment - this ensure I have the same number of points
str(seg_ID)

strip_av <- group_by(seg_ID,SegmentID,  P_Rates ) %>% 
  summarise_all(mean)
 

group_by(strip_av, P_Rates) %>% 
  summarise(mean(YldMassDry))

strip_av_0vs50 <- filter(strip_av, P_Rates == 0 | P_Rates== 50 )
strip_av_25vs50 <- filter(strip_av, P_Rates == 25 | P_Rates== 50 )
strip_av_100vs50 <- filter(strip_av, P_Rates == 100 | P_Rates== 50 )

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
 

strip_av_0vs50_res <- t.test(YldMassDry ~ P_Rates, data = strip_av_0vs50, paired = TRUE)
strip_av_25vs50_res <- t.test(YldMassDry ~ P_Rates, data = strip_av_25vs50, paired = TRUE)
strip_av_100vs50_res <- t.test(YldMassDry ~ P_Rates, data = strip_av_100vs50, paired = TRUE)

 #Report values from the t.test

### test 1
strip_av_0vs50_res
strip_av_0vs50_res_sig <-
   data.frame(P_value = as.double(strip_av_0vs50_res$p.value),
              Mean_diff = (strip_av_0vs50_res$estimate)) %>%
   mutate(
     rounded = abs(round(Mean_diff, 2)),
     significant = case_when(P_value < 0.05 ~ "significant",
                        TRUE ~ "not significant"))
strip_av_0vs50_res_sig
#### test 2
strip_av_25vs50_res
strip_av_25vs50_res_sig <-
   data.frame(P_value = as.double(strip_av_25vs50_res$p.value),
              Mean_diff = as.double(strip_av_25vs50_res$estimate)) %>%
   mutate(
     rounded = abs(round(Mean_diff, 2)),
     significant = case_when(P_value < 0.05 ~ "significant",
                        TRUE ~ "not significant")) 
strip_av_25vs50_res_sig
### test 3
strip_av_100vs50_res
strip_av_100vs50_res_sig <-
  data.frame(P_value = as.double(strip_av_100vs50_res$p.value),
             Mean_diff = as.double(strip_av_100vs50_res$estimate)) %>%
  mutate(
    rounded = abs(round(Mean_diff, 2)),
    significant = case_when(P_value < 0.05 ~ "significant",
                            TRUE ~ "not significant")) 

strip_av_100vs50_res_sig


p_vlaue_text_strip <- paste0("Yield at P 50 is P 0 minus ", strip_av_0vs50_res_sig$rounded, " and is ", 
                             strip_av_0vs50_res_sig$significant, "\n",
                                 "Yield at P 50 is P 25 minus ", strip_av_25vs50_res_sig$rounded, " and is ", 
                             strip_av_25vs50_res_sig$significant,  "\n",
                              "Yield at P 100 is P 50 plus ", strip_av_100vs50_res_sig$rounded, " and is ", 
                             strip_av_100vs50_res_sig$significant, collapse = "\n")
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
 
 
 
 
 

 ##### Paired t test for zone strip North ####
 
 ##average the yield values in each line segment - this ensure I have the same number of points
 unique(seg_ID$zone)
 zone_North <- filter(seg_ID, zone == "North" )
 zone_north_av <- group_by(zone_North,SegmentID, P_Rates ) %>% 
   summarise_all(mean)

group_by(zone_north_av, P_Rates) %>% 
  summarise(mean(YldMassDry))
 
zone_north_av$P_Rate_as_factor <- as.factor(zone_north_av$P_Rates)
 
zone_north_av_0vs50 <- filter(zone_north_av, P_Rates == 0 | P_Rates== 50 )
zone_north_av_25vs50 <- filter(zone_north_av, P_Rates == 25 | P_Rates== 50 )
zone_north_av_100vs50 <- filter(zone_north_av, P_Rates == 100 | P_Rates== 50 )

dim(zone_north_av_0vs50)
dim(zone_north_av_25vs50)
dim(zone_north_av_100vs50)

#the data is not replictaed and I need to remove some segment ID that dont have matching segements
zone_north_av_0vs50
zone_north_av_0vs50 <- zone_north_av_0vs50 %>% 
  filter(between(SegmentID, 10,17))
zone_north_av_25vs50 <- zone_north_av_25vs50 %>% 
  filter(between(SegmentID, 7,17))
zone_north_av_100vs50 <- zone_north_av_100vs50 %>% 
  filter(between(SegmentID, 13,17))
                              
zone_north_av_0_25_100vs50 <- rbind(zone_north_av_0vs50, zone_north_av_25vs50, zone_north_av_100vs50)
group_by(zone_north_av_0_25_100vs50, P_Rates) %>% 
  summarise(mean(YldMassDry))
 # http://www.sthda.com/english/wiki/paired-samples-t-test-in-r
 
 #test assumptions
 
 
 # # compute the difference
 # d <- with(zone_av, 
 #           YLDMASSDR[P_Rate == 0] - YLDMASSDR[P_Rate == 50])
 # # Shapiro-Wilk normality test for the differences
 # shapiro.test(d) # => p-value = 0.5161
 # 
 # I think this means that it is  normally distrubuted
 
zone_north_av_0vs50_res <- t.test(YldMassDry ~ P_Rates, data = zone_north_av_0vs50, paired = TRUE)
zone_north_av_25vs50_res <- t.test(YldMassDry ~ P_Rates, data = zone_north_av_25vs50, paired = TRUE)
zone_north_av_100vs50_res <- t.test(YldMassDry ~ P_Rates, data = zone_north_av_100vs50, paired = TRUE)

#test 1 results
zone_north_av_0vs50_res
 #Report values from the t.test
zone_north_av_0vs50_res_sig <-
   data.frame(P_value = as.double(zone_north_av_0vs50_res$p.value),
              Mean_diff = (zone_north_av_0vs50_res$estimate)) %>%
   mutate(
     rounded = abs(round(Mean_diff, 2)),
     Significant = case_when(P_value < 0.05 ~ "significant",
                        TRUE ~ "not significant"))
zone_north_av_0vs50_res_sig 
 
#test 2 results
zone_north_av_25vs50_res
zone_north_av_25vs50_res_sig <-
  data.frame(P_value = as.double(zone_north_av_25vs50_res$p.value),
             Mean_diff = (zone_north_av_25vs50_res$estimate)) %>%
  mutate(
    rounded = abs(round(Mean_diff, 2)),
    Significant = case_when(P_value < 0.05 ~ "significant",
                            TRUE ~ "not significant"))
zone_north_av_25vs50_res_sig 

#test 3 results
zone_north_av_100vs50_res
zone_north_av_100vs50_res_res_sig <-
  data.frame(P_value = as.double(zone_north_av_100vs50_res$p.value),
             Mean_diff = (zone_north_av_100vs50_res$estimate)) %>%
  mutate(
    rounded = abs(round(Mean_diff, 2)),
    Significant = case_when(P_value < 0.05 ~ "significant",
                            TRUE ~ "not significant"))

zone_north_av_100vs50_res_res_sig 


p_vlaue_text_north <- paste0("Yield at P 50 is P 0 plus ", zone_north_av_0vs50_res_sig$rounded, " and is ", 
                             zone_north_av_0vs50_res_sig$Significant, "\n",
                             "Yield at P 50 is P 25 plus ", zone_north_av_25vs50_res_sig$rounded, " and is ", 
                            zone_north_av_25vs50_res_sig$Significant,  "\n",
                             "Yield at P 100 is P 50 minus ", zone_north_av_100vs50_res_res_sig$rounded, " and is ", 
                            zone_north_av_100vs50_res_res_sig$Significant, collapse = "\n")





 print(p_vlaue_text_north)
 library(grid)
 Pvalue_on_graph <- grobTree(textGrob(p_vlaue_text_north, x=0.1,  y=0.10, hjust=0,
                                      gp=gpar(col="black", fontsize=6, fontface="italic")))
 
 
 
 
 # Compute t-test - this is done for 0 vs 100 for all values in the strip
 zone_north_av_0_25_100vs50
 zone_north <- ggplot( zone_north_av_0_25_100vs50, aes(P_Rate_as_factor, YldMassDry))+
   geom_boxplot(alpha=0.1)+
   geom_point(colour = "blue", alpha = 0.1)+
   theme_bw()+
   ylim(0.0,4)+
   theme(axis.text=element_text(size=8),
         axis.title=element_text(size=10,))+
   labs(x = "P rate",
        y= "Yield t/ha",
        title = "North")+
   annotation_custom(Pvalue_on_graph)+
   annotate("text", x = "25", y= 4,label = "Small sample size because zone is at an angle", size = 1.5)
 
 ##save the results of the zone strip work
 zone_north
 ggsave(path= graph_path, filename = "t-test_zone_north_strip.png", device = "png" ,
        width = 20, height = 10, units = "cm")
 write.csv(zone_north_av_0_25_100vs50, paste0(graph_path,"/t_testzone_north_av.csv"))
 
 
 ###########################################################################################################################################
 ##### Paired t test for zone strip South ####
 
 ##average the yield values in each line segment - this ensure I have the same number of points
 str(seg_ID)
 zone_south <- filter(seg_ID, zone == "South" )
 zone_south_av <- group_by(zone_south,SegmentID, P_Rates ) %>% 
   summarise_all(mean)
 
 group_by(zone_south_av, P_Rates) %>% 
   summarise(mean(YldMassDry))
 
 zone_south_av$P_Rate_as_factor <- as.factor(zone_south_av$P_Rates)
 
 zone_south_av_0vs50 <- filter(zone_south_av, P_Rates == 0 | P_Rates== 50 )
 zone_south_av_25vs50 <- filter(zone_south_av, P_Rates == 25 | P_Rates== 50 )
 zone_south_av_100vs50 <- filter(zone_south_av, P_Rates == 100 | P_Rates== 50 )
 
 
 
 # http://www.sthda.com/english/wiki/paired-samples-t-test-in-r
 
 #test assumptions
 
 
 # # compute the difference
 # d <- with(zone_av, 
 #           YLDMASSDR[P_Rate == 0] - YLDMASSDR[P_Rate == 50])
 # # Shapiro-Wilk normality test for the differences
 # shapiro.test(d) # => p-value = 0.5161
 # 
 # I think this means that it is  normally distrubuted
 
 #remove one point that is not duplicated
 zone_south_av_25vs50
 zone_south_av_25vs50 <- filter(zone_south_av_25vs50, SegmentID != 49)
 
 
 zone_south_av_0vs50_res <- t.test(YldMassDry ~ P_Rates, data = zone_south_av_0vs50, paired = TRUE)
 zone_south_av_25vs50_res <- t.test(YldMassDry ~ P_Rates, data = zone_south_av_25vs50, paired = TRUE)
 zone_south_av_100vs50_res <- t.test(YldMassDry ~ P_Rates, data = zone_south_av_100vs50, paired = TRUE)
 
 #test 1 results
 zone_south_av_0vs50_res
 #Report values from the t.test
 zone_south_av_0vs50_res_sig  <-
   data.frame(P_value = as.double(zone_south_av_0vs50_res$p.value),
              Mean_diff = (zone_south_av_0vs50_res$estimate)) %>%
   mutate(
     rounded = abs(round(Mean_diff, 2)),
     Significant = case_when(P_value < 0.05 ~ "significant",
                             TRUE ~ "not significant"))
 zone_south_av_0vs50_res_sig 
 
 #test 2 results
 zone_south_av_25vs50_res
 zone_south_av_25vs50_res_sig <-
   data.frame(P_value = as.double(zone_south_av_25vs50_res$p.value),
              Mean_diff = (zone_south_av_25vs50_res$estimate)) %>%
   mutate(
     rounded = abs(round(Mean_diff, 2)),
     Significant = case_when(P_value < 0.05 ~ "significant",
                             TRUE ~ "not significant"))
 zone_south_av_25vs50_res_sig 
 
 #test 3 results
 zone_south_av_100vs50_res
 zone_south_av_100vs50_res_sig <-
   data.frame(P_value = as.double(zone_south_av_100vs50_res$p.value),
              Mean_diff = (zone_south_av_100vs50_res$estimate)) %>%
   mutate(
     rounded = abs(round(Mean_diff, 2)),
     Significant = case_when(P_value < 0.05 ~ "significant",
                             TRUE ~ "not significant"))
 
 zone_south_av_100vs50_res_sig 
 
 
 p_vlaue_text_south <- paste0("Yield at P 50 is P 0 minus ", zone_south_av_0vs50_res_sig$rounded, " and is ", 
                             zone_south_av_0vs50_res_sig$Significant, "\n",
                             "Yield at P 50 is P 25 minus ", zone_south_av_25vs50_res_sig$rounded, " and is ", 
                             zone_south_av_25vs50_res_sig$Significant,  "\n",
                             "Yield at P 100 is P 50 plus ", zone_south_av_100vs50_res_sig$rounded, " and is ", 
                             zone_south_av_100vs50_res_sig$Significant, collapse = "\n")
 
 
 
 
 
 print(p_vlaue_text_south)
 
 
 
 library(grid)
 Pvalue_on_graph <- grobTree(textGrob(p_vlaue_text_south, x=0.1,  y=0.10, hjust=0,
                                      gp=gpar(col="black", fontsize=6, fontface="italic")))
 
 
 
 
 # Compute t-test - this is done for 0 vs 100 for all values in the strip
 zone_south_av
 group_by(zone_south_av, P_Rates) %>% 
   summarise(mean(YldMassDry))
 group_by(zone_south_av, P_Rates) %>% 
   summarise(median(YldMassDry))
 
 zone_south <- ggplot( zone_south_av, aes(P_Rate_as_factor, YldMassDry))+
   geom_boxplot(alpha=0.1)+
   geom_point(colour = "blue", alpha = 0.1)+
   theme_bw()+
   ylim(0.0,4)+
   theme(axis.text=element_text(size=8),
         axis.title=element_text(size=10,))+
   labs(x = "P rate",
        y= "Yield t/ha",
        title = "South")+
   annotation_custom(Pvalue_on_graph)
 
 ##save the results of the zone strip work
 zone_south
 ggsave(path= graph_path, filename = "t-test_zone_south_strip.png", device = "png" ,
        width = 20, height = 10, units = "cm")
 write.csv(zone_south_av, paste0(graph_path,"/t_testzone_south_av.csv"))
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
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
 
 Craig_Muir_Dunns <- filter(harm_database,
                                   Paddock_tested == "Dunns") %>% 
   dplyr::select(5: 11)


 Craig_Muir_Dunns
 
 
  TSpecial <- ttheme_minimal(base_size = 8)
table1 <- tableGrob(Craig_Muir_Dunns , rows = NULL, theme=TSpecial )
#table2 <- tableGrob(table_segments, rows = NULL, theme=TSpecial)

#get the name of the paddock...

paddock <- "Dunns"
test <- textGrob(paddock)
 ####################################################################################################################################
 ## Arrange the outputs onto one page
segments
zone_north
zone_south
paddock
Craig_Muir_Dunns

 collection <- grid.arrange(zone_north, zone_south, table1, segments, test,  nrow = 5, 
              layout_matrix = cbind(c(1,1,5,4,4), c(2,2,3,4,4)))
             
collection

ggsave(path= graph_path, filename = paste0(paddock, "_collection.png"), device = "png", 
       width = 21, height = 15, units = "cm", collection)
 
