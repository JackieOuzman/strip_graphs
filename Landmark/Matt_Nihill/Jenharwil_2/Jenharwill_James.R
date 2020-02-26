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

"\\FSSA2-ADL\clw-share1\Microlab\value_soil_testing_prj\Yield_data\Landmark\Matt_Nihill\Jenharwil_2\James 2\James_Yld_Seg_ID_zone.csv"



name_of_path <-
  file.path(
    "W:",
    "value_soil_testing_prj",
    "Yield_data",
    "Landmark",
    "Matt_Nihill",
    "Jenharwil_2",
    "James 2",
    
    "James_Yld_Seg_ID_zone.csv"
  )
seg_ID <- read_csv(name_of_path)

graph_path <-
  file.path("W:",
            "value_soil_testing_prj",
            "Yield_data",
            "Landmark",
            "Matt_Nihill",
            "Jenharwil_2",
            "James 2")
seg_ID

seg_ID <-
  rename(seg_ID, "P_Rates" = "Rate", 
         "zone" =  "Zone")




###########################################################################################################################
### set up data so its generic

#Define the rates
unique(seg_ID$P_Rates)
str(seg_ID)

Grower_rate = 80
rate1 = 0
rate2 = 150
#rate3 = 110

#Define the zones
unique(seg_ID$zone)
zone1 <- "Low"
zone2 <- "High"

#change clm heading to match YldMassDry and zone
#seg_ID <- rename(seg_ID,"Yld_Mass_D" = "YldMassDry")
seg_ID <- rename(seg_ID,"YldMassDry" = "Yld_Mass_D")



############################################################################################################################
### clean the data removing zero values
unique(seg_ID$zone)
str(seg_ID)

## remove all the values in the data set that won't be included in the analysis this is when distance on line = 0
seg_ID <- filter(seg_ID,
                 DistOnLine != 0)

#The farmer practice wasnt really a true strip but I want to use the data so I need to remove row when we have no yield
seg_ID <- filter(seg_ID,
                 YldMassDry != 0)

#############################################################################################################################
##### t test per segment in the strip Via Andrea method####

#Prep the data so I can check what am I testing (look at Harms list)
head(seg_ID)
unique(seg_ID$P_Rates)
seg_ID_rate1vsGR <- filter(seg_ID, P_Rates == rate1 | P_Rates== Grower_rate )
seg_ID_rate2vsGR <- filter(seg_ID, P_Rates == rate2 | P_Rates== Grower_rate )
#seg_ID_rate3vsGR <- filter(seg_ID, P_Rates == rate3 | P_Rates== Grower_rate )

# Compute t-test - this is done for 0 vs 100 for each segment values in the strip
# Basically what you need to do is subset your data based on segmentID and perform the t-test for each group of data.
# 

#Now put it in the loop with my test data! - its not looping through everything ? try making a list

#I want a list of all values in segment ID to uss in the loop
list <- unique(seg_ID_rate1vsGR$SegmentID)

############################################################################################################
#Run as a loop for test 1 rate 1 vs GR
head(seg_ID_rate1vsGR)
Output_rate1vsGR= data.frame() #create empty df for output
for (i in list){
  segment_data = subset(seg_ID_rate1vsGR, SegmentID == i)
  
  # Method 1: The data are saved in two different numeric vectors.
  data_x=subset(segment_data, P_Rates==Grower_rate, select = YldMassDry, drop = TRUE)
  data_y=subset(segment_data, P_Rates==rate1, select = YldMassDry, drop = TRUE)
  res_method1 <-t.test(data_x,data_y, var.equal = FALSE)
  
  p_vlaue <- res_method1$p.value
  segment_name <- unique(segment_data$SegmentID)
  result <- data.frame(SegmentID = segment_name, P_value = p_vlaue)
  Output_rate1vsGR = rbind(Output_rate1vsGR, result)
} 

#convert the P value into NS or Sig at 0.05
Output_rate1vsGR <- mutate(Output_rate1vsGR, 
                 Significant = case_when(
                 P_value < 0.05 ~ "significant",
                 TRUE ~ "not significant"
               ))
#To make this meaningful I need to summaries the input data and join it to the t - test results
head(Output_rate1vsGR)
seg_ID_rate1vsGR_summary <- group_by(seg_ID_rate1vsGR,
                                          SegmentID, zone, P_Rates ) %>% 
  summarise_all(mean) %>% 
  ungroup()
#join output to summary

#what comparison did I run? - name the df to reflect this
seg_ID_rate1vsGR_summary <- left_join(seg_ID_rate1vsGR_summary, Output_rate1vsGR)
seg_ID_rate1vsGR_summary <- mutate(seg_ID_rate1vsGR_summary, comparison = "rate1vsGR" )

seg_ID_rate1vsGR_summary
#####################################################################################################
#Run as a loop for  test 2 rate 2 vs GR
Output_rate2vsGR= data.frame() #create empty df for output
for (i in list){
  segment_data = subset(seg_ID_rate2vsGR, SegmentID == i)
  
  # Method 1: The data are saved in two different numeric vectors.
  data_x=subset(segment_data, P_Rates==rate2, select = YldMassDry, drop = TRUE)
  data_y=subset(segment_data, P_Rates==Grower_rate, select = YldMassDry, drop = TRUE)
  res_method1 <-t.test(data_x,data_y, var.equal = FALSE)
  
  p_vlaue <- res_method1$p.value
  segment_name <- unique(segment_data$SegmentID)
  result <- data.frame(SegmentID = segment_name, P_value = p_vlaue)
  Output_rate2vsGR = rbind(Output_rate2vsGR, result)
} 

#convert the P value into NS or Sig at 0.05
Output_rate2vsGR <- mutate(Output_rate2vsGR, 
                       Significant = case_when(
                         P_value < 0.05 ~ "significant",
                         TRUE ~ "not significant"
                       ))

#To make this meaningful I need to summaries the input data and join it to the t - test results
seg_ID_rate2vsGR_summary <- group_by(seg_ID_rate2vsGR,
                                 SegmentID, zone, P_Rates ) %>% 
  summarise_all(mean) %>% 
  ungroup()
#join output to summary
seg_ID_rate2vsGR_summary <- left_join(seg_ID_rate2vsGR_summary, Output_rate2vsGR)
#what comparison did I run? - name the df to reflect this
seg_ID_rate2vsGR_summary <- mutate(seg_ID_rate2vsGR_summary, comparison = "rate2vsGR" )


#####################################################################################################
#Run as a loop for  test 3 rate 3 vs GR
Output_rate3vsGR= data.frame() #create empty df for output
for (i in list){
  segment_data = subset(seg_ID_rate3vsGR, SegmentID == i)
  
  # Method 1: The data are saved in two different numeric vectors.
  data_x=subset(segment_data, P_Rates==rate3, select = YldMassDry, drop = TRUE)
  data_y=subset(segment_data, P_Rates==Grower_rate, select = YldMassDry, drop = TRUE)
  res_method1 <-t.test(data_x,data_y, var.equal = FALSE)
  
  p_vlaue <- res_method1$p.value
  segment_name <- unique(segment_data$SegmentID)
  result <- data.frame(SegmentID = segment_name, P_value = p_vlaue)
  Output_rate3vsGR = rbind(Output_rate3vsGR, result)
} 

#convert the P value into NS or Sig at 0.05
Output_rate3vsGR <- mutate(Output_rate3vsGR, 
                           Significant = case_when(
                             P_value < 0.05 ~ "significant",
                             TRUE ~ "not significant"
                           ))

#To make this meaningful I need to summaries the input data and join it to the t - test results
seg_ID_rate3vsGR_summary <- group_by(seg_ID_rate3vsGR,
                                     SegmentID, zone, P_Rates ) %>% 
  summarise_all(mean) %>% 
  ungroup()
#join output to summary
seg_ID_rate3vsGR_summary <- left_join(seg_ID_rate3vsGR_summary, Output_rate3vsGR)
#what comparison did I run? - name the df to reflect this
seg_ID_rate3vsGR_summary <- mutate(seg_ID_rate3vsGR_summary, comparison = "rate3vsGR" )


###############################################################################################################

##Join the two strip data results togther join info from test 1 to test 2 and test 3
head(seg_ID_rate1vsGR_summary)
head(seg_ID_rate2vsGR_summary)
#head(seg_ID_rate3vsGR_summary)

seg_ID_rate1_2vsGR_summary <- rbind(seg_ID_rate1vsGR_summary, seg_ID_rate2vsGR_summary)
#seg_ID_rate1_3vsGR_summary <- rbind(seg_ID_rate1vsGR_summary, seg_ID_rate2vsGR_summary, seg_ID_rate3vsGR_summary)

###remove some of the data from my workspace
rm(list = c("Output_rate1vsGR", 
            "Output_rate2vsGR", 
            "seg_ID_rate1vsGR_summary", 
            "seg_ID_rate2vsGR_summary",
            "res_method1",
            "result", 
            "seg_ID_rate1vsGR", 
            "seg_ID_rate2vsGR" 
            )) 


##############################################################################################################
#########    plot results  of t.test ########################################################################
#seg_ID_rate1_2vsGR_summary <- seg_ID_rate1_3vsGR_summary #just so the names the same
seg_ID_rate1_2vsGR_summary$P_Rate_as_factor <- as.factor(seg_ID_rate1_2vsGR_summary$P_Rates)
str(seg_ID_rate1_2vsGR_summary)
unique(seg_ID_rate1_2vsGR_summary$zone)

#what is the area of the zones this can be added to the graph?
filter(seg_ID_rate1_2vsGR_summary, zone == zone1) %>% 
  summarise(min_zone = min(SegmentID),
            max_zone = max(SegmentID))

filter(seg_ID_rate1_2vsGR_summary, zone == zone2) %>% 
  summarise(min_zone = min(SegmentID),
            max_zone = max(SegmentID))
head(seg_ID_rate1_2vsGR_summary)
#create a table that count how many signifcant vs non significant values we have
table_segments <- group_by(seg_ID_rate1_2vsGR_summary,comparison,   Significant ) %>% 
  count(Significant) %>% 
  summarise(count = (n/2)) 
table_segments$count <- round(table_segments$count, 0)



str(seg_ID_rate1_2vsGR_summary)
### Plot the results 
segments <- ggplot(seg_ID_rate1_2vsGR_summary, aes(SegmentID , YldMassDry, group = P_Rate_as_factor))+
  geom_line(size=1, alpha=0.4, aes( color = P_Rate_as_factor ))+
  scale_color_manual(values=c('darkgrey','green', 'blue', 'red'), name  ="N Rates")+
  theme_bw()+
  ylim(0.0,6)+
  labs(x= "Distance along the strip",
       y = "Yield t/ha",
       title = "",
       subtitle = "",
       caption = "")+
   annotate("rect", xmin = 80, xmax = 90, ymin = 0, ymax = 6, #Zone 1
           alpha = .2) +
  annotate("text", x = 85, y= 1,label = zone1)+
  
   annotate("rect", xmin = 52, xmax = 62, ymin = 0, ymax = 6, #zone 2
            alpha = .2)+
  annotate("text", x = 57, y= 1,label = zone2)
#+
 # annotate("text", x = 40, y= 1,label = "Missing data")


##save the results of the segment work
segments #this is the graph


ggsave(path= graph_path, filename = "t-test_segments.png", device = "png" ,
       width = 20, height = 10, units = "cm")

write.csv(seg_ID_rate1_2vsGR_summary, paste0(graph_path,"/t_test_segments.csv"))
write.csv(table_segments, paste0(graph_path,"/table_segments.csv"))



##########################################################################################################################################
###ONLY DO THIS STEP IF WE ARE MISSING ZONE DATA

##### Paired t test for whole strip ####

##average the yield values in each line segment - this ensure I have the same number of points
str(seg_ID)

strip_av <- group_by(seg_ID,SegmentID,  P_Rates ) %>% 
  summarise_all(mean)
 

group_by(strip_av, P_Rates) %>% 
  summarise(mean(YldMassDry))

strip_rate1vsGR <- filter(strip_av, P_Rates == rate1 | P_Rates== Grower_rate )
strip_rate2vsGR <- filter(strip_av, P_Rates == rate2 | P_Rates== Grower_rate )




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
 
strip_rate1vsGR
strip_rate1vsGR_res <- t.test(YldMassDry ~ P_Rates, data = strip_rate1vsGR, paired = TRUE)
strip_rate2vsGR_res <- t.test(YldMassDry ~ P_Rates, data = strip_rate2vsGR, paired = TRUE)


 #Report values from the t.test

### test 1
strip_rate1vsGR_res
strip_rate1vsGR_res_sig <-
   data.frame(P_value = as.double(strip_rate1vsGR_res$p.value),
              Mean_diff = (strip_rate1vsGR_res$estimate)) %>%
   mutate(
     rounded = abs(round(Mean_diff, 2)),
     significant = case_when(P_value < 0.05 ~ "significant",
                        TRUE ~ "not significant"))
strip_rate1vsGR_res_sig
#### test 2

strip_rate2vsGR_res
strip_rate2vsGR_res_sig <-
  data.frame(P_value = as.double(strip_rate2vsGR_res$p.value),
             Mean_diff = (strip_rate2vsGR_res$estimate)) %>%
  mutate(
    rounded = abs(round(Mean_diff, 2)),
    significant = case_when(P_value < 0.05 ~ "significant",
                            TRUE ~ "not significant"))
strip_rate2vsGR_res_sig


p_vlaue_text_strip <- paste0("Yield at P ", rate1, " is  P ", Grower_rate,  " plus ", strip_rate1vsGR_res_sig$rounded, " and is ", 
                             strip_rate1vsGR_res_sig$significant, "\n",
                             
                             "Yield at P ", Grower_rate, " is  P ", rate2 ,  " minus ", strip_rate2vsGR_res_sig$rounded, " and is ", 
                             strip_rate2vsGR_res_sig$significant, collapse = "\n")

 print(p_vlaue_text_strip)

 Pvalue_on_graph <- grobTree(textGrob(p_vlaue_text_strip, x=0.1,  y=0.90, hjust=0,
                                      gp=gpar(col="black", fontsize=6, fontface="italic")))
 
 
 str(strip_av)
 strip_av$P_Rate_as_factor <- as.factor(strip_av$P_Rates)
 # Compute t-test - this is done for 0 vs 100 for all values in the strip

 strip <- ggplot( strip_av, aes(P_Rate_as_factor, YldMassDry))+
   geom_boxplot(alpha=0.1)+
   stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                width = .75, linetype = "dashed")+
   geom_point(colour = "blue", alpha = 0.1)+
   theme_bw()+
   ylim(0.0,4)+
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
 
 
 
 
################################################################################################################################### 

 ##### Paired t test for zone strip Zone 1 ####
 
 ##average the yield values in each line segment - this ensure I have the same number of points
 unique(seg_ID$zone)
 
 zone_1 <- filter(seg_ID, zone == zone1 )
 zone_av_1 <- group_by(zone_1,SegmentID, P_Rates ) %>% 
   summarise_all(mean)
 
 zone_av_1
 unique(zone_1$zone)
 
 group_by(zone_av_1, P_Rates) %>% 
  summarise(mean(YldMassDry))
 
zone_av_1$P_Rate_as_factor <- as.factor(zone_av_1$P_Rates)


zone_av_1 
zone_av_1_rate1vsGR <- filter(zone_av_1, P_Rates == rate1 | P_Rates== Grower_rate )
zone_av_1_rate2vsGR <- filter(zone_av_1, P_Rates == rate2 | P_Rates== Grower_rate )
#zone_av_1_rate3vsGR <- filter(zone_av_1, P_Rates == rate3 | P_Rates== Grower_rate )
 
zone_av_1_rate2vsGR <- filter(zone_av_1_rate2vsGR, SegmentID != 80 )


# http://www.sthda.com/english/wiki/paired-samples-t-test-in-r
 
 #test assumptions
 
 
 # # compute the difference
 # d <- with(zone_av, 
 #           YLDMASSDR[P_Rate == 0] - YLDMASSDR[P_Rate == 50])
 # # Shapiro-Wilk normality test for the differences
 # shapiro.test(d) # => p-value = 0.5161
 # 
 # I think this means that it is  normally distrubuted
 
zone_av_1_rate1vsGR_res <- t.test(YldMassDry ~ P_Rates, data = zone_av_1_rate1vsGR, paired = TRUE)
zone_av_1_rate2vsGR_res <- t.test(YldMassDry ~ P_Rates, data = zone_av_1_rate2vsGR, paired = TRUE)
#zone_av_1_rate3vsGR_res <- t.test(YldMassDry ~ P_Rates, data = zone_av_1_rate3vsGR, paired = TRUE)



#test 1 results
zone_av_1_rate1vsGR_res
 #Report values from the t.test
zone_av_1_rate1vsGR_res_sig <-
   data.frame(P_value = as.double(zone_av_1_rate1vsGR_res$p.value),
              Mean_diff = (zone_av_1_rate1vsGR_res$estimate)) %>%
   mutate(
     rounded = abs(round(Mean_diff, 2)),
     Significant = case_when(P_value < 0.05 ~ "significant",
                        TRUE ~ "not significant"))
zone_av_1_rate1vsGR_res_sig 
 
#test 2 results
zone_av_1_rate2vsGR_res
#Report values from the t.test
zone_av_1_rate2vsGR_res_sig <-
  data.frame(P_value = as.double(zone_av_1_rate2vsGR_res$p.value),
             Mean_diff = (zone_av_1_rate2vsGR_res$estimate)) %>%
  mutate(
    rounded = abs(round(Mean_diff, 2)),
    Significant = case_when(P_value < 0.05 ~ "significant",
                            TRUE ~ "not significant"))

#test 3 results
# zone_av_1_rate3vsGR_res
# #Report values from the t.test
# zone_av_1_rate3vsGR_res_sig <-
#   data.frame(P_value = as.double(zone_av_1_rate3vsGR_res$p.value),
#              Mean_diff = (zone_av_1_rate3vsGR_res$estimate)) %>%
#   mutate(
#     rounded = abs(round(Mean_diff, 2)),
#     Significant = case_when(P_value < 0.05 ~ "significant",
#                             TRUE ~ "not significant"))

zone_av_1_rate1vsGR_res_sig 
zone_av_1_rate2vsGR_res_sig
#zone_av_1_rate3vsGR_res_sig

# positive_negative_rate1_GRS <- 
mean_zone_av_1 <-  group_by(zone_av_1, P_Rates) %>% 
  summarise(mean(YldMassDry))
mean_zone_av_1
positive_neg_value_GR_rate1_zone1 <- ifelse(filter(mean_zone_av_1, P_Rates == Grower_rate) 
                                            - filter(mean_zone_av_1, P_Rates == rate1)>0, "plus", "minus") 
positive_neg_value_GR_rate1_zone1 <- positive_neg_value_GR_rate1_zone1[1,2]

positive_neg_value_rate2_GR_zone1 <- ifelse(filter(mean_zone_av_1, P_Rates == rate2) 
                                            - filter(mean_zone_av_1, P_Rates == Grower_rate)>0, "plus", "minus")
positive_neg_value_rate2_GR_zone1 <- positive_neg_value_rate2_GR_zone1[1,2]

# positive_neg_value_rate3_GR_zone1 <- ifelse(filter(mean_zone_av_1, P_Rates == rate3) 
#                                             - filter(mean_zone_av_1, P_Rates == Grower_rate)>0, "plus", "minus")
# positive_neg_value_rate3_GR_zone1 <- positive_neg_value_rate3_GR_zone1[1,2]



p_vlaue_text_zone_1 <- paste0("Yield at N ", Grower_rate, " is  N ", rate1, " " ,positive_neg_value_GR_rate1_zone1, " ", 
                              zone_av_1_rate1vsGR_res_sig$rounded, " and is ", 
                              zone_av_1_rate1vsGR_res_sig$Significant, "\n",
                              
                              "Yield at N ", rate2, " is  N ", Grower_rate, " " ,positive_neg_value_rate2_GR_zone1, " ", 
                              zone_av_1_rate2vsGR_res_sig$rounded, " and is ", 
                              zone_av_1_rate2vsGR_res_sig$Significant, collapse = "\n")
                              
                              # "Yield at P ", rate3, " is  P ", Grower_rate , " " ,positive_neg_value_rate3_GR_zone1, " ", 
                              # zone_av_1_rate3vsGR_res_sig$rounded, " and is ", 
                              # zone_av_1_rate3vsGR_res_sig$Significant, collapse = "\n")
print(p_vlaue_text_zone_1)
 
 library(grid)
 Pvalue_on_graph <- grobTree(textGrob(p_vlaue_text_zone_1, x=0.1,  y=0.10, hjust=0,
                                      gp=gpar(col="black", fontsize=6, fontface="italic")))
 
 
 
 
 # Compute t-test - this is done for rate1 vs GS for all values in the strip
 zone_av_1
 zone_1 <- ggplot( zone_av_1, aes(P_Rate_as_factor, YldMassDry))+
   geom_boxplot(alpha=0.1)+
   geom_point(colour = "blue", alpha = 0.1)+
   stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                width = .75, linetype = "dashed")+
   theme_bw()+
   ylim(2.5,6)+
   theme(axis.text=element_text(size=8),
         axis.title=element_text(size=10,))+
   labs(x = "N rate",
        y= "Yield t/ha",
        title = zone1)+
   annotation_custom(Pvalue_on_graph)
 
 ##save the results of the zone strip work
 zone_1
 ggsave(path= graph_path, filename = "t-test_zone_zone1_strip.png", device = "png" ,
        width = 20, height = 10, units = "cm")
 write.csv(zone_av_1, paste0(graph_path,"/t_testzone_zone1_av.csv"))
 
 
 ###########################################################################################################################################
 ##### Paired t test for zone strip 2 ####
 
 zone_2 <- filter(seg_ID, zone == zone2 )
 #remove a few segements that are not duplicated...
 #zone_2 <- filter(zone_2, SegmentID != 85)
 
 zone_av_2 <- group_by(zone_2,SegmentID, P_Rates ) %>% 
   summarise_all(mean)
 
 zone_av_2
 unique(zone_2$zone)
 
 group_by(zone_av_2, P_Rates) %>% 
   summarise(mean(YldMassDry))
 
 zone_av_2$P_Rate_as_factor <- as.factor(zone_av_2$P_Rates)
 
 
 zone_av_2 
 zone_av_2_rate1vsGR <- filter(zone_av_2, P_Rates ==  rate1 | P_Rates==   Grower_rate)
 zone_av_2_rate2vsGR <- filter(zone_av_2, P_Rates ==  rate2 | P_Rates==  Grower_rate )
 #zone_av_2_rate3vsGR <- filter(zone_av_2, P_Rates ==  rate3 | P_Rates==  Grower_rate )
 
 # http://www.sthda.com/english/wiki/paired-samples-t-test-in-r
 
 #test assumptions
 
 
 # # compute the difference
 # d <- with(zone_av, 
 #           YLDMASSDR[P_Rate == 0] - YLDMASSDR[P_Rate == 50])
 # # Shapiro-Wilk normality test for the differences
 # shapiro.test(d) # => p-value = 0.5161
 # 
 # I think this means that it is  normally distrubuted
 
 zone_av_2_rate1vsGR_res <- t.test(YldMassDry ~ P_Rates, data = zone_av_2_rate1vsGR, paired = TRUE)
 zone_av_2_rate2vsGR_res <- t.test(YldMassDry ~ P_Rates, data = zone_av_2_rate2vsGR, paired = TRUE)
 #zone_av_2_rate3vsGR_res <- t.test(YldMassDry ~ P_Rates, data = zone_av_2_rate3vsGR, paired = TRUE)
 
 #test 1 results
 zone_av_2_rate1vsGR_res
 #Report values from the t.test
 zone_av_2_rate1vsGR_res_sig <-
   data.frame(P_value = as.double(zone_av_2_rate1vsGR_res$p.value),
              Mean_diff = (zone_av_2_rate1vsGR_res$estimate)) %>%
   mutate(
     rounded = abs(round(Mean_diff, 2)),
     Significant = case_when(P_value < 0.05 ~ "significant",
                             TRUE ~ "not significant"))
 zone_av_2_rate1vsGR_res_sig 
 
 #test 2 results
 zone_av_2_rate2vsGR_res
 #Report values from the t.test
 zone_av_2_rate2vsGR_res_sig <-
   data.frame(P_value = as.double(zone_av_2_rate2vsGR_res$p.value),
              Mean_diff = (zone_av_2_rate2vsGR_res$estimate)) %>%
   mutate(
     rounded = abs(round(Mean_diff, 2)),
     Significant = case_when(P_value < 0.05 ~ "significant",
                             TRUE ~ "not significant"))
 # #test 3 results
 # zone_av_2_rate2vsGR_res
 # #Report values from the t.test
 # zone_av_2_rate3vsGR_res_sig <-
 #   data.frame(P_value = as.double(zone_av_2_rate3vsGR_res$p.value),
 #              Mean_diff = (zone_av_2_rate3vsGR_res$estimate)) %>%
 #   mutate(
 #     rounded = abs(round(Mean_diff, 2)),
 #     Significant = case_when(P_value < 0.05 ~ "significant",
 #                             TRUE ~ "not significant"))
 
 zone_av_2_rate1vsGR_res_sig 
 zone_av_2_rate2vsGR_res_sig 
 #zone_av_2_rate3vsGR_res_sig 
 
# positive_negative_rate1_GRS <- 
 mean_zone_av_2 <-  group_by(zone_av_2, P_Rates) %>% 
   summarise(mean(YldMassDry))
 positive_neg_value_GR_rate1 <- ifelse(filter(mean_zone_av_2, P_Rates == Grower_rate) 
                                       - filter(mean_zone_av_2, P_Rates == rate1)>0, "plus", "minus") 
 positive_neg_value_GR_rate1 <- positive_neg_value_GR_rate1[1,2]
 
 positive_neg_value_rate2_GR <- ifelse(filter(mean_zone_av_2, P_Rates == rate2) 
                                       - filter(mean_zone_av_2, P_Rates == Grower_rate)>0, "plus", "minus")
 positive_neg_value_rate2_GR <- positive_neg_value_rate2_GR[1,2]

 positive_neg_value_rate3_GR <- ifelse(filter(mean_zone_av_2, P_Rates == rate3) - filter(mean_zone_av_2, P_Rates == Grower_rate)>0, "plus", "minus")
 positive_neg_value_rate3_GR <- positive_neg_value_rate3_GR[1,2]
 

 
 p_vlaue_text_zone_2 <- paste0("Yield at N ", Grower_rate, " is  N ", rate1, " " ,positive_neg_value_GR_rate1, " ", zone_av_2_rate1vsGR_res_sig$rounded, " and is ", 
                               zone_av_2_rate1vsGR_res_sig$Significant, "\n",
                               
                               "Yield at N ", rate2, " is  N ", Grower_rate, " " ,positive_neg_value_rate2_GR, " ", zone_av_2_rate2vsGR_res_sig$rounded, " and is ", 
                               zone_av_2_rate2vsGR_res_sig$Significant, collapse = "\n")
                               
                               # "Yield at P ", rate3, " is  P ", Grower_rate , " " ,positive_neg_value_rate3_GR, " ", zone_av_2_rate3vsGR_res_sig$rounded, " and is ", 
                               # zone_av_2_rate3vsGR_res_sig$Significant, collapse = "\n")
 print(p_vlaue_text_zone_2)
 
 library(grid)
 Pvalue_on_graph <- grobTree(textGrob(p_vlaue_text_zone_2, x=0.1,  y=0.10, hjust=0,
                                      gp=gpar(col="black", fontsize=6, fontface="italic")))
 
 
 
 
 # Compute t-test - this is done for rate1 vs GS for all values in the strip
 zone_av_2
 zone_2 <- ggplot( zone_av_2, aes(P_Rate_as_factor, YldMassDry))+
   geom_boxplot(alpha=0.1)+
   geom_point(colour = "blue", alpha = 0.1)+
   stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                width = .75, linetype = "dashed")+
   theme_bw()+
   ylim(2.5,6)+
   theme(axis.text=element_text(size=8),
         axis.title=element_text(size=10,))+
   labs(x = "N rate",
        y= "Yield t/ha",
        title = zone2)+
   annotation_custom(Pvalue_on_graph)
 
 ##save the results of the zone strip work
 zone_2
 ggsave(path= graph_path, filename = "t-test_zone_zone2_strip.png", device = "png" ,
        width = 20, height = 10, units = "cm")
 write.csv(zone_av_2, paste0(graph_path,"/t_testzone_zone2_av.csv"))
 
 
 
#####################################################################################################################################
### should get this from harms database
 #bring in data from the most current database
 "\\FSSA2-ADL\clw-share1\Microlab\value_soil_testing_prj\data_base\N&P 2019 data for analysis Vic and SA latest.xlsx"
 name_of_path_database <-
   file.path("W:", "value_soil_testing_prj", "data_base", "26022020_NP_data_base.xlsx")
 
 harm_database <- read_excel(name_of_path_database, sheet = "2019 full data", range = cell_cols("A:O"))
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
 Jenharwil_James2 <- filter(harm_database,
                            Paddock_tested == "James 2") %>% 
   dplyr::select(5, 6: 11)


 Jenharwil_James2

#make a table of the mean yield for zones
names(mean_zone_av_1) <- c("Rates", zone1)
names(mean_zone_av_2) <- c("Rates", zone2)

mean_zone_av_1_2 <- left_join(mean_zone_av_1, mean_zone_av_2)
mean_zone_av_1_2 <- round(mean_zone_av_1_2,2)
mean_zone_av_1_2 <-  lapply(mean_zone_av_1_2, as.character)
mean_zone_av_1_2 <- as.data.frame(mean_zone_av_1_2 )

  
mean_zone_av_1_2


TSpecial <- ttheme_minimal(base_size = 8)
table1 <- tableGrob(Jenharwil_James2 , rows = NULL, theme=TSpecial )
table2 <- tableGrob(mean_zone_av_1_2, rows = NULL, theme=TSpecial)

#get the name of the paddock...

paddock <- "James_2"


library(DT)
test <- textGrob(paddock)
 ####################################################################################################################################
 ## Arrange the outputs onto one page
segments
zone_1
zone_2
paddock


collection <- grid.arrange(zone_2, zone_1,  table1, segments, table2,  nrow = 5, 
              layout_matrix = cbind(c(1,1,5,4,4), c(2,2,3,4,4)))
             
collection

ggsave(path= graph_path, filename = paste0(paddock, "_collection.png"), device = "png", 
       width = 21, height = 15, units = "cm", collection)
 




##########################################################################################################################################

###DANS WORK
#1. bring in the files and append file names

mydir <- file.path("W:", "value_soil_testing_prj", "Yield_data", "Jenharwil", "James 2", "James 2_drop_box")
myfiles = list.files(path = mydir,
                     pattern = "*.csv",
                     full.names = TRUE)

myfiles

Urea_0kgha_High <- read_csv("W:/value_soil_testing_prj/Yield_data/Jenharwil/James 2/James 2_drop_box/Urea 0kgha High.csv") %>% 
  mutate(strip_name = "Urea_0kgha_High")
Urea_0kgha_Low <- read_csv("W:/value_soil_testing_prj/Yield_data/Jenharwil/James 2/James 2_drop_box/Urea 0kgha Low.csv") %>% 
  mutate(strip_name = "Urea_0kgha_Low")

Urea_150kgh_High <- read_csv("W:/value_soil_testing_prj/Yield_data/Jenharwil/James 2/James 2_drop_box/Urea 150kgha High.csv") %>% 
  mutate(strip_name = "Urea_150kgh_High")
Urea_150kgh_Low <- read_csv("W:/value_soil_testing_prj/Yield_data/Jenharwil/James 2/James 2_drop_box/Urea 150kgha Low.csv") %>% 
  mutate(strip_name = "Urea_150kgh_Low")


Urea_80kgha_High <- read_csv("W:/value_soil_testing_prj/Yield_data/Jenharwil/James 2/James 2_drop_box/Urea 80kgha High.csv") %>% 
  mutate(strip_name = "Urea_80kgha_High")
Urea_80kgha_Low <- read_csv("W:/value_soil_testing_prj/Yield_data/Jenharwil/James 2/James 2_drop_box/Urea 80kgha Low.csv") %>% 
  mutate(strip_name = "Urea_80kgha_Low")

#Join togther
James_Dans_wk <- rbind(Urea_0kgha_High,
                       Urea_0kgha_Low,
                       Urea_150kgh_High,
                       Urea_150kgh_Low,
                       Urea_80kgha_High,
                       Urea_80kgha_Low)
str(James_Dans_wk)
James_Dans_wk <- James_Dans_wk[,6:7]
#split the clm Urea_0kgha_High 0kgha
James_Dans_wk <- separate(James_Dans_wk, strip_name, into = paste(c("Fert", "Rate", "zone"), sep = "_"))
write.csv(James_Dans_wk, "W:/value_soil_testing_prj/Yield_data/Jenharwil/James 2/James 2_drop_box/James_Dans_wk.csv")

unique(James_Dans_wk$Rate)
# James_Dans_wk <- mutate(James_Dans_wk,
#                               Rate1 = case_when(
#   Rate == "0kgha" ~ "0",
#   Rate == "150kgh" ~ "150",
#   Rate == "80kgha" ~ "80",
#   TRUE ~ "Rate"))

str(James_Dans_wk)
unique(James_Dans_wk$Rate1)
James_Dans_wk_summary <- group_by(James_Dans_wk, Rate, zone ) %>% 
  summarise_all(mean)
James_Dans_wk_summary


unique(James_Dans_wk$Rate)
James_Dans_wk <- mutate(James_Dans_wk, Rate = fct_relevel(Rate, c( "0kgha" , "80kgha", "150kgh")) )
                                                                 

filter(James_Dans_wk, zone == "Low") %>% 
ggplot( aes(Rate, Yld_Mass_D))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed")+
  theme_bw()+
  ylim(2.5,6)+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,))+
  labs(x = "N rate",
       y= "Yield t/ha",
       title = "Dans  - Low")


filter(James_Dans_wk, zone == "High") %>% 
  ggplot( aes(Rate, Yld_Mass_D))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed")+
  theme_bw()+
  ylim(2.5,6)+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,))+
  labs(x = "N rate",
       y= "Yield t/ha",
       title = "Dans  - High")



