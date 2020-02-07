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
  file.path("W:", "value_soil_testing_prj", "Yield_data", "Craig_Muir", "Arthurs",  "Arthurs_yld_sed_ID_zone.csv")
seg_ID <-read_csv(name_of_path)

graph_path <- file.path("W:", "value_soil_testing_prj", "Yield_data", "Craig_Muir", "Arthurs")

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
seg_ID_20vs40 <- filter(seg_ID, P_Rates == 20 | P_Rates== 40 )
seg_ID_80vs40 <- filter(seg_ID, P_Rates == 80 | P_Rates== 40 )
seg_ID_0vs40 <- filter(seg_ID, P_Rates == 0 | P_Rates== 40 )



# Compute t-test - this is done for 0 vs 100 for each segment values in the strip
# Basically what you need to do is subset your data based on segmentID and perform the t-test for each group of data.
# 

#Now put it in the loop with my test data! - its not looping through everything ? try making a list

#I want a list of all values in segment ID to uss in the loop
list <- unique(seg_ID_20vs40$SegmentID)

############################################################################################################
#Run as a loop for 20_40 test 1
head(seg_ID_20vs40)
Output_20vs40= data.frame() #create empty df for output
for (i in list){
  segment_data = subset(seg_ID_20vs40, SegmentID == i)
  
  # Method 1: The data are saved in two different numeric vectors.
  data_x=subset(segment_data, P_Rates==20, select = YldMassDry, drop = TRUE)
  data_y=subset(segment_data, P_Rates==40, select = YldMassDry, drop = TRUE)
  res_method1 <-t.test(data_x,data_y, var.equal = FALSE)
  
  p_vlaue <- res_method1$p.value
  segment_name <- unique(segment_data$SegmentID)
  result <- data.frame(SegmentID = segment_name, P_value = p_vlaue)
  Output_20vs40 = rbind(Output_20vs40, result)
} 

#convert the P value into NS or Sig at 0.05
Output_20vs40 <- mutate(Output_20vs40, 
                 Significant = case_when(
                 P_value < 0.05 ~ "significant",
                 TRUE ~ "not significant"
               ))
#To make this meaningful I need to summaries the input data and join it to the t - test results
head(Output_20vs40)
seg_ID_20vs40_summary <- group_by(seg_ID_20vs40,
                                          SegmentID, zone, P_Rates ) %>% 
  summarise_all(mean) %>% 
  ungroup()
#join output to summary

#what comparison did I run? - name the df to reflect this
seg_ID_20vs40_summary <- left_join(seg_ID_20vs40_summary, Output_20vs40)
seg_ID_20vs40_summary <- mutate(seg_ID_20vs40_summary, comparison = "P20vsP40" )


#####################################################################################################
#Run as a loop for 80_40 test 2
Output_80vs40= data.frame() #create empty df for output
for (i in list){
  segment_data = subset(seg_ID_80vs40, SegmentID == i)
  
  # Method 1: The data are saved in two different numeric vectors.
  data_x=subset(segment_data, P_Rates==80, select = YldMassDry, drop = TRUE)
  data_y=subset(segment_data, P_Rates==40, select = YldMassDry, drop = TRUE)
  res_method1 <-t.test(data_x,data_y, var.equal = FALSE)
  
  p_vlaue <- res_method1$p.value
  segment_name <- unique(segment_data$SegmentID)
  result <- data.frame(SegmentID = segment_name, P_value = p_vlaue)
  Output_80vs40 = rbind(Output_80vs40, result)
} 

#convert the P value into NS or Sig at 0.05
Output_80vs40 <- mutate(Output_80vs40, 
                       Significant = case_when(
                         P_value < 0.05 ~ "significant",
                         TRUE ~ "not significant"
                       ))

#To make this meaningful I need to summaries the input data and join it to the t - test results
seg_ID_80vs40_summary <- group_by(seg_ID_80vs40,
                                 SegmentID, zone, P_Rates ) %>% 
  summarise_all(mean) %>% 
  ungroup()
#join output to summary
seg_ID_80vs40_summary <- left_join(seg_ID_80vs40_summary, Output_80vs40)
#what comparison did I run? - name the df to reflect this
seg_ID_80vs40_summary <- mutate(seg_ID_80vs40_summary, comparison = "P80vsP40" )


#####################################################################################################
#Run as a loop for 0_40 test 2
Output_0vs40= data.frame() #create empty df for output
for (i in list){
  segment_data = subset(seg_ID_0vs40, SegmentID == i)
  
  # Method 1: The data are saved in two different numeric vectors.
  data_x=subset(segment_data, P_Rates==0, select = YldMassDry, drop = TRUE)
  data_y=subset(segment_data, P_Rates==40, select = YldMassDry, drop = TRUE)
  res_method1 <-t.test(data_x,data_y, var.equal = FALSE)
  
  p_vlaue <- res_method1$p.value
  segment_name <- unique(segment_data$SegmentID)
  result <- data.frame(SegmentID = segment_name, P_value = p_vlaue)
  Output_0vs40 = rbind(Output_0vs40, result)
} 

#convert the P value into NS or Sig at 0.05
head(Output_0vs40)
Output_0vs40 <- mutate(Output_0vs40, 
                        Significant = case_when(
                          P_value < 0.05 ~ "significant",
                          TRUE ~ "not significant"
                        ))

#To make this meaningful I need to summaries the input data and join it to the t - test results
seg_ID_0vs40_summary <- group_by(seg_ID_0vs40,
                                  SegmentID, zone, P_Rates ) %>% 
  summarise_all(mean) %>% 
  ungroup()
#join output to summary
seg_ID_0vs40_summary <- left_join(seg_ID_0vs40_summary, Output_0vs40)
#what comparison did I run? - name the df to reflect this
seg_ID_0vs40_summary <- mutate(seg_ID_0vs40_summary, comparison = "P0vsP40" )


###############################################################################################################

##Join the two strip data results togther join info from test 1 to test 2 and test 3
head(seg_ID_80vs40_summary)
head(seg_ID_20vs40_summary)
head(seg_ID_0vs40_summary)

seg_ID_0vs0_80vs40summary <- rbind(seg_ID_80vs40_summary, seg_ID_20vs40_summary, seg_ID_0vs40_summary)

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
seg_ID_0vs0_80vs40summary$P_Rate_as_factor <- as.factor(seg_ID_0vs0_80vs40summary$P_Rates)
str(seg_ID_0vs0_80vs40summary)
unique(seg_ID_0vs0_80vs40summary$zone)

#what is the area of the zones this can be added to the graph?
filter(seg_ID_0vs0_80vs40summary, zone == "low") %>% 
  summarise(min_zone = min(SegmentID),
            max_zone = max(SegmentID))

filter(seg_ID_0vs0_80vs40summary, zone == "high") %>% 
  summarise(min_zone = min(SegmentID),
            max_zone = max(SegmentID))
head(seg_ID_0vs50_100s50summary)
#create a table that count how many signifcant vs non significant values we have
table_segments <- group_by(seg_ID_0vs0_80vs40summary,comparison,   Significant ) %>% 
  count(Significant) %>% 
  summarise(count = (n/2)) 
table_segments$count <- round(table_segments$count, 0)



str(seg_ID_0vs0_80vs40summary)
### Plot the results 
segments <- ggplot(seg_ID_0vs0_80vs40summary, aes(SegmentID , YldMassDry, group = P_Rate_as_factor))+
  geom_line(size=1, alpha=0.4, aes( color = P_Rate_as_factor ))+
  scale_color_manual(values=c('darkgrey','green', 'blue', 'red'), name  ="P Rates")+
  theme_bw()+
  ylim(0.0,6)+
  labs(x= "Distance along the strip",
       y = "Yield t/ha",
       title = "",
       subtitle = "",
       caption = "")+
   annotate("rect", xmin = 82, xmax = 92, ymin = 0, ymax = 6,
           alpha = .2) +
  annotate("text", x = 86, y= 1,label = "Flat")+
   annotate("rect", xmin = 10, xmax = 20, ymin = 0, ymax = 6,
            alpha = .2)+
  annotate("text", x = 15, y= 1,label = "Hill")+
  annotate("text", x = 32, y= 1,label = "Missing yield data")


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

sstrip_av_av_20vs40 <- filter(strip_av, P_Rates == 20 | P_Rates== 40 )
strip_av_av_80vs40 <- filter(strip_av, P_Rates == 80 | P_Rates== 40 )
strip_av_av_0vs40 <- filter(strip_av, P_Rates == 0 | P_Rates== 40 )


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
 
strip_av_av_20vs40
strip_avr_res20vs40 <- t.test(YldMassDry ~ P_Rates, data = strip_av_av_20vs40, paired = TRUE)
strip_avr_res80vs40 <- t.test(YldMassDry ~ P_Rates, data = strip_av_av_80vs40, paired = TRUE)
strip_avr_res0vs40 <- t.test(YldMassDry ~ P_Rates, data = strip_av_av_0vs40, paired = TRUE)

 #Report values from the t.test

### test 1
strip_avr_res20vs40
strip_avr_res20vs40_sig <-
   data.frame(P_value = as.double(strip_avr_res20vs40$p.value),
              Mean_diff = (strip_avr_res20vs40$estimate)) %>%
   mutate(
     rounded = abs(round(Mean_diff, 2)),
     significant = case_when(P_value < 0.05 ~ "significant",
                        TRUE ~ "not significant"))
strip_avr_res20vs40_sig
#### test 2

strip_avr_res80vs40_sig <-
   data.frame(P_value = as.double(strip_avr_res80vs40$p.value),
              Mean_diff = as.double(strip_avr_res80vs40$estimate)) %>%
   mutate(
     rounded = abs(round(Mean_diff, 2)),
     significant = case_when(P_value < 0.05 ~ "significant",
                        TRUE ~ "not significant")) 
strip_avr_res80vs40_sig
### test 3
strip_avr_res0vs40_sig <-
  data.frame(P_value = as.double(strip_avr_res0vs40$p.value),
             Mean_diff = as.double(strip_avr_res0vs40$estimate)) %>%
  mutate(
    rounded = abs(round(Mean_diff, 2)),
    significant = case_when(P_value < 0.05 ~ "significant",
                            TRUE ~ "not significant")) 

strip_avr_res0vs40_sig


p_vlaue_text_strip <- paste0("Yield at P 40 is P 0 plus ", strip_avr_res0vs40_sig$rounded, " and is ", 
                             strip_avr_res0vs40_sig$significant, "\n",
                                 "Yield at P 40 is P 20 plus ", strip_avr_res20vs40_sig$rounded, " and is ", 
                              strip_avr_res20vs40_sig$significant,  "\n",
                              "Yield at P 80 is P 40 plus ", strip_avr_res80vs40_sig$rounded, " and is ", 
                              strip_avr_res80vs40_sig$significant, collapse = "\n")
 print(p_vlaue_text_strip)

 Pvalue_on_graph <- grobTree(textGrob(p_vlaue_text_strip, x=0.1,  y=0.90, hjust=0,
                                      gp=gpar(col="black", fontsize=6, fontface="italic")))
 
 
 str(strip_av)
 strip_av$P_Rate_as_factor <- as.factor(strip_av$P_Rate)
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
 write.csv(seg_ID_0vs50_100s50summary, paste0(graph_path,"/t_test_strip_av.csv"))
 
 
 
 
 

 ##### Paired t test for zone strip LOW ####
 
 ##average the yield values in each line segment - this ensure I have the same number of points
 str(seg_ID)
 zone_low <- filter(seg_ID, zone == "low" )
 zone_low_av <- group_by(zone_low,SegmentID, P_Rates ) %>% 
   summarise_all(mean)

group_by(zone_low_av, P_Rates) %>% 
  summarise(mean(YldMassDry))
 
zone_low_av$P_Rate_as_factor <- as.factor(zone_low_av$P_Rates)
 
zone_low_av_20vs40 <- filter(zone_low_av, P_Rates == 20 | P_Rates== 40 )
zone_low_av_80vs40 <- filter(zone_low_av, P_Rates == 80 | P_Rates== 40 )
zone_low_av_0vs40 <- filter(zone_low_av, P_Rates == 0 | P_Rates== 40 )

 # http://www.sthda.com/english/wiki/paired-samples-t-test-in-r
 
 #test assumptions
 
 
 # # compute the difference
 # d <- with(zone_av, 
 #           YLDMASSDR[P_Rate == 0] - YLDMASSDR[P_Rate == 50])
 # # Shapiro-Wilk normality test for the differences
 # shapiro.test(d) # => p-value = 0.5161
 # 
 # I think this means that it is  normally distrubuted
 
zone_low_av_20vs40_res <- t.test(YldMassDry ~ P_Rates, data = zone_low_av_20vs40, paired = TRUE)
zone_low_av_80vs40_res <- t.test(YldMassDry ~ P_Rates, data = zone_low_av_80vs40, paired = TRUE)
zone_low_av_0vs40_res <- t.test(YldMassDry ~ P_Rates, data = zone_low_av_0vs40, paired = TRUE)

#test 1 results
zone_low_av_20vs40_res
 #Report values from the t.test
zone_low_av_20vs40_res_sig <-
   data.frame(P_value = as.double(zone_low_av_20vs40_res$p.value),
              Mean_diff = (zone_low_av_20vs40_res$estimate)) %>%
   mutate(
     rounded = abs(round(Mean_diff, 2)),
     Significant = case_when(P_value < 0.05 ~ "significant",
                        TRUE ~ "not significant"))
zone_low_av_20vs40_res_sig 
 
#test 2 results
zone_low_av_80vs40_res_sig <-
  data.frame(P_value = as.double(zone_low_av_80vs40_res$p.value),
             Mean_diff = (zone_low_av_80vs40_res$estimate)) %>%
  mutate(
    rounded = abs(round(Mean_diff, 2)),
    Significant = case_when(P_value < 0.05 ~ "significant",
                            TRUE ~ "not significant"))
zone_low_av_80vs40_res_sig 

#test 3 results
zone_low_av_0vs40_res_sig <-
  data.frame(P_value = as.double(zone_low_av_0vs40_res$p.value),
             Mean_diff = (zone_low_av_0vs40_res$estimate)) %>%
  mutate(
    rounded = abs(round(Mean_diff, 2)),
    Significant = case_when(P_value < 0.05 ~ "significant",
                            TRUE ~ "not significant"))
zone_low_av_0vs40_res
zone_low_av_0vs40_res_sig 

p_vlaue_text_zone_low <- paste0("Yield at P 40 is P 0 plus ", zone_low_av_0vs40_res_sig$rounded, " and is ", 
                                zone_low_av_0vs40_res_sig$Significant, "\n",
                             "Yield at P 40 is P 20 minus ", zone_low_av_20vs40_res_sig$rounded, " and is ", 
                             zone_low_av_20vs40_res_sig$Significant,  "\n",
                             "Yield at P 80 is P 40 plus ", zone_low_av_80vs40_res_sig$rounded, " and is ", 
                             zone_low_av_80vs40_res_sig$Significant, collapse = "\n")

 print(p_vlaue_text_zone_low)
 
 
 
 library(grid)
 Pvalue_on_graph <- grobTree(textGrob(p_vlaue_text_zone_low, x=0.1,  y=0.10, hjust=0,
                                      gp=gpar(col="black", fontsize=6, fontface="italic")))
 
 
 
 
 # Compute t-test - this is done for 0 vs 100 for all values in the strip
 zone_low_av
 zone_low <- ggplot( zone_low_av, aes(P_Rate_as_factor, YldMassDry))+
   geom_boxplot(alpha=0.1)+
   geom_point(colour = "blue", alpha = 0.1)+
   theme_bw()+
   ylim(0.0,5)+
   theme(axis.text=element_text(size=8),
         axis.title=element_text(size=10,))+
   labs(x = "P rate",
        y= "Yield t/ha",
        title = "Zone 1 - Low")+
   annotation_custom(Pvalue_on_graph)
 
 ##save the results of the zone strip work
  zone_low
 ggsave(path= graph_path, filename = "t-test_zone_low_strip.png", device = "png" ,
        width = 20, height = 10, units = "cm")
 write.csv(zone_low_av, paste0(graph_path,"/t_testzone_low_av.csv"))
 
 
 ###########################################################################################################################################
 ##### Paired t test for zone strip high ####
 
 ##average the yield values in each line segment - this ensure I have the same number of points
 str(seg_ID)
 zone_high <- filter(seg_ID, zone == "high" )
 zone_high_av <- group_by(zone_high,SegmentID, P_Rates ) %>% 
   summarise_all(mean)
 
 group_by(zone_high_av, P_Rates) %>% 
   summarise(mean(YldMassDry))
 
 zone_high_av$P_Rate_as_factor <- as.factor(zone_high_av$P_Rates)
 
 
 zone_high_av_20vs40 <- filter(zone_high_av, P_Rates == 20 | P_Rates== 40 )
 zone_high_av_80vs40 <- filter(zone_high_av, P_Rates == 80 | P_Rates== 40 )
 zone_high_av_0vs40 <- filter(zone_high_av, P_Rates == 0 | P_Rates== 40 )
 
 
 # http://www.sthda.com/english/wiki/paired-samples-t-test-in-r
 
 #test assumptions
 
 
 # compute the difference
 # d <- with(zone_av, 
 #           YLDMASSDR[P_Rate == 0] - YLDMASSDR[P_Rate == 50])
 # # Shapiro-Wilk normality test for the differences
 # shapiro.test(d) # => p-value = 0.5161
 
 # I think this means that it is  normally distrubuted
 
 
 zone_high_av_20vs40_res <- t.test(YldMassDry ~ P_Rates, data = zone_high_av_20vs40, paired = TRUE)
 zone_high_av_80vs40_res <- t.test(YldMassDry ~ P_Rates, data = zone_high_av_80vs40, paired = TRUE)
 zone_high_av_0vs40_res <- t.test(YldMassDry ~ P_Rates, data = zone_high_av_0vs40, paired = TRUE)
 

 #Report values from the t.test
 zone_high_av_20vs40_res_sig <-
   data.frame(P_value = as.double(zone_high_av_20vs40_res$p.value),
              Mean_diff = (zone_high_av_20vs40_res$estimate)) %>%
   mutate(
     rounded = abs(round(Mean_diff, 2)),
     Significant = case_when(P_value < 0.05 ~ "significant",
                             TRUE ~ "not significant"))

 zone_high_av_20vs40_res_sig
 
 zone_high_av_80vs40_res_sig <-
   data.frame(P_value = as.double(zone_high_av_80vs40_res$p.value),
              Mean_diff = (zone_high_av_80vs40_res$estimate)) %>%
   mutate(
     rounded = abs(round(Mean_diff, 2)),
     Significant = case_when(P_value < 0.05 ~ "significant",
                             TRUE ~ "not significant"))
 
 
 zone_high_av_0vs40_res_sig <-
   data.frame(P_value = as.double(zone_high_av_0vs40_res$p.value),
              Mean_diff = (zone_high_av_0vs40_res$estimate)) %>%
   mutate(
     rounded = abs(round(Mean_diff, 2)),
     Significant = case_when(P_value < 0.05 ~ "significant",
                             TRUE ~ "not significant"))
 
 zone_high_av_0vs40_res_sig
 p_vlaue_text_zone_high <- paste0("Yield at P 40 is P 0 minu ", zone_high_av_0vs40_res_sig$rounded, " and is ", 
                                 zone_high_av_0vs40_res_sig$Significant, "\n",
                                 "Yield at P 40 is P 20 minus ", zone_high_av_20vs40_res_sig$rounded, " and is ", 
                                 zone_high_av_20vs40_res_sig$Significant,  "\n",
                                 "Yield at P 80 is P 40 plus ", zone_high_av_80vs40_res_sig$rounded, " and is ", 
                                 zone_high_av_80vs40_res_sig$Significant, collapse = "\n")
 

 print(p_vlaue_text_zone_high)
 library(grid)
 Pvalue_on_graph_high <- grobTree(textGrob(p_vlaue_text_zone_high, x=0.1,  y=0.10, hjust=0,
                                      gp=gpar(col="black", fontsize=6, fontface="italic")))
 
 
 
 
 # Compute t-test - this is done for 0 vs 100 for all values in the strip
 zone_high_av
 zone_high <- ggplot( zone_high_av, aes(P_Rate_as_factor, YldMassDry))+
   geom_boxplot(alpha=0.1)+
   geom_point(colour = "blue", alpha = 0.1)+
   theme_bw()+
   ylim(0.0,5)+
   theme(axis.text=element_text(size=8),
         axis.title=element_text(size=10,))+
   labs(x = "P rate",
        y= "Yield t/ha",
        title = "Zone 2 - High")+
   annotation_custom(Pvalue_on_graph_high)
 
 ##save the results of the zone strip work
 zone_high
 ggsave(path= graph_path, filename = "t-test_zone_high_strip.png", device = "png" ,
        width = 20, height = 10, units = "cm")
 write.csv(zone_low_av, paste0(graph_path,"/t_testzone_high_av.csv"))
 
 
 
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
 Craig_Muir_Arthurs <- filter(harm_database,
                                   Paddock_tested == "Arthurs") %>% 
   dplyr::select(5: 11)


 Craig_Muir_Arthurs
 
 
  TSpecial <- ttheme_minimal(base_size = 8)
table1 <- tableGrob(Craig_Muir_Arthurs , rows = NULL, theme=TSpecial )
#table2 <- tableGrob(table_segments, rows = NULL, theme=TSpecial)

#get the name of the paddock...

paddock <- "Arthurs"
test <- textGrob(paddock)
 ####################################################################################################################################
 ## Arrange the outputs onto one page
segments
zone_low
zone_high
paddock

Craig_Muir_Arthurs
 collection <- grid.arrange(zone_low, zone_high, table1, segments, test,  nrow = 5, 
              layout_matrix = cbind(c(1,1,5,4,4), c(2,2,3,4,4)))
             
collection

ggsave(path= graph_path, filename = "collection.png", device = "png", 
       width = 21, height = 15, units = "cm", collection)
 
