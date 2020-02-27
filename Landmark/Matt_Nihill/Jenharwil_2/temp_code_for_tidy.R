
#################################################################################################
###This code will generate grower reports by:
#1.bring in the strip data for a site 
#2.run paired t-test, 
#3.create plots 
#4.Accesses lab results and generates reports

## Approach using polygon to pull out raw yield data

###############################################################################################
### load in the libraries

#install.packages("PairedData")
#install.packages("RGraphics")
#install.packages("gridExtra")

library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(PairedData)
library(cowplot)
library(grid)
library(RGraphics)
library(gridExtra)

###############################################################################################
##1a. Details about the site what it looks like in the database


Organisation	  = "Landmark"
Contact_Farmer  = "Matt Nihill   7"
Paddock_tested  =	"Jenharwil 2"
Zone            = "James 2"
data_file       = "James_Yld_Seg_ID_zone.csv"

Fert_legend_name <- "N Rates"

##1b. set path for getting my spatial data and location of saving outputs

name_of_path <-
  file.path(
    "W:",
    "value_soil_testing_prj",
    "Yield_data",
    Organisation,
    "Matt_Nihill",
    "Jenharwil_2",
    "James 2")
graph_path <-
  file.path(name_of_path)

seg_ID <- read_csv(paste0(name_of_path, "/", data_file))
names(seg_ID)
##1c. make name consistant

seg_ID <-
  rename(seg_ID, 
         "Rates" = "Rate", #new name = old name
         "Zone" =  "Zone",
         "Yld" = "Yld_Mass_D"
         )

##1c. Set up data so its generic growers rate, rate1, rate2, rate3, zone1, zone2

#Define the rates
unique(seg_ID$Rates)

Grower_rate = 80
rate1 = 0
rate2 = 150
#rate3 = 110

#Define the zones
unique(seg_ID$Zone)
zone1 <- "Low"
zone2 <- "High"


############################################################################################################################
### clean the data removing zero values
## remove all the values in the data set that won't be included in the analysis this is when distance on line = 0
seg_ID <- filter(seg_ID,
                 DistOnLine != 0)

#The farmer practice wasnt really a true strip but I want to use the data so I need to remove row when we have no yield
seg_ID <- filter(seg_ID,
                 Yld != 0)

#############################################################################################################################
##2. t test per segment in the strip Via Andrea method####

#Prep the data so I can check what am I testing (look at Harms list)

seg_ID_rate1vsGR <- filter(seg_ID, Rates == rate1 | Rates== Grower_rate )
seg_ID_rate2vsGR <- filter(seg_ID, Rates == rate2 | Rates== Grower_rate )
#seg_ID_rate3vsGR <- filter(seg_ID, P_Rates == rate3 | P_Rates== Grower_rate )

#I want a list of all values in segment ID to uss in the loop
list <- unique(seg_ID_rate1vsGR$SegmentID)

############################################################################################################
##2a. Run as a loop for test 1 rate 1 vs GR

Output_rate1vsGR= data.frame() #create empty df for output
for (i in list){
  segment_data = subset(seg_ID_rate1vsGR, SegmentID == i)
  
  # Method 1: The data are saved in two different numeric vectors.
  data_x=subset(segment_data, Rates==Grower_rate, select = Yld, drop = TRUE)
  data_y=subset(segment_data, Rates==rate1, select = Yld, drop = TRUE)
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
                                          SegmentID, Zone, Rates ) %>% 
  summarise_all(mean) %>% 
  ungroup()
#join output to summary

#what comparison did I run? - name the df to reflect this
seg_ID_rate1vsGR_summary <- left_join(seg_ID_rate1vsGR_summary, Output_rate1vsGR)
seg_ID_rate1vsGR_summary <- mutate(seg_ID_rate1vsGR_summary, comparison = "rate1vsGR" )

seg_ID_rate1vsGR_summary
#####################################################################################################
##2b.Run as a loop for  test 2 rate 2 vs GR
Output_rate2vsGR= data.frame() #create empty df for output
for (i in list){
  segment_data = subset(seg_ID_rate2vsGR, SegmentID == i)
  
  # Method 1: The data are saved in two different numeric vectors.
  data_x=subset(segment_data, Rates==rate2, select = Yld, drop = TRUE)
  data_y=subset(segment_data, Rates==Grower_rate, select = Yld, drop = TRUE)
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
                                 SegmentID, Zone, Rates ) %>% 
  summarise_all(mean) %>% 
  ungroup()
#join output to summary
seg_ID_rate2vsGR_summary <- left_join(seg_ID_rate2vsGR_summary, Output_rate2vsGR)
#what comparison did I run? - name the df to reflect this
seg_ID_rate2vsGR_summary <- mutate(seg_ID_rate2vsGR_summary, comparison = "rate2vsGR" )


#####################################################################################################
##2c.Run as a loop for  test 3 rate 3 vs GR
Output_rate3vsGR= data.frame() #create empty df for output
for (i in list){
  segment_data = subset(seg_ID_rate3vsGR, SegmentID == i)
  
  # Method 1: The data are saved in two different numeric vectors.
  data_x=subset(segment_data, Rates==rate3, select = Yld, drop = TRUE)
  data_y=subset(segment_data, Rates==Grower_rate, select = Yld, drop = TRUE)
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
                                     SegmentID, Zone, Rates ) %>% 
  summarise_all(mean) %>% 
  ungroup()
#join output to summary
seg_ID_rate3vsGR_summary <- left_join(seg_ID_rate3vsGR_summary, Output_rate3vsGR)
#what comparison did I run? - name the df to reflect this
seg_ID_rate3vsGR_summary <- mutate(seg_ID_rate3vsGR_summary, comparison = "rate3vsGR" )


###############################################################################################################
##2d. Join the two strip data results togther join info from test 1 to test 2 and test 3
head(seg_ID_rate1vsGR_summary)
head(seg_ID_rate2vsGR_summary)
#head(seg_ID_rate3vsGR_summary)

seg_ID_t_test_summary <- rbind(seg_ID_rate1vsGR_summary, seg_ID_rate2vsGR_summary)
#seg_ID_t_test_summary <- rbind(seg_ID_rate1vsGR_summary, seg_ID_rate2vsGR_summary, seg_ID_rate3vsGR_summary)

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
##3a.    plot results  of t.test ########################################################################

seg_ID_t_test_summary$P_Rate_as_factor <- as.factor(seg_ID_t_test_summary$Rates)


##3aa - define some parameters for the graph - set the zone bands on the graph.
#Zone1
zone1_min <- filter(seg_ID_t_test_summary, Zone == zone1) %>% 
  summarise(min_zone = min(SegmentID))
zone1_min <- zone1_min[[1]]
zone1_max <- filter(seg_ID_t_test_summary, Zone == zone1) %>% 
  summarise(max_zone = max(SegmentID))
zone1_max <- zone1_max[[1]]

#Zone2
zone2_min <- filter(seg_ID_t_test_summary, Zone == zone2) %>% 
  summarise(min_zone = min(SegmentID))
zone2_min <- zone2_min[[1]]
zone2_max <- filter(seg_ID_t_test_summary, Zone == zone2) %>% 
  summarise(max_zone = max(SegmentID))
zone2_max <- zone2_max[[1]]


##3b. Plot the results 
segments <- ggplot(seg_ID_t_test_summary, aes(SegmentID , Yld, group = P_Rate_as_factor))+
  geom_line(size=1, alpha=0.4, aes( color = P_Rate_as_factor ))+
  scale_color_manual(values=c('darkgrey','green', 'blue', 'red'), name  = Fert_legend_name)+
  theme_bw()+
  ylim(0.0,6)+
  labs(x= "Distance along the strip",
       y = "Yield t/ha",
       title = "",
       subtitle = "",
       caption = "")+
   annotate("rect", xmin = zone1_min, xmax = zone1_max, ymin = 0, ymax = 6, #Zone 1
           alpha = .2) +
  annotate("text", x = 85, y= 1,label = zone1)+
  
   annotate("rect", xmin =zone2_min , xmax = zone2_max, ymin = 0, ymax = 6, #zone 2
            alpha = .2)+
  annotate("text", x = 57, y= 1,label = zone2)
#+
 # annotate("text", x = 40, y= 1,label = "Missing data")


##3c. Save the results of the segment work
segments #this is the graph
ggsave(path= graph_path, filename = "t-test_segments.png", device = "png" ,
       width = 20, height = 10, units = "cm")

write.csv(seg_ID_t_test_summary, paste0(graph_path,"/t_test_segments.csv"))



################################################################################################################################### 

##4a.  Paired t test for zone strip Zone 1 ####
 
##average the yield values in each line segment - this ensure I have the same number of points
# filter out data so we just have zone 1
zone_1 <- filter(seg_ID, Zone == zone1 )
zone_av_1 <- group_by(zone_1,SegmentID, Rates ) %>% 
   summarise_all(mean)
 
 zone_av_1
 unique(zone_1$zone)
 
 group_by(zone_av_1, Rates) %>% 
  summarise(mean(Yld))
 
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

