
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
#install.packages("rdrop2")


library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(PairedData)
library(cowplot)
library(grid)
library(RGraphics)
library(gridExtra)
library(rdrop2) 

###############################################################################################
##1a. Details about the site what it looks like in the database

database_name_of_path <-
  file.path(
    "W:",
    "value_soil_testing_prj",
    "data_base")

	   	
"W:\value_soil_testing_prj\Yield_data\Vic_Ind\Anne_Jackman\Nelson"
#As it appears in the database
Organisation_db	  = "Vic_Ind"
Contact_db = "Anne_Jackman"
Farmer_db  =  "Nelson"
#Paddock_tested_db  =	"Brennans"
#Zone_db            = 
data_file       = "Nelson_Yld_Seg_ID_zone.csv"

Fert_legend_name <- "P Rates"


#finished work with results
finished_name_of_path <-
  file.path(
    "W:",
    "value_soil_testing_prj",
    "Yield_data",
    "finished")

#As it appears in the file directory
site_details <- paste0(Organisation_db,"_",
                       "Anne_Jackman","_",
                       "Nelson")

                       

##1b. set path for getting my spatial data and location of saving outputs

name_of_path <-
  file.path(
    "W:",
    "value_soil_testing_prj",
    "Yield_data",
    Organisation_db,
    "Anne_Jackman",
    "Nelson"
    )
graph_path <-
  file.path(name_of_path)

seg_ID <- read_csv(paste0(name_of_path, "/", data_file))
names(seg_ID)
##1c. make name consistant

# P rates first
unique(seg_ID$MESZ_Urea)
seg_ID <- filter(seg_ID,
               MESZ_Urea == "50_MESZ_25_Urea"|
                 MESZ_Urea == "25_MESZ_25_Urea" |
                 MESZ_Urea == "75_MESZ_25_Urea"|
                 MESZ_Urea == "0_MESZ_25_Urea")

seg_ID <-
  rename(seg_ID, 
         "Rates" = "Rates", #new name = old name
         "Zone" =  "zones",
         "Yld" = "YldMassDry"
         )

##1c. Set up data so its generic growers rate, rate1, rate2, rate3, zone1, zone2

#Define the rates
unique(seg_ID$Rates)


Grower_rate = 50
rate1 = 0
rate2 = 25
rate3 = 75

list_rates <- data.frame( rate_name = c("Grower_rate" , "rate1",  "rate2",  "rate3"),
                          Rates = c(Grower_rate,rate1, rate2, rate3 ) )
  
  
 
list_rates
#Define the zones
unique(seg_ID$Zone)
zone1 <- "low"
zone2 <- "high"


############################################################################################################################
### clean the data removing zero values
## remove all the values in the data set that won't be included in the analysis this is when distance on line = 0
seg_ID <- filter(seg_ID,
                 DistOnLine != 0)

#The farmer practice wasnt really a true strip but I want to use the data so I need to remove row when we have no yield
seg_ID <- filter(seg_ID,
                 Yld != 0)
#heaps of missing data in the ploygon vectors min is 10 max is 212


#test <- dplyr::filter(seg_ID, between(seg_ID$SegmentID, 1, 2))
seg_ID <- filter(seg_ID,
                 SegmentID != 107&
                 SegmentID != 1&
                 SegmentID != 2)
str(seg_ID)

#############################################################################################################################
##2. t test per segment in the strip Via Andrea method####

#Prep the data so I can check what am I testing (look at Harms list)

seg_ID_rate1vsGR <- filter(seg_ID, Rates == rate1 | Rates== Grower_rate )
seg_ID_rate2vsGR <- filter(seg_ID, Rates == rate2 | Rates== Grower_rate )
seg_ID_rate3vsGR <- filter(seg_ID, Rates == rate3 | Rates== Grower_rate )

#I want a list of all values in segment ID to uss in the loop
list <- unique(seg_ID_rate1vsGR$SegmentID)
list 
############################################################################################################
##2a. Run as a loop for test 1 rate 1 vs GR



Output_rate1vsGR= data.frame() #create empty df for output
for (i in list){
  segment_data = subset(seg_ID_rate1vsGR, SegmentID == i)
  
  # Method 1: The data are saved in two different numeric vectors.
  data_x=subset(segment_data, Rates == Grower_rate, select = Yld, drop = TRUE)
  data_y=subset(segment_data, Rates == rate1, select = Yld, drop = TRUE)
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
head(seg_ID_rate3vsGR_summary)

#seg_ID_t_test_summary <- rbind(seg_ID_rate1vsGR_summary, seg_ID_rate2vsGR_summary)
seg_ID_t_test_summary <- rbind(seg_ID_rate1vsGR_summary, seg_ID_rate2vsGR_summary, seg_ID_rate3vsGR_summary)

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

zone1_range <- ((zone1_max - zone1_min)/2)+zone1_min
zone1_range

  #Zone2
zone2_min <- filter(seg_ID_t_test_summary, Zone == zone2) %>% 
  summarise(min_zone = min(SegmentID))
zone2_min <- zone2_min[[1]]
zone2_max <- filter(seg_ID_t_test_summary, Zone == zone2) %>% 
  summarise(max_zone = max(SegmentID))
zone2_max <- zone2_max[[1]]

zone2_range <- ((zone2_max - zone2_min)/2)+zone2_min
zone2_range

##3b. Plot the results 
seg_ID_t_test_summary
segments <- ggplot(seg_ID_t_test_summary, aes(SegmentID , Yld, group = P_Rate_as_factor))+
  geom_line(size=1, alpha=0.4, aes( color = P_Rate_as_factor ))+
  scale_color_manual(values=c('darkgrey','green', 'blue', 'red'), name  = Fert_legend_name)+
  theme_bw()+
  ylim(0.0,3)+
  labs(x= "Distance along the strip",
       y = "Yield t/ha",
       title = "",
       subtitle = "",
       caption = "")+
   annotate("rect", xmin = zone1_min, xmax = zone1_max, ymin = 0, ymax = 3, #Zone 1
           alpha = .2) +
  annotate("text", x = zone1_range, y= 1,label = zone1)+
  
   annotate("rect", xmin =zone2_min , xmax = zone2_max, ymin = 0, ymax = 3, #zone 2
            alpha = .2)+
  annotate("text", x = zone2_range, y= 1,label = zone2)
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

#subset the zone 1 data
zone_av_1_rate1vsGR <- filter(zone_av_1, Rates == rate1 | Rates== Grower_rate )
zone_av_1_rate2vsGR <- filter(zone_av_1, Rates == rate2 | Rates== Grower_rate )
zone_av_1_rate3vsGR <- filter(zone_av_1, Rates == rate3 | Rates== Grower_rate )
 
#ensure that the dataset is duplictaed
list_SegmentID_values <- zone_av_1_rate1vsGR$SegmentID[duplicated(zone_av_1_rate1vsGR$SegmentID)] #this returns a list of values I want to keep
zone_av_1_rate1vsGR <- zone_av_1_rate1vsGR %>% filter(SegmentID %in% list_SegmentID_values)

list_SegmentID_values <- zone_av_1_rate2vsGR$SegmentID[duplicated(zone_av_1_rate2vsGR$SegmentID)] #this returns a list of values I want to keep
zone_av_1_rate2vsGR <- zone_av_1_rate2vsGR %>% filter(SegmentID %in% list_SegmentID_values)

list_SegmentID_values <- zone_av_1_rate3vsGR$SegmentID[duplicated(zone_av_1_rate3vsGR$SegmentID)] #this returns a list of values I want to keep
zone_av_1_rate3vsGR <- zone_av_1_rate3vsGR %>% filter(SegmentID %in% list_SegmentID_values)

#run the paired t test
zone_av_1_rate1vsGR_res <- t.test(Yld ~ Rates, data = zone_av_1_rate1vsGR, paired = TRUE)
zone_av_1_rate2vsGR_res <- t.test(Yld ~ Rates, data = zone_av_1_rate2vsGR, paired = TRUE)
zone_av_1_rate3vsGR_res <- t.test(Yld ~ Rates, data = zone_av_1_rate3vsGR, paired = TRUE)

#####test 1 results
# Report values from the t.test
zone_av_1_rate1vsGR_res_sig <-
   data.frame(P_value = as.double(zone_av_1_rate1vsGR_res$p.value),
              Mean_diff = (zone_av_1_rate1vsGR_res$estimate)) %>%
   mutate(
     rate_name = "rate1",
     rounded = abs(round(Mean_diff, 2)),
     Significant = case_when(P_value < 0.05 ~ "significant",
                        TRUE ~ "not significant"))
zone_av_1_rate1vsGR_res_sig 
 
####test 2 results
# Report values from the t.test
zone_av_1_rate2vsGR_res_sig <-
  data.frame(P_value = as.double(zone_av_1_rate2vsGR_res$p.value),
             Mean_diff = (zone_av_1_rate2vsGR_res$estimate)) %>%
  mutate(
    rate_name = "rate2",
    rounded = abs(round(Mean_diff, 2)),
    Significant = case_when(P_value < 0.05 ~ "significant",
                            TRUE ~ "not significant"))

####test 3 results
# Report values from the t.test
 zone_av_1_rate3vsGR_res
 #Report values from the t.test
 zone_av_1_rate3vsGR_res_sig <-
   data.frame(P_value = as.double(zone_av_1_rate3vsGR_res$p.value),
              Mean_diff = (zone_av_1_rate3vsGR_res$estimate)) %>%
   mutate(
     rate_name = "rate3",
     rounded = abs(round(Mean_diff, 2)),
     Significant = case_when(P_value < 0.05 ~ "significant",
                             TRUE ~ "not significant"))

zone_av_1_rate1vsGR_res_sig 
zone_av_1_rate2vsGR_res_sig
zone_av_1_rate3vsGR_res_sig

# positive_negative_rate1_GRS <- 
mean_zone_av_1 <-  group_by(zone_av_1, Rates) %>% 
  summarise(mean(Yld))
mean_zone_av_1
positive_neg_value_GR_rate1_zone1 <- ifelse(filter(mean_zone_av_1, Rates == Grower_rate) 
                                            - filter(mean_zone_av_1, Rates == rate1)>0, "plus", "minus") 
positive_neg_value_GR_rate1_zone1 <- positive_neg_value_GR_rate1_zone1[1,2]

positive_neg_value_rate2_GR_zone1 <- ifelse(filter(mean_zone_av_1, Rates == Grower_rate) 
                                            - filter(mean_zone_av_1, Rates == rate2)>0, "plus", "minus")
positive_neg_value_rate2_GR_zone1 <- positive_neg_value_rate2_GR_zone1[1,2]

 positive_neg_value_rate3_GR_zone1 <- ifelse(filter(mean_zone_av_1, Rates == rate3) 
                                             - filter(mean_zone_av_1, Rates == Grower_rate)>0, "plus", "minus")
 positive_neg_value_rate3_GR_zone1 <- positive_neg_value_rate3_GR_zone1[1,2]



p_vlaue_text_zone_1 <- paste0("Yield at P ", Grower_rate, " is  P ", rate1, " " ,positive_neg_value_GR_rate1_zone1, " ", 
                              zone_av_1_rate1vsGR_res_sig$rounded, " and is ", 
                              zone_av_1_rate1vsGR_res_sig$Significant, "\n",
                              
                              "Yield at P ", Grower_rate, " is  P ", rate2, " " ,positive_neg_value_rate2_GR_zone1, " ", 
                              zone_av_1_rate2vsGR_res_sig$rounded, " and is ", 
                              zone_av_1_rate2vsGR_res_sig$Significant, "\n",
                              
                               "Yield at P ", rate3, " is  P ", Grower_rate , " " ,positive_neg_value_rate3_GR_zone1, " ", 
                               zone_av_1_rate3vsGR_res_sig$rounded, " and is ", 
                               zone_av_1_rate3vsGR_res_sig$Significant, collapse = "\n")
print(p_vlaue_text_zone_1)
 
 library(grid)
 Pvalue_on_graph <- grobTree(textGrob(p_vlaue_text_zone_1, x=0.1,  y=0.10, hjust=0,
                                      gp=gpar(col="black", fontsize=6, fontface="italic")))
 # Plot the results
 zone_av_1
 zone_av_1$Rate_as_factor  <- as.factor(zone_av_1$Rates)
 zone_1 <- ggplot( zone_av_1, aes(Rate_as_factor, Yld))+
   geom_boxplot(alpha=0.1)+
   geom_point(colour = "blue", alpha = 0.1)+
   stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                width = .75, linetype = "dashed")+
   theme_bw()+
   ylim(0,3)+
   theme(axis.text=element_text(size=8),
         axis.title=element_text(size=10,))+
   labs(x = Fert_legend_name,
        y= "Yield t/ha",
        title = zone1)+
   annotation_custom(Pvalue_on_graph)
 zone_1
 
 ##save the graphs of the zone strip work
 ggsave(path= graph_path, filename = "t-test_zone_zone1_strip.png", device = "png" ,
        width = 20, height = 10, units = "cm")
 
 #make a table of the mean yield for zones with t test reuslts
 zone_av_1
 mean_zone_av_1 <-  group_by(zone_av_1, Rates) %>% 
   summarise(mean(Yld))
 mean_zone_av_1 <- left_join(mean_zone_av_1,list_rates)
 
 mean_zone_av_1and_res_sig <-  rbind(zone_av_1_rate1vsGR_res_sig, zone_av_1_rate2vsGR_res_sig)
 mean_zone_av_1 <- left_join(mean_zone_av_1,mean_zone_av_1and_res_sig)
 mean_zone_av_1 <- mutate(mean_zone_av_1, 
                          Zone = zone1,
                          Organisation =Organisation_db,
                          Contact = Contact_db,
                          Farmer = Farmer_db)
 #, Paddock_tested = Paddock_tested_db)
 names(mean_zone_av_1)[2] <- "Yld"
 mean_zone_av_1
 
 write.csv(zone_av_1, paste0(graph_path,"/t_testzone_zone1_av.csv"))
 
 
 
 

 
 ###########################################################################################################################################
 ##4b.  Paired t test for zone strip Zone 2 ####
 
 ##average the yield values in each line segment - this ensure I have the same number of points
 # filter out data so we just have zone 2
 zone_2 <- filter(seg_ID, Zone == zone2 )
 zone_2
 zone_av_2 <- group_by(zone_2,SegmentID, Rates ) %>% 
   summarise_all(mean)
 
 #subset the zone 1 data
 zone_av_2_rate1vsGR <- filter(zone_av_2, Rates == rate1 | Rates== Grower_rate )
 zone_av_2_rate2vsGR <- filter(zone_av_2, Rates == rate2 | Rates== Grower_rate )
 zone_av_2_rate3vsGR <- filter(zone_av_2, Rates == rate3 | Rates== Grower_rate )
 
 #ensure that the dataset is duplictaed
 list_SegmentID_values <- zone_av_2_rate1vsGR$SegmentID[duplicated(zone_av_2_rate1vsGR$SegmentID)] #this returns a list of values I want to keep
 zone_av_2_rate1vsGR <- zone_av_2_rate1vsGR %>% filter(SegmentID %in% list_SegmentID_values)
 
 list_SegmentID_values <- zone_av_2_rate2vsGR$SegmentID[duplicated(zone_av_2_rate2vsGR$SegmentID)] #this returns a list of values I want to keep
 zone_av_2_rate2vsGR <- zone_av_2_rate2vsGR %>% filter(SegmentID %in% list_SegmentID_values)
 
 list_SegmentID_values <- zone_av_2_rate3vsGR$SegmentID[duplicated(zone_av_2_rate3vsGR$SegmentID)] #this returns a list of values I want to keep
 zone_av_2_rate3vsGR <- zone_av_2_rate3vsGR %>% filter(SegmentID %in% list_SegmentID_values)
 
 #run the paired t test
 zone_av_2_rate1vsGR_res <- t.test(Yld ~ Rates, data = zone_av_2_rate1vsGR, paired = TRUE)
 zone_av_2_rate2vsGR_res <- t.test(Yld ~ Rates, data = zone_av_2_rate2vsGR, paired = TRUE)
 zone_av_2_rate3vsGR_res <- t.test(Yld ~ Rates, data = zone_av_2_rate3vsGR, paired = TRUE)
 
 #####test 1 results
 # Report values from the t.test
 zone_av_2_rate1vsGR_res_sig <-
   data.frame(P_value = as.double(zone_av_2_rate1vsGR_res$p.value),
              Mean_diff = (zone_av_2_rate1vsGR_res$estimate)) %>%
   mutate(
     rate_name = "rate1",
     rounded = abs(round(Mean_diff, 2)),
     Significant = case_when(P_value < 0.05 ~ "significant",
                             TRUE ~ "not significant"))
 zone_av_2_rate1vsGR_res_sig 
 
 ####test 2 results
 # Report values from the t.test
 zone_av_2_rate2vsGR_res_sig <-
   data.frame(P_value = as.double(zone_av_2_rate2vsGR_res$p.value),
              Mean_diff = (zone_av_2_rate2vsGR_res$estimate)) %>%
   mutate(
     rate_name = "rate2",
     rounded = abs(round(Mean_diff, 2)),
     Significant = case_when(P_value < 0.05 ~ "significant",
                             TRUE ~ "not significant"))
 
 ####test 3 results
 # Report values from the t.test
  zone_av_2_rate3vsGR_res
 # #Report values from the t.test
  zone_av_2_rate3vsGR_res_sig <-
    data.frame(P_value = as.double(zone_av_2_rate3vsGR_res$p.value),
               Mean_diff = (zone_av_2_rate3vsGR_res$estimate)) %>%
    mutate(
      rate_name = "rate3",
      rounded = abs(round(Mean_diff, 2)),
      Significant = case_when(P_value < 0.05 ~ "significant",
                             TRUE ~ "not significant"))
 
 zone_av_2_rate1vsGR_res_sig 
 zone_av_2_rate2vsGR_res_sig
 zone_av_2_rate3vsGR_res_sig
 
 # positive_negative_rate1_GRS <- 
 mean_zone_av_2 <-  group_by(zone_av_2, Rates) %>% 
   summarise(mean(Yld))
 mean_zone_av_2
 positive_neg_value_GR_rate1_zone2 <- ifelse(filter(mean_zone_av_2, Rates == Grower_rate) 
                                             - filter(mean_zone_av_2, Rates == rate1)>0, "plus", "minus") 
 positive_neg_value_GR_rate1_zone2 <- positive_neg_value_GR_rate1_zone2[1,2]
 
 positive_neg_value_rate2_GR_zone2 <- ifelse(filter(mean_zone_av_2, Rates == Grower_rate) 
                                             - filter(mean_zone_av_2, Rates == rate2)>0, "plus", "minus")
 positive_neg_value_rate2_GR_zone2 <- positive_neg_value_rate2_GR_zone2[1,2]
 
 positive_neg_value_rate3_GR_zone2 <- ifelse(filter(mean_zone_av_2, Rates == rate3) 
                                              - filter(mean_zone_av_2, Rates == Grower_rate)>0, "plus", "minus")
 positive_neg_value_rate3_GR_zone2 <- positive_neg_value_rate3_GR_zone2[1,2]
 
 
 
 p_vlaue_text_zone_2 <- paste0("Yield at P ", Grower_rate, " is  P ", rate1, " " ,positive_neg_value_GR_rate1_zone2, " ", 
                               zone_av_2_rate1vsGR_res_sig$rounded, " and is ", 
                               zone_av_2_rate1vsGR_res_sig$Significant, "\n",
                               
                               "Yield at P ", Grower_rate, " is  P ", rate2, " " ,positive_neg_value_rate2_GR_zone2, " ", 
                               zone_av_2_rate2vsGR_res_sig$rounded, " and is ", 
                               zone_av_2_rate2vsGR_res_sig$Significant, "\n",
                               
                               "Yield at P ", rate3, " is  P ", Grower_rate, " " ,positive_neg_value_rate3_GR_zone2, " ", 
                               zone_av_2_rate3vsGR_res_sig$rounded, " and is ", 
                               zone_av_2_rate3vsGR_res_sig$Significant, collapse = "\n")
 
 
 print(p_vlaue_text_zone_2)
 
 library(grid)
 Pvalue_on_graph <- grobTree(textGrob(p_vlaue_text_zone_2, x=0.1,  y=0.10, hjust=0,
                                      gp=gpar(col="black", fontsize=6, fontface="italic")))
 # Plot the results
 zone_av_2
 zone_av_2$Rate_as_factor  <- as.factor(zone_av_2$Rates)
 zone_2 <- ggplot( zone_av_2, aes(Rate_as_factor, Yld))+
   geom_boxplot(alpha=0.1)+
   geom_point(colour = "blue", alpha = 0.1)+
   stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                width = .75, linetype = "dashed")+
   theme_bw()+
   ylim(0,3)+
   theme(axis.text=element_text(size=8),
         axis.title=element_text(size=10,))+
   labs(x = Fert_legend_name,
        y= "Yield t/ha",
        title = zone2)+
   annotation_custom(Pvalue_on_graph)
 zone_2
 
 ##save the graphs of the zone strip work
 ggsave(path= graph_path, filename = "t-test_zone_zone2_strip.png", device = "png" ,
        width = 20, height = 10, units = "cm")
 
 #make a table of the mean yield for zones with t test reuslts
 zone_av_2
 mean_zone_av_2 <-  group_by(zone_av_2, Rates) %>% 
   summarise(mean(Yld))
 mean_zone_av_2 <- left_join(mean_zone_av_2,list_rates)
 
 mean_zone_av_2and_res_sig <-  rbind(zone_av_2_rate1vsGR_res_sig, zone_av_2_rate2vsGR_res_sig)
 mean_zone_av_2 <- left_join(mean_zone_av_2,mean_zone_av_2and_res_sig)
 mean_zone_av_2 <- mutate(mean_zone_av_2, 
                          Zone = zone2,
                          Organisation =Organisation_db,
                          Contact = Contact_db,
                          Farmer = Farmer_db)
 #,                          Paddock_tested = Paddock_tested_db)
 
 
 
 
 names(mean_zone_av_2)[2] <- "Yld"
 mean_zone_av_2
 write.csv(zone_av_2, paste0(graph_path,"/t_testzone_zone2_av.csv"))
 
 
 
 
 
 
 
#####################################################################################################################################
### should get this from harms database
 #bring in data from the most current database from dropbox
 
 #set up access to dropbox when is is password protected
 token<-drop_auth()
 saveRDS(token, "droptoken.rds")
 
 token<-readRDS("droptoken.rds")
 drop_acc(dtoken=token)
 
 #https://www.dropbox.com/home/GRDC_Soil_Plant_Testing_Database
 #download the database file from dropbox and save it to mircolab
 drop_download(path = "GRDC_Soil_Plant_Testing_Database/NP_database_28022020.xlsx", 
               local_path = database_name_of_path,
               dtoken = token,
               overwrite = TRUE)
 
 #bring in the excel sheet as a r object
 database_name_of_path
 
harm_database <- read_excel(paste0(
                            database_name_of_path,"/", "NP_database_28022020.xlsx"),
                            sheet = "2019 full data", range = cell_cols("A:O"))


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
 site <- filter(harm_database,
                Farmer == Farmer_db) %>% 
   dplyr::select(5, 6: 11)

 
 site
 Farmer_db


#make a table of the mean yield for zones
mean_zone_av_1
mean_zone_av_2
 
mean_zone_av_1_2 <- as.data.frame( rbind(mean_zone_av_1, mean_zone_av_2))

mean_zone_av_1_2
write.csv(mean_zone_av_1_2, paste0(finished_name_of_path,
                                 "/",
                                 site_details,
                                 "_",
                                 zone2,
                                 "mean_zone_av_1_2.csv"))



mean_zone_av_1_2_display <- dplyr::select(mean_zone_av_1_2,
                                   Rates, 
                                   Yld, 
                                   Zone)
mean_zone_av_1_2_display
mean_zone_av_1_2_display <- spread(mean_zone_av_1_2_display, Zone, Yld)
mean_zone_av_1_2_display <- round(mean_zone_av_1_2_display,2)



TSpecial <- ttheme_minimal(base_size = 8)
table1 <- tableGrob(site , rows = NULL, theme=TSpecial )
table2 <- tableGrob(mean_zone_av_1_2_display, rows = NULL, theme=TSpecial)

#get the name of the paddock...

paddock <- Farmer_db


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

