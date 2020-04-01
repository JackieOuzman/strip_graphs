
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
libs <- c("dplyr", "tidyr", 
          "ggplot2", "readxl",
          "PairedData", "cowplot", "grid", 
          "RGraphics", 
          "gridExtra", "rdrop2", "readr")

install.libraries <- function(lib=NULL){
  new <- lib[!(lib %in% installed.packages()[, "Package"])]
  if (length(new)){   
    install.packages(new, dependencies = TRUE)
  }
} 

load.libraries <- function(lib=NULL){
  sapply(libs, require, character.only = TRUE)
}

install.libraries(libs)
load.libraries(libs)
#########################################################

###############################################################################################
#------------USER INPUTS ------------------#
##1a. Details about the site what it looks like in the database
"W:\value_soil_testing_prj\Yield_data\MSF\Michael_Moodie_Todd_McDonald\Anderson\Dave Dows\processing\Dave_Yld_Seg_ID_zone.csv"

#As it appears in the database
Organisation_db	  = "MSF"
Contact_db = "Michael_Moodie_Todd_McDonald"
Farmer_db  =  "Anderson"
Paddock_tested_db  =	"Dave Dows"
#Zone_db            = 
data_file       = "Dave_Yld_Seg_ID_zone.csv"

#As it appears in the file directory I use it for name of files
site_details <- paste0(Organisation_db,"_",
                       Contact_db,"_",
                       Farmer_db, "_",
                       Paddock_tested_db)
site_details
##1b. set path for getting my spatial data and location of saving outputs
name_of_path <-
  file.path(
    "W:",
    "value_soil_testing_prj",
    "Yield_data",
    Organisation_db,
    Contact_db,
    Farmer_db,
    Paddock_tested_db
  )
name_of_path

#what are we testing - what fert ws implemented?
Fert_legend_name <- "P Rates"

#################################################################################################
#These file loaction won't change from site to site
#location of the database
database_name_of_path <-
  file.path(
    "W:",
    "value_soil_testing_prj",
    "data_base")
#finished work with results
finished_name_of_path <-
  file.path(
    "W:",
    "value_soil_testing_prj",
    "Yield_data",
    "finished")

graph_path <-
 paste0( file.path(name_of_path), "/", "output")

###########################################################################################################
##Read in the file from the spatial work

seg_ID <- read_csv(paste0(name_of_path, "/", "processing",  "/", data_file))
names(seg_ID)

##1c. make name consistant

seg_ID <-seg_ID %>% 
  rename_at(vars(contains("ate")), ~"Rates") %>% 
  rename_at(vars(contains("one")), ~"Zone") %>% 
  rename_at(vars(contains("ield")), ~"Yld") 
       

### clean the data removing zero values
seg_ID <- filter(seg_ID,
                 DistOnLine != 0)


seg_ID <- filter(seg_ID,
                 Yld != 0)
### Add correction to segmentID so we can read it as meters (this will depend on how it was defined in spatial)
seg_ID <-  mutate(seg_ID,
                  SegmentID = SegmentID *10)


#######################################################################################################
#------------USER INPUTS ------------------#
##1c. Set up data so its generic growers rate, rate1, rate2, rate3, zone1, zone2

#Define the rates
unique(seg_ID$Rates)
str(seg_ID)

# change any non numbric values 
seg_ID <- seg_ID %>% 
  mutate(Rates = case_when(Rates == "Nil" ~ "0",
         TRUE ~ Rates ))
seg_ID$Rates <- as.numeric(seg_ID$Rates)  

##### Rate need to check this with database and work out which is grower_rate
Grower_rate = 80
rate1 = 0
rate2 = 40
rate3 = 120

#### 1 rate
#list_rates <- data.frame( rate_name = c("Grower_rate" , "rate1"),Rates = c(Grower_rate,rate1 ) )
#### 2 rate
#list_rates <- data.frame( rate_name = c("Grower_rate" , "rate1",  "rate2"), Rates = c(Grower_rate,rate1, rate2 ) )
#### 3 rate
list_rates <- data.frame( rate_name = c("Grower_rate" , "rate1",  "rate2",  "rate3"),Rates = c(Grower_rate,rate1, rate2, rate3 ) )  

##### Zones need to check this with 

#Define the zones
unique(seg_ID$Zone)
zone1 <- "Calcareus"
zone2 <- "Loam"
zone3 <- "Sand"


############################################################################################################################
#------------USER INPUTS yield data check------------------#
## Do I need to remove any segments with no yield data?

#seg_ID <- dplyr::filter(seg_ID, !between(seg_ID$SegmentID, 88, 89))

## yield data in t/ha or kg/ha
head(seg_ID)
seg_ID <-  mutate(seg_ID,
                  Yld = Yld /1000)

#############################################################################################################################
##2. t test per segment in the strip Via Andrea method####


#Prep the data 
seg_ID_rate1vsGR <- filter(seg_ID, Rates == rate1 | Rates== Grower_rate )
seg_ID_rate2vsGR <- filter(seg_ID, Rates == rate2 | Rates== Grower_rate )
seg_ID_rate3vsGR <- filter(seg_ID, Rates == rate3 | Rates== Grower_rate )

#I want a list of all values in segment ID to uss in the loop
list <- unique(seg_ID_rate1vsGR$SegmentID)
 
############################################################################################################
##2a. Run as a loop for rate 1 vs GR

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
##2b.Run as a loop for rate 2 vs GR
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
##2c.Run as a loop for   rate 3 vs GR
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
#------------USER INPUTS choose how to merge data------------------#
##2d. Join the two strip data results togther join info from test 1 to test 2 and test 3
head(seg_ID_rate1vsGR_summary)
head(seg_ID_rate2vsGR_summary)
head(seg_ID_rate3vsGR_summary)

### 1 Rate 
#seg_ID_t_test_summary <- seg_ID_rate1vsGR_summary
### 2 Rates 
#seg_ID_t_test_summary <- rbind(seg_ID_rate1vsGR_summary, seg_ID_rate2vsGR_summary)
### 3 Rates 
seg_ID_t_test_summary <- rbind(seg_ID_rate1vsGR_summary, seg_ID_rate2vsGR_summary, seg_ID_rate3vsGR_summary)

###remove some of the data from my workspace
rm(list = ls()[!(
  ls() %in% c(
    "Contact_db",
    "data_file",
    "database_name_of_path",
    "Farmer_db",
    "Fert_legend_name",
    "finished_name_of_path",
    "graph_path" ,
    "Grower_rate",
    "i",
    "install.libraries" ,
    "libs"  ,
    "list" ,
    "list_rates" ,
    "load.libraries",
    "name_of_path",
    "Organisation_db",
    "Paddock_tested_db",
    "rate1",
    "rate2",
    "rate3",
    "seg_ID",
    "seg_ID_rate1vsGR" ,
    "seg_ID_rate1vsGR_summary",
    "seg_ID_rate2vsGR",
    "seg_ID_t_test_summary",
    "site_details",
    "zone1",
    "zone2",
    "zone3"
  )
)])



##############################################################################################################
##3a.    plot results  of t.test ########################################################################

seg_ID_t_test_summary$Rate_as_factor <- as.factor(seg_ID_t_test_summary$Rates)


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

#Zone3
zone3_min <- filter(seg_ID_t_test_summary, Zone == zone3) %>% 
  summarise(min_zone = min(SegmentID))
zone3_min <- zone3_min[[1]]
zone3_max <- filter(seg_ID_t_test_summary, Zone == zone3) %>% 
  summarise(max_zone = max(SegmentID))
zone3_max <- zone3_max[[1]]

zone3_range <- ((zone3_max - zone3_min)/2)+zone3_min
zone3_range

##3b. Plot the results 
segments <- ggplot(seg_ID_t_test_summary, aes(SegmentID , Yld, group = Rate_as_factor))+
  geom_line(size=1, alpha=0.4, aes( color = Rate_as_factor ))+
  scale_color_manual(values=c('darkgrey','green', 'blue', 'red'), name  = Fert_legend_name)+
  theme_bw()+
  ylim(0.0,4)+
  labs(x= "Distance along the strip (meters)",
       y = "Yield t/ha",
       title = "",
       subtitle = Paddock_tested_db,
       caption = "")+
   annotate("rect", xmin = zone1_min, xmax = zone1_max, ymin = 0, ymax = 4, #Zone 1
           alpha = .2) +
  annotate("text", x = zone1_range, y= 1,label = zone1)+
  
   annotate("rect", xmin =zone2_min , xmax = zone2_max, ymin = 0, ymax = 4, #zone 2
            alpha = .2)+
  annotate("text", x = zone2_range, y= 1,label = zone2)+
  
  annotate("rect", xmin =zone3_min , xmax = zone3_max, ymin = 0, ymax = 4, #zone 3
           alpha = .2)+
  annotate("text", x = zone3_range, y= 1,label = zone3)
#+
 # annotate("text", x = 40, y= 1,label = "Missing data")


##3c. Save the results of the segment work
segments #this is the graph
ggsave(path= graph_path, filename = "t-test_segments.png", device = "png" ,
       width = 20, height = 10, units = "cm")


seg_ID_t_test_summary <- left_join(seg_ID_t_test_summary,list_rates)

write.csv(seg_ID_t_test_summary, paste0(graph_path,"/t_test_segments.csv"))



################################################################################################################################### 

##4a.  Paired t test for zone strip Zone 1 ####
 
##average the yield values in each line segment - this ensure I have the same number of points
# filter out data so we just have zone 1
zone_av_1 <- filter(seg_ID, Zone == zone1 ) %>% 
  dplyr::select(- Zone) %>% 
group_by(SegmentID, Rates ) %>% 
   summarise_all(mean)
zone_av_1$Rate_as_factor  <- as.factor(zone_av_1$Rates) 
zone_av_1

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


 # Plot the results
 zone_av_1
 
 zone_1 <- ggplot( zone_av_1, aes(Rate_as_factor, Yld))+
   geom_boxplot(alpha=0.1)+
   geom_point(colour = "blue", alpha = 0.1)+
   stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                width = .75, linetype = "dashed")+
   theme_bw()+
   ylim(0,4)+
   theme(axis.text=element_text(size=8),
         axis.title=element_text(size=10,))+
   labs(x = Fert_legend_name,
        y= "Yield t/ha",
        title = zone1,
        caption = "Below table reports mean values and significant differences compared to GSR")+
   theme(plot.caption = element_text(size=8, face="italic", color="black"))+
   annotate("text", x = 2, y= 0, size = 3,label = "box plot = 25%, 50%, 75%, dashed line = mean")
 zone_1
 
 ##save the graphs of the zone strip work
 ggsave(path= graph_path, filename = "t-test_zone_zone1_strip.png", device = "png" ,
        width = 20, height = 10, units = "cm")
 
 #make a table of the mean yield for zones with t test reuslts
 zone_av_1
 mean_zone_av_1 <-  group_by(zone_av_1, Rates) %>% 
   summarise(mean(Yld))
 mean_zone_av_1 <- left_join(mean_zone_av_1,list_rates)
 mean_zone_av_1
 #----------- user inputs-------#
 #how many to join?
 # 1 rates
 #mean_zone_av_1and_res_sig <-  zone_av_1_rate1vsGR_res_sig
 # 2 rates
 #mean_zone_av_1and_res_sig <-  rbind(zone_av_1_rate1vsGR_res_sig, zone_av_1_rate2vsGR_res_sig)
 # 3 rates
 mean_zone_av_1and_res_sig <-  rbind(zone_av_1_rate1vsGR_res_sig, zone_av_1_rate2vsGR_res_sig, zone_av_1_rate3vsGR_res_sig)
 
 mean_zone_av_1 <- left_join(mean_zone_av_1,mean_zone_av_1and_res_sig)

 mean_zone_av_1 <- mutate(mean_zone_av_1, 
                          Zone = zone1,
                          Organisation =Organisation_db,
                          Contact = Contact_db,
                          Farmer = Farmer_db)
 #, Paddock_tested = Paddock_tested_db)
 names(mean_zone_av_1)[2] <- "Yld"
 
 zone_av_1 <- left_join(zone_av_1,list_rates)
 write.csv(zone_av_1, paste0(graph_path,"/t_testzone_zone1_av.csv"))
 
 
 ###########################################################################################################################################
 ##4b.  Paired t test for zone strip Zone 2 ####
 
 ##average the yield values in each line segment - this ensure I have the same number of points
 # filter out data so we just have zone 2
 zone_av_2 <- filter(seg_ID, Zone == zone2 ) %>% 
   dplyr::select(- Zone) %>% 
   group_by(SegmentID, Rates ) %>% 
   summarise_all(mean)
 zone_av_2$Rate_as_factor  <- as.factor(zone_av_2$Rates) 
 zone_av_2 <- left_join(zone_av_2,list_rates)
 zone_av_2
 
 #subset the zone 2 data
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
  #Report values from the t.test
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
 

 
 # Plot the results
 
 zone_av_2$Rate_as_factor  <- as.factor(zone_av_2$Rates)
 zone_2 <- ggplot( zone_av_2, aes(Rate_as_factor, Yld))+
   geom_boxplot(alpha=0.1)+
   geom_point(colour = "blue", alpha = 0.1)+
   stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                width = .75, linetype = "dashed")+
   theme_bw()+
   ylim(0,4)+
   theme(axis.text=element_text(size=8),
         axis.title=element_text(size=10,))+
   labs(x = Fert_legend_name,
        y= "Yield t/ha",
        title = zone2,
        caption = "")
   
 zone_2
 
 ##save the graphs of the zone strip work
 ggsave(path= graph_path, filename = "t-test_zone_zone2_strip.png", device = "png" ,
        width = 20, height = 10, units = "cm")
 
 #make a table of the mean yield for zones with t test reuslts
 zone_av_2
 mean_zone_av_2 <-  group_by(zone_av_2, Rates) %>% 
   summarise(mean(Yld))
 mean_zone_av_2 <- left_join(mean_zone_av_2,list_rates)
 
 #----------- user inputs-------#
 #how many to join?
 # 1 rates
 #mean_zone_av_2and_res_sig <-  zone_av_2_rate1vsGR_res_sig
 # 2 rates
 #mean_zone_av_2and_res_sig <-  rbind(zone_av_2_rate1vsGR_res_sig, zone_av_2_rate2vsGR_res_sig)
 # 3 rates
 mean_zone_av_2and_res_sig <-  rbind(zone_av_2_rate1vsGR_res_sig, zone_av_2_rate2vsGR_res_sig, zone_av_2_rate3vsGR_res_sig)
 
 mean_zone_av_2 <- left_join(mean_zone_av_2,mean_zone_av_2and_res_sig)
 mean_zone_av_2 <- mutate(mean_zone_av_2, 
                          Zone = zone2,
                          Organisation =Organisation_db,
                          Contact = Contact_db,
                          Farmer = Farmer_db)

 mean_zone_av_2
 
 names(mean_zone_av_2)[2] <- "Yld"
 mean_zone_av_2
 write.csv(zone_av_2, paste0(graph_path,"/t_testzone_zone2_av.csv"))
 
 
 ###########################################################################################################################################
 ##4b.  Paired t test for zone strip Zone 3 ####
 
 ##average the yield values in each line segment - this ensure I have the same number of points
 # filter out data so we just have zone 2
 zone_av_3 <- filter(seg_ID, Zone == zone3 ) %>% 
   dplyr::select(- Zone) %>% 
   group_by(SegmentID, Rates ) %>% 
   summarise_all(mean)
 zone_av_3$Rate_as_factor  <- as.factor(zone_av_3$Rates) 
 zone_av_3 <- left_join(zone_av_3,list_rates)
 zone_av_3
 
 #subset the zone 2 data
 zone_av_3_rate1vsGR <- filter(zone_av_3, Rates == rate1 | Rates== Grower_rate )
 zone_av_3_rate2vsGR <- filter(zone_av_3, Rates == rate2 | Rates== Grower_rate )
 zone_av_3_rate3vsGR <- filter(zone_av_3, Rates == rate3 | Rates== Grower_rate )
 
 #ensure that the dataset is duplictaed
 list_SegmentID_values <- zone_av_3_rate1vsGR$SegmentID[duplicated(zone_av_3_rate1vsGR$SegmentID)] #this returns a list of values I want to keep
 zone_av_3_rate1vsGR <- zone_av_3_rate1vsGR %>% filter(SegmentID %in% list_SegmentID_values)
 
 list_SegmentID_values <- zone_av_3_rate2vsGR$SegmentID[duplicated(zone_av_3_rate2vsGR$SegmentID)] #this returns a list of values I want to keep
 zone_av_3_rate2vsGR <- zone_av_3_rate2vsGR %>% filter(SegmentID %in% list_SegmentID_values)
 
 list_SegmentID_values <- zone_av_3_rate3vsGR$SegmentID[duplicated(zone_av_3_rate3vsGR$SegmentID)] #this returns a list of values I want to keep
 zone_av_3_rate3vsGR <- zone_av_3_rate3vsGR %>% filter(SegmentID %in% list_SegmentID_values)
 
 
 #run the paired t test
 zone_av_3_rate1vsGR_res <- t.test(Yld ~ Rates, data = zone_av_3_rate1vsGR, paired = TRUE)
 zone_av_3_rate2vsGR_res <- t.test(Yld ~ Rates, data = zone_av_3_rate2vsGR, paired = TRUE)
 zone_av_3_rate3vsGR_res <- t.test(Yld ~ Rates, data = zone_av_3_rate3vsGR, paired = TRUE)
 
 #####test 1 results
 # Report values from the t.test
 zone_av_3_rate1vsGR_res_sig <-
   data.frame(P_value = as.double(zone_av_3_rate1vsGR_res$p.value),
              Mean_diff = (zone_av_3_rate1vsGR_res$estimate)) %>%
   mutate(
     rate_name = "rate1",
     rounded = abs(round(Mean_diff, 2)),
     Significant = case_when(P_value < 0.05 ~ "significant",
                             TRUE ~ "not significant"))
 zone_av_3_rate1vsGR_res_sig 
 
 ####test 2 results
 # Report values from the t.test
 zone_av_3_rate2vsGR_res_sig <-
   data.frame(P_value = as.double(zone_av_3_rate2vsGR_res$p.value),
              Mean_diff = (zone_av_3_rate2vsGR_res$estimate)) %>%
   mutate(
     rate_name = "rate2",
     rounded = abs(round(Mean_diff, 2)),
     Significant = case_when(P_value < 0.05 ~ "significant",
                             TRUE ~ "not significant"))
 
 ####test 3 results
 # Report values from the t.test
 zone_av_3_rate3vsGR_res
 #Report values from the t.test
 zone_av_3_rate3vsGR_res_sig <-
   data.frame(P_value = as.double(zone_av_3_rate3vsGR_res$p.value),
              Mean_diff = (zone_av_3_rate3vsGR_res$estimate)) %>%
   mutate(
     rate_name = "rate3",
     rounded = abs(round(Mean_diff, 2)),
     Significant = case_when(P_value < 0.05 ~ "significant",
                             TRUE ~ "not significant"))
 
 zone_av_3_rate1vsGR_res_sig 
 zone_av_3_rate2vsGR_res_sig
 zone_av_3_rate3vsGR_res_sig
 
 
 
 # Plot the results
 
 zone_av_3$Rate_as_factor  <- as.factor(zone_av_3$Rates)
 zone_3 <- ggplot( zone_av_3, aes(Rate_as_factor, Yld))+
   geom_boxplot(alpha=0.1)+
   geom_point(colour = "blue", alpha = 0.1)+
   stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                width = .75, linetype = "dashed")+
   theme_bw()+
   ylim(0,4)+
   theme(axis.text=element_text(size=8),
         axis.title=element_text(size=10,))+
   labs(x = Fert_legend_name,
        y= "Yield t/ha",
        title = zone3,
        caption = "")
 
 zone_3
 
 ##save the graphs of the zone strip work
 ggsave(path= graph_path, filename = "t-test_zone_zone3_strip.png", device = "png" ,
        width = 20, height = 10, units = "cm")
 
 #make a table of the mean yield for zones with t test reuslts
 zone_av_3
 mean_zone_av_3 <-  group_by(zone_av_3, Rates) %>% 
   summarise(mean(Yld))
 mean_zone_av_3 <- left_join(mean_zone_av_3,list_rates)
 
 #----------- user inputs-------#
 #how many to join?
 # 1 rates
 #mean_zone_av_3and_res_sig <-  zone_av_3_rate1vsGR_res_sig
 # 2 rates
 #mean_zone_av_3and_res_sig <-  rbind(zone_av_3_rate1vsGR_res_sig, zone_av_3_rate2vsGR_res_sig)
 # 3 rates
 mean_zone_av_3and_res_sig <-  rbind(zone_av_3_rate1vsGR_res_sig, zone_av_3_rate2vsGR_res_sig, zone_av_3_rate3vsGR_res_sig)
 
 mean_zone_av_3 <- left_join(mean_zone_av_3,mean_zone_av_3and_res_sig)
 mean_zone_av_3 <- mutate(mean_zone_av_3, 
                          Zone = zone3,
                          Organisation =Organisation_db,
                          Contact = Contact_db,
                          Farmer = Farmer_db)
 
 names(mean_zone_av_3)[2] <- "Yld"
 
 write.csv(zone_av_3, paste0(graph_path,"/t_testzone_zone3_av.csv"))
 
 
 
 
#####################################################################################################################################
### should get this from harms database
 #bring in data from the most current database from dropbox
 
 #set up access to dropbox when is is password protected
 token<-drop_auth()
 saveRDS(token, "droptoken.rds")
 
 token<-readRDS("droptoken.rds")
 drop_acc(dtoken=token)
 database_name_of_path
 
 #https://www.dropbox.com/home/GRDC_Soil_Plant_Testing_Database
 #download the database file from dropbox and save it to mircolab
 drop_download(path = "NP_database_31032020_SA.xlsx", 
               local_path = database_name_of_path,
               dtoken = token,
               overwrite = TRUE)
 
 #bring in the excel sheet as a r object
 database_name_of_path
 
harm_database <- read_excel(paste0(
                            database_name_of_path,"/", "NP_database_31032020_SA.xlsx"),
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
 
 ## Pull out the infor for the paddock I am testing..
 str(harm_database)
 site <- filter(harm_database,
                Paddock_tested == Paddock_tested_db) %>% 
   dplyr::select(5, 6: 11)
 
 
 
 site <- site %>% mutate_if(is.numeric, ~round(., 0))
 site
 Paddock_tested_db


#make a table of the mean yield for zones
mean_zone_av_1
mean_zone_av_2
mean_zone_av_3
 
#---- user input - how many to zones to join

#mean_zone_av_output <- as.data.frame( rbind(mean_zone_av_1, mean_zone_av_2))
mean_zone_av_output <- as.data.frame( rbind(mean_zone_av_1, mean_zone_av_2, mean_zone_av_3))

mean_zone_av_output
site_details
write.csv(mean_zone_av_output, paste0(finished_name_of_path,
                                 "/",
                                 site_details,
                                 "_",
                                 "_zones.csv"))

### display this on the png results page
mean_zone_av_1
mean_zone_av_2
mean_zone_av_3
#---- user input - how many to zones to join
#mean_zone_av_p_values <- rbind(mean_zone_av_1,mean_zone_av_2) 
mean_zone_av_p_values <- rbind(mean_zone_av_1, mean_zone_av_2, mean_zone_av_3)
  
mean_zone_av_p_values <- dplyr::select(mean_zone_av_p_values, Rates, Significant,Zone )
mean_zone_av_output_display <- dplyr::select(mean_zone_av_output,
                                   Rates, 
                                   Yld, 
                                   Zone)

mean_zone_av_output_display <- left_join(mean_zone_av_output_display, mean_zone_av_p_values, by=c("Rates" = "Rates", "Zone" = "Zone" ))
mean_zone_av_output_display <-mean_zone_av_output_display %>% 
                              mutate(Significant = case_when(Significant == "significant" ~ "*",
                              TRUE ~ "" ))
mean_zone_av_output_display <- mean_zone_av_output_display %>% mutate_if(is.numeric, ~round(., 1))
mean_zone_av_output_display <- mutate(mean_zone_av_output_display,
               Yld = paste0(Yld, Significant))
mean_zone_av_output_display <- dplyr::select(mean_zone_av_output_display, -Significant)
mean_zone_av_output_display
mean_zone_av_output_display <- spread(mean_zone_av_output_display, Zone, Yld)
mean_zone_av_output_display
#remove the NA
#mean_zone_av_output_display[] <- replace(as.matrix(mean_zone_av_output_display), is.na(mean_zone_av_output_display), "")



TSpecial <- ttheme_minimal(base_size = 8)
table1 <- tableGrob(site , rows = NULL, theme=TSpecial )
table2 <- tableGrob(mean_zone_av_output_display, rows = NULL, theme=TSpecial)

#get the name of the paddock...

paddock <- Paddock_tested_db


library(DT)
test <- textGrob(paddock)
 ####################################################################################################################################
 ## Arrange the outputs onto one page
segments
zone_1
zone_2
zone_3
table1
paddock
collection <- grid.arrange(zone_1, zone_2,zone_3, table2, table1, segments, nrow = 5,  ncol=3, 
                           layout_matrix = cbind(c(1,1,4,6,6),c(2,2,5,6,6), c(3,3,5,6,6)))

# collection <- grid.arrange(zone_1, zone_2, table2, table1, segments, nrow = 5,  ncol=2, 
#               layout_matrix = cbind(c(1,1,3,5,5), c(2,2,4,5,5)))
             
collection
ggsave(path= graph_path, filename = paste0(paddock, "_collection.png"), device = "png", 
       width = 21, height = 15, units = "cm", collection)







##########################################################################################################################################

