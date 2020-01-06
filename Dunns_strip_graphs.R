library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)

#### 0P ############################################################################################
#0 25 100 - I need no 25 no 100

name_of_path <-
  file.path("W:", "value_soil_testing_prj", "Yield_data", "Dunns", "Jax_processing", "no100_no25", "Trial-0_E-strip.csv")
Dunn_strips_0 <-read_csv(name_of_path)

Dunn_strips_0 <- select(Dunn_strips_0, -TrialID) %>% 
  mutate(name_strip = "0MESZ")


Dunn_strips_0 <- mutate(Dunn_strips_0,
                         zone = case_when(
                           PointID >= 28 & PointID <= 47 ~ 1,
                           PointID >= 17 & PointID <= 36 ~ 1, # should be the same across all the files
                           #PointID >= 104 & PointID <= 122 ~ 2,
                           PointID >= 104 & PointID <= 122 ~ 2))
Dunn_strips_0 <- mutate(Dunn_strips_0,
                         zone_st_practice = case_when(
                           PointID >= 17 & PointID <= 36 ~ "1_50",
                           PointID >= 104 & PointID <= 122 ~ "2_50"))

Dunn_strips_0 <- mutate(Dunn_strips_0,
                         zone_st_strip = case_when(
                           PointID >= 28 & PointID <= 47 ~ "1_0",
                           PointID >= 104 & PointID <= 122 ~ "2_0"))  






#### 25P ############################################################################################
#0 25 100 - I need  no 0 no 100
name_of_path <-
      file.path("W:", "value_soil_testing_prj", "Yield_data", "Dunns", "Jax_processing", "no0_no100", "Trial-0_E-strip.csv")
Dunn_strips_25 <-read_csv(name_of_path)

Dunn_strips_25 <- select(Dunn_strips_25, -TrialID) %>% 
  mutate(name_strip = "25MESZ")
         

Dunn_strips_25 <- mutate(Dunn_strips_25,
                                 zone = case_when(
                                   PointID >= 23 & PointID <= 42 ~ 1,
                                   PointID >= 17 & PointID <= 36 ~ 1,
                                   PointID >= 104 & PointID <= 122 ~ 2))
Dunn_strips_25 <- mutate(Dunn_strips_25,
                           zone_st_practice = case_when(
                             PointID >= 17 & PointID <= 36 ~ "1_50",
                             PointID >= 104 & PointID <= 122 ~ "2_50"))

Dunn_strips_25 <- mutate(Dunn_strips_25,
                           zone_st_strip = case_when(
                             PointID >= 23 & PointID <= 42 ~ "1_25",
                             PointID >= 104 & PointID <= 122 ~ "2_25"))                                                           
                                                   



#### 100P ############################################################################################
#0 25 100 - I need  no 0 no 25

name_of_path <-
  file.path("W:", "value_soil_testing_prj", "Yield_data", "Dunns", "Jax_processing", "no0_no25", "Trial-0_E-strip.csv")
Dunn_strips_100 <-read_csv(name_of_path)

Dunn_strips_100 <- select(Dunn_strips_100, -TrialID) %>% 
  mutate(name_strip = "100MESZ")


Dunn_strips_100 <- mutate(Dunn_strips_100,
                         zone = case_when(
                           PointID >= 32 & PointID <= 51 ~ 1,
                           PointID >= 17 & PointID <= 36 ~ 1, # should be the same across all the files
                           PointID >= 104 & PointID <= 122 ~ 2))
Dunn_strips_100 <- mutate(Dunn_strips_100,
                         zone_st_practice = case_when(
                           PointID >= 17 & PointID <= 36 ~ "1_50",
                           PointID >= 104 & PointID <= 122 ~ "2_50"))

Dunn_strips_100 <- mutate(Dunn_strips_100,
                         zone_st_strip = case_when(
                           PointID >= 32 & PointID <= 51 ~ "1_100",
                           PointID >= 104 & PointID <= 122 ~ "2_100"))       
#############################################################################################################
####### Merge together ###########################################################################

str(Dunn_strips_0)
str(Dunn_strips_25)
str(Dunn_strips_100)
Dunn_strips <- rbind(Dunn_strips_0,
                         Dunn_strips_25,
                         Dunn_strips_100)
str(Dunn_strips$`E Strip Value`)
str(Dunn_strips)

test <- Dunn_strips
test <- mutate(test,
               value = `E Strip Value`)

test <- filter(test,
               name_strip == "0MESZ")

test <- mutate(test,
               name_strip = case_when(
                  name_strip == "0MESZ" ~ "50MESZ"))
Dunn_strips <-  mutate(Dunn_strips,
                       value = `Strip Value`)

str(test)  
#test <- select(test, DistOnLine,name_strip,  value)
test$`E Strip Value` <- NA
test$`E Strip Zone` <- NA
test$`Strip Zone` <- NA
test$`Strip Value` <- NA
test$treat_diff <- NA
test$av_treat_dif <- NA
test$p_value <- NA
test$RI <- NA


#join the files togther
Dunn_strips_plotting <- rbind(Dunn_strips, test)

unique(Dunn_strips_plotting$name_strip)


Dunn_strips_plotting$name_strip <- factor(Dunn_strips_plotting$name_strip, 
                              levels = c("0MESZ",
                                         "25MESZ", 
                                         "50MESZ",
                                         "100MESZ"
                                          ))

unique(Dunn_strips_plotting$name_strip)

ggplot(Dunn_strips_plotting, aes(DistOnLine, value, group = name_strip))+
  geom_line(size=2, alpha=0.4, aes( color = name_strip ))+
  #geom_line(size=2, aes(linetype = name_strip, color = name_strip ))+
  #scale_linetype_manual(values=c("dashed", "dashed", "solid", "dashed"))+
  scale_color_manual(values=c('darkgrey','green', "black", "blue"))+
  theme_bw()+
  labs(x= "distance along the strip (m)",
       y = "yield t/ha",
       title = "",
       subtitle = "",
       caption = "")+
  theme(legend.position = "none") +
  geom_point(data = filter(Dunn_strips_plotting, zone == 1), aes(DistOnLine, value), shape=1, size =2)+
  geom_point(data = filter(Dunn_strips_plotting, zone == 2), aes(DistOnLine, value), shape=1, size =2)

#what is the average yield for each treatment
average_yld_per_sample_zone <- group_by(Dunn_strips_plotting, name_strip, zone) %>% 
  summarise(average = mean(value))

print(average_yld_per_sample_zone)

average_yld_per_treatmnet <- group_by(Dunn_strips_plotting, name_strip) %>% 
  summarise(average = mean(value))

print(average_yld_per_treatmnet)


# make a clm for rate of P applied and N applied


Dunn_strips_partialGM <- Dunn_strips_plotting %>% 
  mutate(
         p_applied = case_when(
           name_strip == "0MESZ" ~ 0,
           name_strip == "25MESZ" ~ 25,
           name_strip == "50MESZ" ~ 50,
           name_strip == "100MESZ" ~ 100),
         n_applied = case_when(
           name_strip == "0MESZ" ~ 45 + 75, # top dressed with 75 on the 28/6
           name_strip == "25MESZ" ~ 45+ 75,
           name_strip == "50MESZ" ~ 45 +75,
           name_strip == "100MESZ" ~ 45+ 75)
         )

cost_MESZ = 690 #$/t
cost_urea = 455 #$/t
port_price = 250
Dunn_strips_partialGM <- Dunn_strips_partialGM %>% 
  mutate(
    p_cost = ((p_applied/1000) * cost_MESZ),
    n_cost = ((n_applied/1000) * cost_urea),
    fert_cost = p_cost + n_cost,
    revenue = value * port_price,
    partial_GM = revenue - fert_cost
    )


Dunn_strips_partialGM$name_strip <- factor(Dunn_strips_partialGM$name_strip, 
                                          levels = c("0MESZ",
                                                     "25MESZ", 
                                                     "50MESZ",
                                                     "100MESZ"
                                          ))

unique(Dunn_strips_partialGM$name_strip)
str(Dunn_strips_partialGM)

#what is the average partial GM for each treatment
average_PGM_per_sample_zone <- group_by(Dunn_strips_partialGM, name_strip, zone) %>% 
  summarise(average = mean(partial_GM))

print(average_PGM_per_sample_zone)

average_PGM_per_treatmnet <- group_by(Dunn_strips_partialGM, name_strip) %>% 
  summarise(average = mean(partial_GM))

print(average_PGM_per_treatmnet)




ggplot(Dunn_strips_partialGM, aes(DistOnLine, partial_GM, group = name_strip))+
  geom_line(size=2, alpha=0.4, aes( color = name_strip ))+
  #geom_line(size=2, aes(linetype = name_strip, color = name_strip ))+
  #scale_linetype_manual(values=c("dashed", "dashed", "solid", "dashed"))+
  scale_color_manual(values=c('darkgrey','green', "black", "blue"))+
  theme_bw()+
  labs(x= "distance along the strip (m)",
       y = "partial GM ($ per ha)",
       title = "",
       subtitle = "",
       caption = "")+
  theme(legend.position = "none") +
  #scale_y_continuous(labels = scales::percent_format(accuracy = 5L))
  geom_point(data = filter(Dunn_strips_partialGM, zone == 1), aes(DistOnLine, partial_GM), shape=1, size =2)+
  geom_point(data = filter(Dunn_strips_partialGM, zone == 2), aes(DistOnLine, partial_GM), shape=1, size =2)



########################################################################################################


##Read in file
name_of_path <-
  file.path("W:", "value_soil_testing_prj", "Yield_data", "Dunns", "Raw_data",  "AndrewParsons_Dunns_23Nov2019_extracted.csv")
Dunn_raw_extracted <-read.csv(name_of_path)
str(Dunn_raw_extracted)
Dunn_raw_extracted <- select(Dunn_raw_extracted,
                             ID,Zone,Treatment,
                             Yld = Yld.Mass.Dry..tonne.ha.
                             )
Dunn_raw_extracted_summary <- group_by(Dunn_raw_extracted, ID, Treatment) %>% 
  summarise(value = mean(Yld))

print(Dunn_raw_extracted_summary)

Dunn_raw_extracted_summary_partialGM <- Dunn_raw_extracted_summary %>% 
  mutate(
    p_applied = case_when(
       Treatment == "MESZ 0" ~ 0,
       Treatment == "MESZ 25" ~ 25,
      Treatment == "MESZ 50" ~ 50,
      Treatment == "MESZ 100" ~ 100),
    n_applied = case_when(
      Treatment == "MESZ 0" ~ 45 + 75, # top dressed with 75 on the 28/6
      Treatment == "MESZ 25" ~ 45+ 75,
      Treatment == "MESZ 50" ~ 45 +75,
      Treatment == "MESZ 100" ~ 45+ 75)
  )
print(Dunn_raw_extracted_summary_partialGM)

Dunn_raw_extracted_summary_partialGM <- Dunn_raw_extracted_summary_partialGM %>% 
  mutate(
    p_cost = ((p_applied/1000) * cost_MESZ),
    n_cost = ((n_applied/1000) * cost_urea),
    fert_cost = p_cost + n_cost,
    revenue = value * port_price,
    partial_GM = revenue - fert_cost
  )

Dunn_raw_extractedpartialGM <- group_by(Dunn_raw_extracted_summary_partialGM, ID) %>% 
  summarise(average = mean(partial_GM))

print(Dunn_raw_extractedpartialGM)

