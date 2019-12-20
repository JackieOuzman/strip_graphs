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

ggplot(Dunn_strips_plotting, aes(DistOnLine, value, colour = name_strip))+
  geom_point()



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
cost_urea = 45 #$/t
port_price = 250
Dunn_strips_partialGM <- Dunn_strips_partialGM %>% 
  mutate(
    p_cost = ((p_applied/1000) * cost_MESZ),
    n_cost = ((n_applied/1000) * cost_urea),
    fert_cost = p_cost + n_cost,
    revenue = value * port_price,
    partial_GM = revenue - fert_cost
    )


ggplot(Dunn_strips_partialGM, aes(DistOnLine, partial_GM, colour = name_strip))+
  geom_line()
