#Collating zone analsyis results

#1. install the lib that I will be using for this work

libs <- c("dplyr", "tidyverse", "stringr",
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

#bring in th zone analysis results for each site.
#what are the files that have been finished?
getwd()
path_finished_wk <- "W:/value_soil_testing_prj/Yield_data/finished/"
#path_finished_wk <- "C:/Users/ouz001/Desktop/soil_testing_proj_full/finished/"
setwd(path_finished_wk)

 my_list_non_landmark_files <- grep(list.files(path = path_finished_wk),
                                      pattern = "^[Landmark]", inv=T, value=T)

 my_list_Vic_Ind <- grep(my_list_non_landmark_files,
                                    pattern = "Vic_Ind", value = T)
 my_list_SA_Ind <- grep(my_list_non_landmark_files,
                         pattern = "SA_Ind", value = T)
 
 my_list_MSF <- grep(my_list_non_landmark_files,
                        pattern = "MSF", value = T)
 
 
 
 my_list_non_landmark_files
 my_list_Vic_Ind
 my_list_SA_Ind
 my_list_MSF
 
 
 
 
 # my_list_MSF_sub <- my_list_MSF[1:10]
 # # ####Some files are missing paddock info so I need to add these now - oops!
 #  modify <- read.csv("MSF_Michael_Moodie_Todd_McDonald_Nulty_Kirbys__zones.csv")
 #  modify <- modify %>% 
 #    mutate(Paddock_tested = "Nulty_Kirbys")
 #  write_csv(modify,"MSF_Michael_Moodie_Todd_McDonald_Nulty_Kirbys__zones.csv")
 
 
 MSF <- 
   do.call("rbind", 
           lapply(my_list_MSF, #this is a list of files
                  function(x) 
                    read.csv(paste(path_finished_wk, x, sep=''), 
                             stringsAsFactors = FALSE)))
 
 
Vic_Ind <- 
  do.call("rbind", 
          lapply(my_list_Vic_Ind, #this is a list of files
                 function(x) 
                   read.csv(paste(path_finished_wk, x, sep=''), 
                            stringsAsFactors = FALSE)))

SA_Ind <- 
  do.call("rbind", 
          lapply(my_list_SA_Ind, #this is a list of files
                 function(x) 
                   read.csv(paste(path_finished_wk, x, sep=''), 
                            stringsAsFactors = FALSE)))

names(Vic_Ind) #paddock should be Paddock_tested
names(SA_Ind) #paddock should be Paddock_tested
names(MSF)


# 
# ####Some files are missing paddock info and other info so I need to add these now - oops!

 Vic_Ind <- Vic_Ind %>% 
   mutate(Details = "not confirmed yet",
          Starter_Feriliser = "not confirmed yet",
          Topdress = "not supplied yet")
 Vic_Ind <- Vic_Ind %>% 
   rename(
     Paddock_tested = Paddock)
          

 SA_Ind <- SA_Ind %>% 
   mutate(Details = "not confirmed yet",
          Starter_Feriliser = "not confirmed yet",
          Topdress = "not supplied yet")
 SA_Ind <- SA_Ind %>% 
   rename(
     Paddock_tested = Paddock)
 
 MSF <- MSF %>% 
   mutate(Details = "not confirmed yet",
          Starter_Feriliser = "not confirmed yet",
          Topdress = "not supplied yet")



Non_Landmark <- rbind(Vic_Ind, SA_Ind, MSF)



write.csv(Non_Landmark, paste0(path_finished_wk,
                       "complied",
                       "/",
                       "Non_Landmark",
                       Sys.Date(),
                       "_For_TM.csv"))



