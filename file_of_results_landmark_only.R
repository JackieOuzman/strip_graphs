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

my_list_landmark_files <- list.files(path = path_finished_wk,
                                     pattern = "^[Landmark]")
#pattern = ".csv") 
print(my_list_landmark_files)
#remove the summary landmark file Landmark_15_05_2020.csv - which appear first
landmark_files <- my_list_landmark_files[2:40]
landmark_files

data <- 
  do.call("rbind", 
          lapply(landmark_files, #this is a list of files
                 function(x) 
                   read.csv(paste(path_finished_wk, x, sep=''), 
                            stringsAsFactors = FALSE)))




# ####Some files are missing paddock info so I need to add these now - oops!
# modify <- read.csv("Landmark_Steve_Richmond_5_AC_Jacka_2_Home_6__zones.csv")
# modify <- modify %>% 
#   mutate(Paddock_tested = "Home_6")
# write_csv(modify,"Landmark_Steve_Richmond_5_AC_Jacka_2_Home_6__zones.csv")
# 
# ####Some files are missing paddock info and other info so I need to add these now - oops!
# modify <- read.csv("Landmark_James_Falvey_2_Tim_McClelland_4_Backsheehans__zones.csv")
# modify <- modify %>% 
#   mutate(Details = "not supplied yet",
#          Starter_Feriliser = "not supplied yet",
#          Topdress = "not supplied yet",
#          Zone = "no zones")
# write_csv(modify,"Landmark_James_Falvey_2_Tim_McClelland_4_Backsheehans__zones.csv")


##these are the files that have clm added to them
dim(read.csv( "Landmark_Claire_Gutsche_6_Lister_Trading_1_Bevs__zones.csv" ))  #16     
dim(read.csv( "Landmark_Claire_Gutsche_6_Lister_Trading_1_Willings_Willows__zones.csv"))#16
dim(read.csv( "Landmark_Hamish_Verco_7_Ballinger_1_Reschkes_North__zones.csv"))  #16       
dim(read.csv( "Landmark_Heath_Griffith_5_Campbell_2_Brennans__zones.csv"))   #16       
dim(read.csv( "Landmark_James_Falvey_2_Tim_McClelland_4_Clover__zones.csv"))  #16          
dim(read.csv("Landmark_James_Falvey_2_Tim_McClelland_4_Hennessy__zones.csv"  )) #16  
dim(read.csv( "Landmark_James_Falvey_2_Tim_McClelland_4_Top__zones.csv"  ))  #16 
dim(read.csv( "Landmark_James_Falvey_2_Tim_McClelland_4_Mervyns__strip.csv" )) #16 
dim(read.csv( "Landmark_James_Falvey_2_Tim_McClelland_4_Backsheehans__zones.csv" )) #16

#these were good from the start
dim(read.csv("Landmark_Alister_Tippert_1_Chris_Shark_4_Balls_Centre_2__zones.csv")) #16
dim(read.csv("Landmark_Alister_Tippert_1_Chris_Shark_4_Balls_Centre_2__zones.csv" ))   
dim(read.csv("Landmark_Alister_Tippert_1_Chris_Shark_4_Balls_Silo_1__zones.csv" ))     
dim(read.csv("Landmark_Alister_Tippert_1_Leighview_2_Jash_5__zones.csv" ))             
dim(read.csv( "Landmark_Alister_Tippert_1_PaulJenz_3_Harrys_Long_2__zones.csv"))        
dim(read.csv( "Landmark_Andrew_McMahen_8_Barney_OCallaghan_2_Hickmont__zones.csv" ))    
dim(read.csv( "Landmark_Andrew_McMahen_8_Barney_OCallaghan_2_Jardines__zones.csv" ))    
dim(read.csv( "Landmark_Andrew_McMahen_8_Barney_OCallaghan_2_NOC_2__zones.csv" ))       
dim(read.csv( "Landmark_Andrew_McMahen_8_Birkinshaw_1_Rail__zones.csv"   ))             
dim(read.csv( "Landmark_Andrew_McMahen_8_Birkinshaw_1_Round_Home__zones.csv"))          
dim(read.csv( "Landmark_Chris_Dunn_7_Jenharwil_2_Brooklands_2__zones.csv"  ))           
dim(read.csv( "Landmark_Chris_Dunn_7_Jenharwil_2_James_2__zones.csv" ))                 
dim(read.csv( "Landmark_Chris_Dunn_7_Jenharwil_2_Lindsey_Cali__zones.csv"  )) 
dim(read.csv( "Landmark_Claire_Gutsche_6_Lister_Trading_1_East_Laundys__zones.csv" ))#16 
dim(read.csv( "Landmark_James_Falvey_2_David_Ferrier_School__zones.csv"))               
dim(read.csv( "Landmark_James_Falvey_2_Frankel_2_Front_1__zones.csv"  ))                
dim(read.csv( "Landmark_James_Falvey_2_Frankel_2_Front_3__zones.csv"  ))                
dim(read.csv( "Landmark_James_Falvey_2_Frankel_2_Stewarts_3__zones.csv"  ))             
dim(read.csv( "Landmark_James_Falvey_2_Lehmann_3_Back__zones.csv"   ))                  
dim(read.csv( "Landmark_James_Falvey_2_Lehmann_3_Swamp__zones.csv" ))                   
dim(read.csv( "Landmark_James_Falvey_2_Tim_McClelland_4_Backsheehans__zones.csv"  ))  
dim(read.csv( "Landmark_Kris_Dixon_6_Heightons_1_Bellas__zones.csv"  ))                 
dim(read.csv( "Landmark_Kris_Dixon_6_Heightons_1_McKenzie__zones.csv"  ))               
dim(read.csv( "Landmark_Matt_Nihill_7_GRG_Weeks_1_Reids_mid__zones.csv"   ))            
dim(read.csv( "Landmark_Matt_Nihill_7_GRG_Weeks_1_Reids_mount__zones.csv"  ))           
dim(read.csv( "Landmark_Steve_Richmond_5_AC_Jacka_2_Heads__zones.csv"   ))              
dim(read.csv( "Landmark_Steve_Richmond_5_AC_Jacka_2_Home_6__zones.csv"  ))              
dim(read.csv( "Landmark_Steve_Richmond_5_Clarke_Bros_1_07_Back_Grussing__zones.csv"  )) #16
dim(read.csv( "Landmark_Steve_Richmond_5_Clarke_Bros_1_30_below_house__zones.csv"  ))   
dim(read.csv( "Landmark_Tom_Cooper_9_A_P_Robertson_1_Affecks__zones.csv" ))             
dim(read.csv( "Landmark_Tom_Cooper_9_A_P_Robertson_1_Bird__zones.csv"))                 
dim(read.csv( "Landmark_Tom_Cooper_9_A_P_Robertson_1_McPhees__zones.csv"))   

          


write.csv(data, paste0(path_finished_wk,
                                      "complied",
                                      "/",
                                      "Landmark_",
                                      Sys.Date(),
                                      "_For_TM.csv"))
    


