###This bit of code check is the values are duplicated and only retains duplicated values in fi9nal data frame


#check if all the SegmentID have duplicates
df <- zone_av_1_rate2vsGR
print(df) #not value 80 has no duplicate

list_SegmentID_values <- df$SegmentID[duplicated(df$SegmentID)] #this returns a list of values I want to keep
str(list_SegmentID_values) #notice the duplicated values are now dropped

df2 <- df %>% filter(SegmentID %in% list_values_to_keep)


##### how do I bring in dropbox data?


#https://www.dropbox.com/scl/fi/iqbra05xr03hk9xvkq6ak/Template.xlsx?dl=0&rlkey=t4yzx002jx0a9m9r544el6fhh

install.packages("repmis")    
library(repmis)

#FinURL <- paste0("#https://www.dropbox.com/scl/fi/iqbra05xr03hk9xvkq6ak/Template.xlsx


 FinDataFull <- repmis::source_DropboxData("fin_research_note.csv",
                                   "exh4iobbm2p5p1v",
                                   sep = ",",
                                   header = TRUE)
                                  
                                  
 FinDataFull <- repmis::source_DropboxData("Template.xlsx",
                                   "iqbra05xr03hk9xvkq6ak",
                                   sep = ",",
                                   header = TRUE)                               
                                  

----------------------------------------------------------------------------------------------------------
#install.packages("rdrop2")
library(rdrop2) 
 
 token<-drop_auth()
 saveRDS(token, "droptoken.rds")
 
 token<-readRDS("droptoken.rds")
 drop_acc(dtoken=token)

test <- drop_read_csv(file = "GRDC Soil Plant Testing Project 9176604 (Agronomy Solutions)/2019 Data summary CSIRO/26022020_NP_data_base.csv", 
                      dtoken = token)




 