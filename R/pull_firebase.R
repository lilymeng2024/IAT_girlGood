# require devtools and fireData packages
if (!require("devtools")) install.packages("devtools")
if (!require("fireData")) devtools::install_github("Kohze/fireData")

library(fireData) # https://github.com/Kohze/fireData


# secret key = Ce72TrPPKxBYHmCDEvvVebDINnDy76TXF3lo2So7

# download all files from database
dataBackup(projectURL = "https://jspsych-online-experiment-default-rtdb.firebaseio.com", #databaseURL
           secretKey = "Ce72TrPPKxBYHmCDEvvVebDINnDy76TXF3lo2So7", #Secret Key of the database
           "C:/Users/25688/OneDrive - University of Manitoba/桌面/IAT_girlGood./data/data.json") # file path of where to save the data (out default assumes cloning the GitHub repo)


# install.packages("jsonlite")
library("jsonlite")


#································To check the number of data points in the database ··············································

library(jsonlite)
library(dplyr)
library(plyr)

raw <- fromJSON("C:/Users/25688/OneDrive - University of Manitoba/桌面/IAT_girlGood./data/data.json")

participants <- raw$participants

all_data <- lapply(participants, function(p){
  
  df <- bind_rows(p$data)
  
  # keep only trials with block
  df <- df %>% filter(!is.na(block))
  
  # create trial index within block
  df <- df %>%
    dplyr::group_by(block) %>%
    dplyr::mutate(trial_in_block = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  df$subject <- p$subject
  df$participant_id <- p$participant_id
  df$trial_label <- paste0("block", df$block, "_", df$trial_in_block)
  
  return(df)
})

final_data <- rbind.fill(all_data)

#********************************************************************************************************************************
#*******************Data will still have json levels that need to be unpacked if you collected survey responses******************
#*******************One way to do this will be demonstrated in data_analysis.R***************************************************
#********************************************************************************************************************************
#number of subjects
length(unique(final_data$subject))

# save data to .csv file

final_data <- final_data %>%
  arrange(subject, block, trial_in_block)

#remove unnecessary columns
final_data_clean <- final_data %>%
  select(-plugin_version, -response, -time_elapsed, -trial_type)

write.csv(final_data_clean, "C:/Users/25688/OneDrive - University of Manitoba/桌面/IAT_girlGood./data/data.csv", row.names = FALSE)
