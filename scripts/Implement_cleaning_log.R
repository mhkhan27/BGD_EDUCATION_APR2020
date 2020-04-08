rm(list = ls())
library(dplyr)
library(butteR)

dropbox_path <- "C:\\Users\\MEHEDI\\Dropbox\\REACH_BGD\\REACH\\Ongoing\\70iAMD 70EAD - Host Community Education\\04_daily_monitoring\\01_facility_assessment\\03_data_cleaning_tracker"


cleaning_log <- read.csv(paste0(dropbox_path,"/cleaning_log.csv"),stringsAsFactors = F, 
                         na.strings = c(""," ", "n/a",NA)) %>% dplyr::filter(Issue != c("Added new column", "Added new column based on prevalence of other response"))

raw_df <- read.csv(paste0(dropbox_path,"/master_data.csv"),stringsAsFactors = F, 
                   na.strings = c(""," ", "n/a",NA))
butteR::check_cleaning_log(df = raw_df,df_uuid = "X_uuid",cl = cleaning_log,cl_change_type_col = "change_type",cl_uuid = "uuid",
                           cl_change_col = "indicator",cl_new_val = "new_value")

data_for_analysis <- butteR::implement_cleaning_log(df = raw_df,df_uuid = "X_uuid",cl = cleaning_log,cl_change_type_col = "change_type",cl_uuid = "uuid",
                                                    cl_change_col = "indicator",cl_new_val = "new_value")
write.csv(data_for_analysis,"inputs/01_data_collection/03_cleaned_data/data_for_analysis.csv")

