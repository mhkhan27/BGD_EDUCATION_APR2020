rm(list = ls())
library(dplyr)
library(butteR)

cleaning_log <- read.csv("inputs/01_data_collection/03_cleaned_data/cleaning_log.csv",stringsAsFactors = F, 
                         na.strings = c(""," ", "n/a",NA)) %>% dplyr::filter(Issue != c("Added new column", "Added new column based on prevalence of other response"))

raw_df <- read.csv("inputs/01_data_collection/03_cleaned_data/BGD_Education_Facility_Survey.csv",stringsAsFactors = F, 
                   na.strings = c(""," ", "n/a",NA))

butteR::check_cleaning_log(df = raw_df,df_uuid = "X_uuid",cl = cleaning_log,cl_change_type_col = "change_type",cl_uuid = "uuid",
                           cl_change_col = "indicator",cl_new_val = "new_value")

data_for_analysis <- butteR::implement_cleaning_log(df = raw_df,df_uuid = "X_uuid",cl = cleaning_log,cl_change_type_col = "change_type",cl_uuid = "uuid",
                                                    cl_change_col = "indicator",cl_new_val = "new_value")


data_for_analysis <- data_for_analysis %>% dplyr::mutate(
  i.edu_fee_annualy =if_else(barriers_and_facilitators.tuition_education_facilities_charging_fees_by_frequency == "monthly",                        
                             as.integer(barriers_and_facilitators.tuition_education_facilities_charging_fees*12),
                             if_else(barriers_and_facilitators.tuition_education_facilities_charging_fees_by_frequency == "quarterly",
                                     as.integer(barriers_and_facilitators.tuition_education_facilities_charging_fees*4),
                                     if_else(barriers_and_facilitators.tuition_education_facilities_charging_fees_by_frequency == "weekly",
                                             as.integer(barriers_and_facilitators.tuition_education_facilities_charging_fees*52),
                                             if_else(barriers_and_facilitators.tuition_education_facilities_charging_fees_by_frequency == "bi-annually",
                                                     as.integer(barriers_and_facilitators.tuition_education_facilities_charging_fees*2),
                                                     as.integer(barriers_and_facilitators.tuition_education_facilities_charging_fees), NULL)))),
  
  i.edu_fee_3_annualy =if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency3 == "monthly",                        
                               as.integer(barriers_and_facilitators.average_education_facility_fees3*12),
                               if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency3 == "quarterly",
                                       as.integer(barriers_and_facilitators.average_education_facility_fees3*4),
                                       if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency3 == "weekly",
                                               as.integer(barriers_and_facilitators.average_education_facility_fees3*52),
                                               if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency3 == "bi-annually",
                                                       as.integer(barriers_and_facilitators.average_education_facility_fees3*2),
                                                       as.integer(barriers_and_facilitators.average_education_facility_fees3), NULL)))),
  
  i.edu_fee_4_annualy =if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency4 == "monthly",                        
                               as.integer(barriers_and_facilitators.average_education_facility_fees4*12),
                               if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency4 == "quarterly",
                                       as.integer(barriers_and_facilitators.average_education_facility_fees4*4),
                                       if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency4 == "weekly",
                                               as.integer(barriers_and_facilitators.average_education_facility_fees4*52),
                                               if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency4 == "bi-annually",
                                                       as.integer(barriers_and_facilitators.average_education_facility_fees4*2),
                                                       as.integer(barriers_and_facilitators.average_education_facility_fees4), NULL)))),
  
  i.edu_fees_5_annualy =if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency5 == "monthly",                        
                                as.integer(barriers_and_facilitators.average_education_facility_fees5*12),
                                if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency5 == "quarterly",
                                        as.integer(barriers_and_facilitators.average_education_facility_fees5*4),
                                        if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency5 == "weekly",
                                                as.integer(barriers_and_facilitators.average_education_facility_fees5*52),
                                                if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency5 == "bi-annually",
                                                        as.integer(barriers_and_facilitators.average_education_facility_fees5*2),
                                                        as.integer(barriers_and_facilitators.average_education_facility_fees5), NULL)))),
  
  
  
  i.edu_fees_6_annualy =if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency6 == "monthly",                        
                                as.integer(barriers_and_facilitators.average_education_facility_fees6*12),
                                if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency6 == "quarterly",
                                        as.integer(barriers_and_facilitators.average_education_facility_fees6*4),
                                        if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency6 == "weekly",
                                                as.integer(barriers_and_facilitators.average_education_facility_fees6*52),
                                                if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency6 == "bi-annually",
                                                        as.integer(barriers_and_facilitators.average_education_facility_fees6*2),
                                                        as.integer(barriers_and_facilitators.average_education_facility_fees6), NULL)))),
  
  i.edu_fees_7_annualy =if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency7 == "monthly",                        
                                as.integer(barriers_and_facilitators.average_education_facility_fees7*12),
                                if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency7 == "quarterly",
                                        as.integer(barriers_and_facilitators.average_education_facility_fees7*4),
                                        if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency7 == "weekly",
                                                as.integer(barriers_and_facilitators.average_education_facility_fees7*52),
                                                if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency7 == "bi-annually",
                                                        as.integer(barriers_and_facilitators.average_education_facility_fees7*2),
                                                        as.integer(barriers_and_facilitators.average_education_facility_fees7), NULL)))),
  
  i.edu_fees_8_annualy =if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency8 == "monthly",                        
                                as.integer(barriers_and_facilitators.average_education_facility_fees8*12),
                                if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency8 == "quarterly",
                                        as.integer(barriers_and_facilitators.average_education_facility_fees8*4),
                                        if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency8 == "weekly",
                                                as.integer(barriers_and_facilitators.average_education_facility_fees8*52),
                                                if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency8 == "bi-annually",
                                                        as.integer(barriers_and_facilitators.average_education_facility_fees8*2),
                                                        as.integer(barriers_and_facilitators.average_education_facility_fees8), NULL)))),
  
  i.edu_fees_9_annualy =if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency9 == "monthly",                        
                                as.integer(barriers_and_facilitators.average_education_facility_fees9*12),
                                if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency9 == "quarterly",
                                        as.integer(barriers_and_facilitators.average_education_facility_fees9*4),
                                        if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency9 == "weekly",
                                                as.integer(barriers_and_facilitators.average_education_facility_fees9*52),
                                                if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency9 == "bi-annually",
                                                        as.integer(barriers_and_facilitators.average_education_facility_fees9*2),
                                                        as.integer(barriers_and_facilitators.average_education_facility_fees9), NULL)))),
  
  i.edu_fees_10_annualy =if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency10 == "monthly",                        
                                 as.integer(barriers_and_facilitators.average_education_facility_fees10*12),
                                 if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency10 == "quarterly",
                                         as.integer(barriers_and_facilitators.average_education_facility_fees10*4),
                                         if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency10 == "weekly",
                                                 as.integer(barriers_and_facilitators.average_education_facility_fees10*52),
                                                 if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency10 == "bi-annually",
                                                         as.integer(barriers_and_facilitators.average_education_facility_fees10*2),
                                                         as.integer(barriers_and_facilitators.average_education_facility_fees10), NULL)))),
  
  i.edu_fees_11_annualy =if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency11 == "monthly",                        
                                 as.integer(barriers_and_facilitators.average_education_facility_fees11*12),
                                 if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency11 == "quarterly",
                                         as.integer(barriers_and_facilitators.average_education_facility_fees11*4),
                                         if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency11 == "weekly",
                                                 as.integer(barriers_and_facilitators.average_education_facility_fees11*52),
                                                 if_else(barriers_and_facilitators.education_facilities_charging_fees_by_frequency11 == "bi-annually",
                                                         as.integer(barriers_and_facilitators.average_education_facility_fees11*2),
                                                         as.integer(barriers_and_facilitators.average_education_facility_fees11), NULL)))),
)


write.csv(data_for_analysis,"inputs/01_data_collection/03_cleaned_data/data_for_analysis.csv")

