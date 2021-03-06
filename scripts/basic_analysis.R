rm(list=ls())

library(dplyr)
library(butteR)
library(tidyverse)
library(readr)
library(stringr)
library(srvyr)
library(survey)
library(readxl)

koboquest <- list.files("scrap/koboquest/R",full.names = T)

for (i in koboquest ){
  source(i)
}


# basic_analysis  ---------------------------------------------------------

data_for_analysis <- read.csv("inputs/01_data_collection/03_cleaned_data/data_for_analysis.csv",stringsAsFactors = F,
                 na.strings = c(""," ", "n/a",NA))

analysis_sheet <- read.csv("inputs/99_other/Education Analysis Sheet.csv",stringsAsFactors = F, 
                           na.strings = c(""," ", "n/a",NA))

assess_survey<- readxl::read_xls("inputs/03_tool/Facility_Assessment_final_tool.xls",sheet = "survey")
assess_choices<-readxl::read_xls("inputs/03_tool/Facility_Assessment_final_tool.xls",sheet = "choices")
assessment<-load_questionnaire(data = data_for_analysis,questions = assess_survey,
                               choices = assess_choices,choices.label.column.to.use = "label::english")

# Select multiple colnames ------------------------------------------------

colnames_multiple_ans_df <-analysis_sheet %>% filter(Mutiple_choice == 1) 
colnames_multiple_ans_stw<- colnames_multiple_ans_df$Main.variable.of.interest %>% as.character()%>% dput()

all_mutiple <- character()

for (i in colnames_multiple_ans_stw) {
  all_mutiple1<-data_for_analysis %>% select(starts_with(i)) %>% colnames() 
  all_mutiple<- c(all_mutiple,all_mutiple1)
}

# select single colnames --------------------------------------------------
colnames_single_ans_df <-analysis_sheet %>% filter(Mutiple_choice == 0) 
colnames_single_ans_stw<- colnames_single_ans_df$Main.variable.of.interest %>% as.character()%>% dput()


# recoding_cols -----------------------------------------------------------

recoding_cols <- data_for_analysis %>% dplyr::select(starts_with("i.")) %>% colnames()

# all columns -------------------------------------------------------------

all_coloumn_names <- c(colnames_single_ans_stw,all_mutiple,recoding_cols,"upazilla","facility_type") 


# analysis ----------------------------------------------------------------
df_strata <- "upazilla"
dfsvy<-svydesign(ids = ~1,strata = formula(paste0("~",df_strata)),data = data_for_analysis)
dfsvy$variables<- butteR::questionnaire_factorize_categorical(data = dfsvy$variables,questionnaire = assessment,return_full_data = T)

is_not_empty<-function(x){ all(is.na(x))==FALSE}
cols_to_analyze<-data_for_analysis[all_coloumn_names] %>% select(-starts_with("Other"), -ends_with(".other")) %>%
  select_if(.,is_not_empty) %>% colnames() 


# factorization -----------------------------------------------------------

dfsvy$variables$barriers_and_facilitators.education_facilities_charging_fees_by_frequency3<- forcats::fct_expand(
  dfsvy$variables$barriers_and_facilitators.education_facilities_charging_fees_by_frequency3,c( "monthly", "error"))

dfsvy$variables$barriers_and_facilitators.education_facilities_charging_fees_by_frequency4<- forcats::fct_expand(
  dfsvy$variables$barriers_and_facilitators.education_facilities_charging_fees_by_frequency4,c( "annually", "error"))                                                              

dfsvy$variables$barriers_and_facilitators.education_facilities_charging_fees_by_frequency7<- forcats::fct_expand(
  dfsvy$variables$barriers_and_facilitators.education_facilities_charging_fees_by_frequency7, c( "bi-annually", "error"))                                                              

dfsvy$variables$inclusive_and_protective_environment.frequency_of_extra.curricular_actvities6<- forcats::fct_expand(
  dfsvy$variables$inclusive_and_protective_environment.frequency_of_extra.curricular_actvities6, c( "other", "error"))                                                              


# butteR analysis ----------------------------------------------------------

basic_analysis_overall<-butteR::mean_proportion_table(design = dfsvy,list_of_variables = cols_to_analyze)
basic_analysis_by_edu_level<-butteR::mean_proportion_table(design = dfsvy,list_of_variables = cols_to_analyze,aggregation_level = "facility_type")


# write_csv ---------------------------------------------------------------

output_location <- "output/03_butter_basic_analysis/"
write.csv(basic_analysis_overall,paste0(output_location,str_replace_all(Sys.Date(),"-","_"),"_basic_analysis_overall.csv"))
write.csv(basic_analysis_by_edu_level,paste0(output_location,str_replace_all(Sys.Date(),"-","_"),"_basic_analysis_by_edu_level.csv"))
