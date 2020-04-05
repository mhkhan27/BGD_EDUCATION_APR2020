# common ------------------------------------------------------------------

day_to_run<- Sys.Date()

write_csv_output <- c("yes","no")[1]

# COPY TO DROPBOX --------------------------------------------------------------

rmarkdown::render("Edu_facility_Daily_Monitoring_Report.Rmd")

file_location <- "Edu_facility_Daily_Monitoring_Report.html"
copy_to_drop <- "C:\\Users\\MEHEDI\\Dropbox\\REACH_BGD\\REACH\\Ongoing\\70iAMD 70EAD - Host Community Education/04_daily_monitoring/01_facility_assessment/02_daily_reports/"
file.copy(file_location,paste0(copy_to_drop,str_replace_all(day_to_run,"-","_"),"_Education_facility_Daily_Monitoring_Report.html"),overwrite = T)
