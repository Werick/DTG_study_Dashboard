# Helper function to get data from the access db and create csv files that will used by the 
# Main dashboard

library(RODBC) #library to get data from SQL server/MsAccess DB
library(dplyr)

df_file_path =  "C:/DTGSTudy/MSAccessDB/dtgstudy.mdb"
df_file_path_lab =  "C:/DTGSTudy/MSAccessDB/DTG Lab database.accdb"

loadData <- function(file_path) {
  con = odbcConnectAccess2007(file_path)
  df_enrollment = sqlFetch(con, 'enrollment', stringsAsFactors=FALSE)
  df_track = sqlFetch(con, 'tracking', stringsAsFactors=FALSE)
  df_withdraw = sqlFetch(con, 'withdrawal', stringsAsFactors=FALSE)
  df_followup = sqlFetch(con, 'followup', stringsAsFactors=FALSE)
  df_screening = sqlFetch(con, 'screening', stringsAsFactors=FALSE)
  
  
  to_return = list("enroll" = df_enrollment,
                   "fup" = df_followup, 
                   "screening" = df_screening,
                   "track" = df_track,
                   "withdraw" = df_withdraw)
  odbcCloseAll()
  return(to_return)  
}

writeCsvFile <- function(df, fname) {
  
  file_name<-paste("./data/",fname, ".csv", sep="")
  write.csv(df, file_name, row.names = FALSE)
}

dfs_all <- loadData(df_file_path)

con_lab <- odbcConnectAccess2007(df_file_path_lab)
df_lab <- sqlFetch(con_lab, "dtgtb", stringsAsFactors=FALSE)

## Write to the csv files
writeCsvFile(dfs_all$enroll,"enrollment")
writeCsvFile(dfs_all$screening,"screening")
writeCsvFile(dfs_all$fup,"followup")
writeCsvFile(dfs_all$track,"tracking")
writeCsvFile(dfs_all$withdraw,"withdrawal")
writeCsvFile(df_lab,"lab")

#------------------------------------------------------------------------------
# TEST CODE
#------------------------------------------------------------------------------
df_lab <- df_lab %>%
  filter(studyvisit == 1) %>%
  select(studyid1, studyvisit, daterequested, fastbsvalue, fasttotcholestval, fasthdlcholestval, fasttryglcrdsval, haemoglbna1cval)

df_enr <- dfs_all$enroll %>%
  select(studyid,pinitials, enrdate, f_glucose,f_total_chol, f_hdl_chol, f_trig, hgb )

df_lab_qc <- merge(df_enr,df_lab, by.x='studyid', by.y='studyid1')

head(df_lab_qc)
df_lab_qc <- df_lab_qc %>% 
  rename(fasting_bloodsugar_clinc = f_glucose,
         fasting_bloodsugar_lab = fastbsvalue,
         hemoglobinA1C_clinic = hgb,
         hemoglobinA1C_lab = haemoglbna1cval,
         total_cholesterol_clinic = f_total_chol,
         HDL_cholesterol_clinic = f_hdl_chol,
         Triglycerides_clinic = f_trig,
         total_cholesterol_lab = fasttotcholestval,
         HDL_cholesterol_lab = fasthdlcholestval,
         Triglycerides_lab = fasttryglcrdsval)

head(df_lab_qc)

df_lab_qc <- df_lab_qc %>%
  filter(as.double(hemoglobinA1C_clinic) != as.double(hemoglobinA1C_lab) |
           as.double(total_cholesterol_clinic) != as.double(total_cholesterol_lab) |
           as.double(HDL_cholesterol_clinic) != as.double(HDL_cholesterol_lab) |
           as.double(Triglycerides_clinic) != as.double(Triglycerides_lab))

head(df_lab_qc)

# order columns
col_order <- c("studyid", "enrdate","daterequested", "hemoglobinA1C_clinic", "hemoglobinA1C_lab", "total_cholesterol_clinic",
               "total_cholesterol_lab", "HDL_cholesterol_clinic", "HDL_cholesterol_lab","Triglycerides_clinic",
               "Triglycerides_lab", "fasting_bloodsugar_clinc", "fasting_bloodsugar_lab")

df_lab_qc <- df_lab_qc[,col_order]
head(df_lab_qc)
