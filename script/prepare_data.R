# Helper function to get data from the access db and create csv files that will used by the 
# Main dashboard

library(RODBC) #library to get data from SQL server/MsAccess DB

df_file_path =  "C:/DTGSTudy/MSAccessDB/dtgstudy.mdb"

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

## Write to the csv files
writeCsvFile(dfs_all$enroll,"enrollment")
writeCsvFile(dfs_all$screening,"screening")
writeCsvFile(dfs_all$fup,"followup")
writeCsvFile(dfs_all$track,"tracking")
writeCsvFile(dfs_all$withdraw,"withdrawal")
