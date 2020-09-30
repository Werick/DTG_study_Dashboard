## Helper Functions
library(dplyr)

# -------------------------------------------
# GENERATE AGE GROUP
# -------------------------------------------
set_age_group <- function(df_enr, df_scr) {
  df_age <- df_scr %>%
    filter(studyid %in% df_enr$studyid) %>%
    select(studyid,age)
 
  df_enr <- df_enr %>%
    inner_join(df_age, by="studyid") 
  
  df_enr$age <- as.integer(df_enr$age)
  
  df_enr <- df_enr %>%
    mutate(age_group = ifelse(age<=34,1,
                              ifelse(age>34 & age<=44,2,
                                     ifelse(age>=45 & age <=54,3,
                                            age>=55 & age<=64,4,5))))
  
  df_enr$age_group <- factor(df_enr$age_group, levels = c(1,2,3,4,5),
                             labels = c("25-34","35-44","45-54","55-64","65+"))
  
  return(df_enr)
  
}

# -------------------------------------------
# GENERATES DEMOGRAPHIC INFORMATION
# -------------------------------------------

get_demographic_data <- function(df_enr, df_scr, study_id) {
  
  df_select_enr <- df_enr %>%
    filter(toupper(studyid)  == toupper(study_id) )
  
  df_select_scr <- df_scr %>%
    filter(toupper(studyid) == toupper(study_id))
  
  sex <- df_select_enr$gender
  age <- df_select_enr$age
  clinicid <- df_select_enr$clinicid
  year_art <- df_select_enr$year_start_art
  regimen <- df_select_scr$cur_regimen
  
  # some string manipulation
  return(sprintf("<b>Clinic ID</b>: %s <br>
                 <b>Sex</b>: %s <br> 
                 <b>Age</b>: %s <br> 
                 <b>Year Started ART</b>: %s <br>
                 <b>Current Regimen </b>: %s <br> <br> <br>", clinicid, sex, age, year_art,regimen))
}

# -------------------------------------------
# ENROLLMENT INFORMATION
# -------------------------------------------

get_enrollment_details <- function(df_enr, study_id) {
  df_select_enr <- df_enr %>%
    filter(toupper(studyid)  == toupper(study_id) )
  
  
}


# --------------------------------------------
# TIME VISUALIZATION DATA FRAME
# --------------------------------------------
# Create a time visualization data frame with the following structure
# --------------------------------------------

# data <- data.frame(
#   id      = 1:4,
#   content = c("Item one", "Item two",
#               "Ranged item", "Item four"),
#   start   = c("2016-01-10", "2016-01-11",
#               "2016-01-20", "2016-02-14 15:00:00"),
#   end     = c(NA, NA, "2016-02-04", NA)
# )

create_timeline_df <- function(df_enr, study_id) {
  start_vec <- c()
  content_vec <- c()
  end_vec <- c()
  
  # ENROLLMENT
  df_enr_filtered = df_enr %>%
    filter(toupper(df_enr$studyid) == toupper(study_id))
  
  if (length(df_enr_filtered$studyid) != 0) {
  
    start_vec = c(start_vec, as.character(df_enr_filtered$enrdate_0))
    content_vec = c(content_vec, '<b>Enrolled in DTG Study</b>')
    end_vec = c(end_vec, NA)
    
  }
  
  data <- data.frame(
    id      = seq(1: length(start_vec)),
    content = content_vec,
    start   = start_vec,
    end     = end_vec
  )
  
  return(data)
}