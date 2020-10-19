## Helper Functions
library(dplyr)


get_age_group <- function(df) {
  age <- as.integer(df['age']) 
  
  grp <- ifelse(age<=34,1,
                ifelse(age>34 & age<=44,2,
                       ifelse(age>=45 & age <=54,3,
                              ifelse(age>=55 & age<=64,4,5))))
  return(grp)
}

# -------------------------------------------
# CALCULATE BMI
# -------------------------------------------
calculate_bmi <- function(df) {
  weight <- as.integer(df['weight'])
  height <- as.integer(df['height'])
  
  height_in_m <- height/100
  bmi <- weight/(height_in_m*height_in_m)
  return(round(bmi,1))
  
}


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
  
  df_enr$age_group = apply(df_enr,1,get_age_group)
  
  df_enr$age_group <- factor(df_enr$age_group, levels = c(1,2,3,4,5),
                             labels = c("25-34","35-44","45-54","55-64","65+"))
  
  return(df_enr)
  
}

# -------------------------------------------
# GENERATES DEMOGRAPHIC INFORMATION
# -------------------------------------------

get_demographic_data <- function(df_enr, study_id) {
  
  df_selected_enr <- df_enr %>%
    filter(toupper(studyid)  == toupper(study_id) )
  
  
  sex <- df_selected_enr$gender
  age <- df_selected_enr$age
  clinicid <- df_selected_enr$clinicid
  year_art <- df_selected_enr$year_start_art
  dtg_start_date <- as.character(df_selected_enr$dtg_start_date)
  initials <- df_selected_enr$pinitials
  
  # some string manipulation
  return_string1 <- sprintf("<b>Clinic ID</b>: %s <br>
                <b>Patient Initials </b>: %s <br>
                <b>Sex</b>: %s <br> <br>
                 ", clinicid, initials, sex)
  
  return_string2 <- sprintf("
                 <b>Age</b>: %s <br> 
                 <b>Year Started ART</b>: %s <br>
                 <b>Date Started on DTG </b>: %s <br>", age, year_art, dtg_start_date)
  
  return(c(return_string1,return_string2))
}

# -------------------------------------------
# ENROLLMENT BASELINE INFORMATION
# -------------------------------------------

get_enrollment_details <- function(df_enr, study_id) {
  df_selected_enr <- df_enr %>%
    filter(toupper(studyid)  == toupper(study_id) )
  
  weight <- df_selected_enr$weight
  height <- df_selected_enr$height
  bp_systolic_1 <- as.character(df_selected_enr$bp_systolic_1)
  bp_diastolic_1 <- as.character(df_selected_enr$bp_diastolic_1)
  bp_systolic_2 <- ifelse(df_selected_enr$bp_systolic_2 %in% c(-6,-9), "N/A", as.character(df_selected_enr$bp_systolic_2))
  bp_diastolic_2 <- ifelse(df_selected_enr$bp_diastolic_2 %in% c(-6,-9), "N/A", as.character(df_selected_enr$bp_diastolic_2))
  bp_systolic_3 <- ifelse(df_selected_enr$bp_systolic_3 %in% c(-6,-9), "N/A", as.character(df_selected_enr$bp_systolic_3))
  bp_diastolic_3 <- ifelse(df_selected_enr$bp_diastolic_3 %in% c(-6,-9), "N/A", as.character(df_selected_enr$bp_diastolic_3))
  tca_fasting_labs <- ifelse(df_selected_enr$tca_fasting_labs== "01/01/2000", as.character(df_selected_enr$next_visit_date), as.character(df_selected_enr$tca_fasting_labs))
  
  return_string1 <- sprintf("
    <u><b>Vitals</b> </u> <br>
    <b>Height (cm)</b> : %s <br>
    <b>Weight (kg)</b> : %s <br>
    <b>Blood pressure Reading 1</b> : %s / %s <br>
    <b>Blood pressure Reading 2</b> : %s / %s <br>
    <b>Blood pressure Reading 3</b> : %s / %s <br>
    <b> Date scheduled Next Visit </b> : %s <br>
                            ", height,weight , bp_systolic_1,bp_diastolic_1,bp_systolic_2,bp_diastolic_2,
                            bp_systolic_3,bp_diastolic_3,tca_fasting_labs)
  
  
  fasting <- ifelse(df_selected_enr$fasting == 1, "Yes", "No")
  f_glucose <- ifelse(df_selected_enr$fasting == 1, as.character(df_selected_enr$f_glucose), "N/A")
  f_total_chol <- ifelse(df_selected_enr$fasting == 1, as.character(df_selected_enr$f_total_chol), "N/A")
  f_hdl_chol <- ifelse(df_selected_enr$fasting == 1, as.character(df_selected_enr$f_hdl_chol), "N/A")
  f_trig <- ifelse(df_selected_enr$fasting == 1, as.character(df_selected_enr$f_trig), "N/A")
  hgb <- ifelse(df_selected_enr$hgb %in% c(-9,-6), "Not Done",as.character(df_selected_enr$hgb))
  
  return_string2 <- sprintf("
    <u><b>Fasting Labs</b> </u> <br>
    <b>Is participant fasting?</b> : %s <br>
    <b>Fasting glucose (mmol/L)</b> : %s <br>
    <b>Fasting total cholesterol (mmol/L)</b> : %s <br>
    <b>Fasting HDL cholesterol (mmol/L)</b> : %s <br>
    <b>Fasting Triglycerides (mmol/L)</b> : %s <br>
    <b>Hgb A1c (&#37;)</b> : %s <br>
                            ", fasting, f_glucose, f_total_chol,f_hdl_chol,f_trig,hgb)
  
  
  return(c(return_string1,return_string2))
}

# -------------------------------------------
# ART AND MEDICATION HISTORY INFORMATION AT ENROLLMENT
# -------------------------------------------

get_hiv_history <- function(df_enr, study_id) {
  
  df_select_enr <- df_enr %>%
    filter(toupper(studyid)  == toupper(study_id) )
  
  hiv_year <- df_select_enr$year_diagnosed
  art_year <- df_select_enr$year_start_art
  stopped_meds <- ifelse(df_select_enr$ever_stopped_meds == 1, "Yes", "No") 
  duration_stopped <- ifelse(df_select_enr$period_stopped == -9,"N/A",df_select_enr$period_stopped)
  weight_change <- ifelse(df_select_enr$weight_change_past_month == 0,"No Change",
                          ifelse(df_select_enr$weight_change_past_month == 1, "I think I lost weight","I think I gained weight"))
  symp_thirsty <- ifelse(df_select_enr$symp_thirsty == 1, "I felt more thirsty than usual", "")
  symp_drink_water <- ifelse(df_select_enr$symp_drink_water == 1, "I needed to drink much more water than usual", "")
  symp_urinate <- ifelse(df_select_enr$symp_urinate == 1, "I noticed I was urinating much more frequently or a larger amount than usual", "")
  symp_none <- ifelse(df_select_enr$symp_none == 1, "Yes", "No")
  symptoms <- "None"
  
  if (symp_none == "No") {
    symptoms <- paste(symp_thirsty,symp_drink_water,symp_urinate,sep = ";")
  }
  
  return(sprintf(
    "<u><b>HIV History</b></u> <br>
    <b>Year Diagnosed with HIV</b>: %s <br>
    <b>Year Started on ART</b>: %s <br>
    <b>Ever Stopped Taking HIV Meds</b>: %s <br>
    <b>Duration Stopped</b>: %s Month(s) <br>
    <u><b>Symptoms</b></u> <br>
    <b>Any weight changes in the past month</b>: %s <br>
    <b>Symptoms experienced in the past month</b>: %s <br>
    ", hiv_year, art_year, stopped_meds, duration_stopped, weight_change, symptoms
  ))
}

get_medication_history <- function(df_enr, study_id) {
  df_select_enr <- df_enr %>%
    filter(toupper(studyid)  == toupper(study_id) )
  
  missed_doses <- df_select_enr$n_missed_doses
  diab <- ifelse(df_select_enr$ever_had_diab==1, "Yes", "No")
  diab_year <- ifelse(df_select_enr$year_diagnosed_diab==-9,"N/A",df_select_enr$year_diagnosed_diab)
  taken_diab_meds <- ifelse(df_select_enr$ever_taken_diab_meds==1,"Yes","No")
  
  hyp <- ifelse(df_select_enr$ever_had_hyp==1,"Yes","No")
  hyp_year <- ifelse(df_select_enr$year_diagnosed_hyp==-9, "N/A",df_select_enr$year_diagnosed_hyp)
  taken_hyp_meds <- ifelse(df_select_enr$ever_taken_hyp_meds==1,"Yes","No")
  
  cholestrol <- ifelse(df_select_enr$ever_had_chol==1, "Yes", "No")
  chol_year <- ifelse(df_select_enr$year_diagnosed_chol==-9, "N/A",df_select_enr$year_diagnosed_chol)
  taken_chol_meds <- ifelse(df_select_enr$ever_taken_chol_meds==1,"Yes","No")
  
  history_1 <- sprintf("
            <b><u>ART Adherence </u> </b> <br>
            <b> Days Missed HIV Medication in the past 7 days </b> : %s <br>
            <b><u>Diabetes History </u> </b> <br>
            <b> Ever diagnosed with Diabetes </b> : %s <br>
            <b> Year diagnosed with Diabetes </b> : %s <br>
            <b> Ever taken meds for Diabetes </b> : %s <br>
                 ", missed_doses,diab,diab_year, taken_diab_meds)
  
  history_2 <- sprintf("
            <b><u>Hypertension History </u> </b> <br>
            <b> Ever diagnosed with Hypertension </b> : %s <br>
            <b> Year diagnosed with Hypertension </b> : %s <br>
            <b> Ever taken meds for Hypertension </b> : %s <br>
                 ", hyp,hyp_year, taken_hyp_meds)
  
  history_3 <- sprintf("
            <b><u>Cholestrol/dyslipidemia History </u> </b> <br>
            <b> Ever diagnosed with high Cholestrol </b> : %s <br>
            <b> Year diagnosed with high Cholestrol </b> : %s <br>
            <b> Ever taken meds for high Cholestrol </b> : %s <br>
                 ", cholestrol,chol_year, taken_chol_meds)
  
  return(c(history_1, history_2, history_3))
}


# -------------------------------------------
# FOOD SECURITY INFORMATION AT ENROLLMENT
# -------------------------------------------
get_food_security <- function(df_enr, study_id) {
  df_selected_enr <- df_enr %>%
    filter(toupper(studyid)  == toupper(study_id) )
  
  no_enough_food <- ifelse(df_selected_enr$no_enough_food == 1, "Yes", "No")
  no_preferred_food <- ifelse(df_selected_enr$no_preferred_food == 1, "Yes", "No")
  limited_food_variety <- ifelse(df_selected_enr$limited_food_variety == 1, "Yes", "No")
  no_other_food <- ifelse(df_selected_enr$no_other_food == 1, "Yes", "No")
  smaller_meal <- ifelse(df_selected_enr$smaller_meal, "Yes", "No")
  
  food_1 <- sprintf("<b>Not have enough food? </b>: %s<br>
    <b>Not able to eat the kinds of foods you preferred? </b>: %s<br>
    <b>Eat a limited variety of foods? </b>: %s<br>
    <b>Eat some foods that you really did not want to eat? </b>: %s<br>
    <b>Eat a smaller meal than you felt you needed? </b>: %s<br>
                    ", no_enough_food,no_preferred_food,limited_food_variety,no_other_food,smaller_meal)
  
  fewer_meals <- ifelse(df_selected_enr$fewer_meals==1, "Yes", "No")
  no_food <- ifelse(df_selected_enr$no_food == 1, "Yes", "No")
  slept_hungry <- ifelse(df_selected_enr$slept_hungry == 1, "Yes", "No")
  hungry_day_night <- ifelse(df_selected_enr$hungry_day_night == 1, "Yes", "No")
  
  food_2 <- sprintf("<b>Eat fewer meals in a day? </b>: %s <br>
    <b>No food to eat of any kind in your household? </b>: %s <br>
    <b>Sleep hungry at night because of food shortages in the house?</b> : %s <br>
    <b>Went hungry for a whole day and night?</b> : %s <br>
                    ", fewer_meals,no_food,slept_hungry,hungry_day_night)
  
  return(c(food_1, food_2))
}

# --------------------------------------------
# GENERATES REPRODUCTIVE HISTORY AT ENROLLMENT
# --------------------------------------------
get_reproductive_history <- function(df_enr, study_id) {
  df_selected_enr <- df_enr %>%
    filter(toupper(studyid)  == toupper(study_id) )
  
  return_string <- sprintf("<h3>No Reproductive information for Male gender</h3>")
  
  if (df_selected_enr$gender == "Female" ) {
    use_fp <- ifelse(df_selected_enr$use_fp == 1, "Yes", "No")
    fp_method <-  ifelse(df_selected_enr$fp_method == 8, df_selected_enr$other_fp_method,
                         ifelse(df_selected_enr$fp_method==-9,"N/A",df_selected_enr$fp_method)) 
    prev_pregnant <- ifelse(df_selected_enr$prev_pregnant == 1, "Yes", "No")
    gravida <- ifelse(df_selected_enr$gravida == -9, "N/A", df_selected_enr$gravida)
    parity <- ifelse(df_selected_enr$parity == -9, "N/A", df_selected_enr$parity)
    
    return_string <- sprintf("
        <b>Currently using contraception (e.g. Family Planning)?</b> : %s <br>
        <b>what kind? If current contraception</b> : %s <br>
        <b>Have you been pregnant previously?</b> : %s <br>
        <b> How many prior pregnancies? </b> : %s <br>
        <b> How many prior deliveries? </b> : %s <br>", use_fp,fp_method,prev_pregnant,gravida,parity)
  } 
  
  return(return_string)
  
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

create_timeline_df <- function(df_sc, df_en, df_fu, df_wd, df_tr, study_id) {
  start_vec <- c()
  content_vec <- c()
  end_vec <- c()
  
  # ENROLLMENT
  df_enr_filtered = df_en %>%
    filter(toupper(studyid) == toupper(study_id))
  
  # Screening
  df_sc_filtered = df_sc %>%
    filter(toupper(studyid) == toupper(study_id))
  
  # Follow-up
  df_fu_filtered = df_fu %>%
    filter(toupper(studyid) == toupper(study_id))
  
  # Withdrawal
  df_wd_filtered = df_wd %>%
    filter(toupper(studyid) == toupper(study_id))
  
  # Tracking
  df_tr_filtered = df_tr %>%
    filter(toupper(studyid) == toupper(study_id))
  
  
  # Add screening details to the timeline vector
  if (length(df_sc_filtered$studyid) != 0) {
    
    start_vec = c(start_vec, as.character(df_sc_filtered$screendate_0))
    content_vec = c(content_vec, '<b>Screening</b>')
    end_vec = c(end_vec, NA)
    
  }
  
  # Add enrollment details to the timeline vector
  if (length(df_enr_filtered$studyid) != 0) {
  
    start_vec = c(start_vec, as.character(df_enr_filtered$enrdate_0))
    content_vec = c(content_vec, '<b>Enrollment</b>')
    end_vec = c(end_vec, NA)
    
  }
  
  # Add withdrawal details to the timeline vector
  if (length(df_wd_filtered$studyid) != 0) {
    
    if(df_wd_filtered$withdrawmove == 2) {
      start_vec = c(start_vec, as.character(df_wd_filtered$tdate_0))
      content_vec = c(content_vec, '<b>Moved</b>')
      end_vec = c(end_vec, NA)
      
    } else if(df_wd_filtered$withdrawmove == 1) {
      start_vec = c(start_vec, as.character(df_wd_filtered$tdate_0))
      content_vec = c(content_vec, '<b>Withdrawal</b>')
      end_vec = c(end_vec, NA)
    }
    
    
  }
  
  
  # Add Followup details to the timeline vector
  if (length(df_fu_filtered$studyid) != 0) {
    
    for (i in 1:nrow(df_fu_filtered)) {
      start_vec = c(start_vec, as.character(df_fu_filtered[i,'vdate_0']))
      content_vec = c(content_vec, '<b>Followup-visit</b>')
      end_vec = c(end_vec, NA) 
    }
    
  }
  
  # Add Tracking details to the timeline vector
  if (length(df_tr_filtered$studyid) != 0) {
    
    for (i in 1:nrow(df_tr_filtered)) {
      start_vec = c(start_vec, as.character(df_tr_filtered[i,'tdate_0']))
      content_vec = c(content_vec, '<b>Tracking</b>')
      end_vec = c(end_vec, NA) 
    }
    
  }
  
  data <- data.frame(
    id      = seq(1: length(start_vec)),
    content = content_vec,
    start   = start_vec,
    end     = end_vec
  )
  #print(data)
  return(data)
}


# -------------------------------------------
# CREATES A LIST OF VISITS FROM TIMELINE
# -------------------------------------------
create_visit_list <- function(df_timeline) {
  df_temp <- df_timeline
  df_temp <- df_temp %>%
    mutate(visit_name = ifelse(grepl("Screening", content),"Screening",
                               ifelse(grepl("Enrollment", content),"Enrollment",
                                      ifelse(grepl("Tracking", content),"Tracking",
                                             ifelse(grepl("Withdrawal", content),"Withdrawal",
                                                    ifelse(grepl("Followup-visit", content),"Followup",
                                                           ifelse(grepl("Moved", content),"Moved","NA")))))))
  
  df_temp$visit_name = paste(df_temp$visit_name, as.character(as.Date(df_temp$start)), sep = ' - ')
  df_temp = arrange(df_temp, desc(start), desc(id))
  
  # Return list of visit names combined with dates
  choices = setNames(df_temp$id, as.character(df_temp$visit_name))
  
  return(list("choices" = choices))
  
}

# --------------------------------------------------------------------
# GENERATE DATASET TO SELECT DATA TO BE DISPLAYED IN ON MAIN DASHBOARD
# --------------------------------------------------------------------
get_dashboard_data <- function(df, main_breakdown, sub_breakdown) {
  dt <- df
  if( main_breakdown == "Gender") {
    if(sub_breakdown != "All") {
      dt <- df %>%
        filter(gender == sub_breakdown)
    }
    
  } else if (main_breakdown == "Age-group") {
    if( sub_breakdown != "All") {
      dt <- df %>%
        filter(age_group == sub_breakdown)
    }
    
  }
  return(dt)
}
