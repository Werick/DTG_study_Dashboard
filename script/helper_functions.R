## Helper Functions
library(dplyr)

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