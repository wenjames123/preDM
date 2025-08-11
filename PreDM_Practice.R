library(tidyverse)
library(meta)

preDM <- read_csv("data/raw/Practice_Data.csv")

#================================================
#STEP 1 - SEPARATE STUDIES BY DEFINITION
#STEP 2 - SEPARATE BY DUPLICATE WITHIN DEFINITION
#================================================

#------------
#Definition 0
#------------
  
definition_0 <- preDM %>% 
  filter(preDM_def ==0)

definition_0_single <- definition_0 %>% 
  group_by(Author) %>% 
  filter(n()==1) %>% 
  ungroup() 

definition_0_multi <- definition_0 %>% 
  group_by(Author) %>% 
  filter(n()>1) %>% 
  ungroup()

#------------
#Definition 1
#------------
definition_1 <- preDM %>% 
  filter(preDM_def ==1)

definition_1_single <- preDM %>% 
  filter(preDM_def ==1) %>% 
  group_by(Author) %>% 
  filter(n()==1) %>% 
  ungroup()

definition_1_multi <- preDM %>% 
  filter(preDM_def ==1) %>% 
  group_by(Author) %>% 
  filter(n()>1) %>% 
  ungroup()

#------------
#Definition 2
#------------
definition_2 <- preDM %>% 
  filter(preDM_def ==2)

definition_2_single <- preDM %>% 
  filter(preDM_def ==2) %>% 
  group_by(Author) %>% 
  filter(n()==1) %>% 
  ungroup()

definition_2_multi <- preDM %>% 
  filter(preDM_def ==2) %>% 
  group_by(Author) %>% 
  filter(n()>1) %>% 
  ungroup()

#------------
#Definition 3
#------------
definition_3 <- preDM %>% 
  filter(preDM_def ==3)

definition_3_single <- preDM %>% 
  filter(preDM_def ==3) %>% 
  group_by(Author) %>% 
  filter(n()==1) %>% 
  ungroup()

definition_3_multi <- preDM %>% 
  filter(preDM_def ==3) %>% 
  group_by(Author) %>% 
  filter(n()>1) %>% 
  ungroup()

#------------
#Definition 4
#------------
definition_4 <- preDM %>% 
  filter(preDM_def ==4)

definition_4_single <- preDM %>% 
  filter(preDM_def ==4) %>% 
  group_by(Author) %>% 
  filter(n()==1) %>% 
  ungroup()

definition_4_multi <- preDM %>% 
  filter(preDM_def ==4)%>% 
  group_by(Author) %>% 
  filter(n()>1) %>% 
  ungroup()

#------------
#Definition 5
#------------
definition_5 <- preDM %>% 
  filter(preDM_def ==5)

definition_5_single <- preDM %>% 
  filter(preDM_def ==5) %>% 
  group_by(Author) %>% 
  filter(n()==1) %>% 
  ungroup()

definition_5_multi <- preDM %>% 
  filter(preDM_def ==5) %>% 
  group_by(Author) %>% 
  filter(n()>1) %>% 
  ungroup()

#===================================================
#STEP 3 - CALCULATE TOTAL SAMPLE SIZE PER DEFINITION
#===================================================

#checks the 'duplicate' column. if NOT duplicate, adds to definition_X_unique. Then sum n_participant column of the studies

definition_0_unique <- definition_0[!duplicated(definition_0$duplicate),]
n_samplesize_0 <- sum(definition_0_unique$n_participants)

definition_1_unique <- definition_1[!duplicated(definition_1$duplicate),]
n_samplesize_1 <- sum(definition_1_unique$n_participants)

definition_2_unique <- definition_2[!duplicated(definition_2$duplicate),]
n_samplesize_2 <- sum(definition_2_unique$n_participants)

definition_3_unique <- definition_3[!duplicated(definition_3$duplicate),]
n_samplesize_3 <- sum(definition_3_unique$n_participants)

definition_4_unique <- definition_4[!duplicated(definition_4$duplicate),]
n_samplesize_4 <- sum(definition_4_unique$n_participants)

definition_5_unique <- definition_5[!duplicated(definition_5$duplicate),]
n_samplesize_5 <- sum(definition_5_unique$n_participants)


#=======================================================================
#STEP 4 - DETERMINE DEFINITION PRIORITY & GENERATE NEW 'PRIORITY' COLUMN
#=======================================================================

#PRIORITY MAPPING: 
#Priority order: 5>0>2>1>3>4
# 5 -> 36 obs
# 0 -> 26 obs
# 2 -> 10 obs
# 1 -> 8 obs
# 3 -> 6 obs
# 4 -> 1 ob

priority_map <- c("5" = 1, "0" = 2, "2" = 3, "1" = 4, "3" = 5, "4" = 6)

#create new column def_priority in preDM. 'as.character(preDM_def' converts each value in preDM_def to a string. Then compares the preDM_def value to corresponding value in priority_map
#That number is added to the new def_priority column --> e.g. priority_map["5"] returns 1. basically repeats this for every single row
preDM <- preDM %>%
  mutate(def_priority = priority_map[as.character(preDM_def)]) %>% 
  relocate(def_priority, .after = preDM_def)


#=========================================================================================================================================
#STEP 5 - SELECT HIGHEST PRIORITY DEFINITION PER STUDY, AND THEN SEPARATE INTO STUDIES WITH ONE SUBGROUP/MULTIPLE SUBGROUPS PER DEFINITION
#=========================================================================================================================================

#For each duplicate group, select the highest-priority definition (min value of def_priority), but keep all rows from that definition (e.g., all subgroups).
preDM_priority_filtered <- preDM %>% 
  group_by(duplicate) %>% 
  filter(def_priority == min(def_priority)) %>% 
  ungroup()

#filter single & multi groups
preDM_priority_filtered_single <- preDM_priority_filtered %>% 
  group_by(duplicate) %>% 
  filter(n()==1) %>% 
  ungroup()

preDM_priority_filtered_multi <- preDM_priority_filtered %>% 
  group_by(duplicate) %>% 
  filter(n()>1) %>% 
  ungroup()

#==================================================
#STEP 6 - GENERATE COUNTS FOR FREQUENCY OF OUTCOMES
#===================================================

#Checks the 'duplicate' column.  For rows where duplicate == FALSE, add it to the unique table 
#(first occurence of each duplicate value is duplicate == FALSE, subsequent ones are dupl == TRUE, thus only takes 1st occurence)
preDM_priority_filtered_unique <- preDM_priority_filtered[!duplicated(preDM_priority_filtered$duplicate),]

#Create vector of column names for the outcomes
outcome_columns <- c(
  "mort_report",
  "cardiomort_report",
  "CVD_report",
  "CHD_report",
  "stroke_report",
  "HF_report",
  "CKD_report"
)

#Reads each row of the outcome column in preDM_priority_filtered_unique. If the value is 1,  adds it to the sum. Does this for each value of the vector.  na.rm = TRUE ignores 'NA' values.
#Resulting vector: c(mort_report = 29, cardiomort_report = 19, CVD_report = 33, ...)
outcome_counts <- colSums(preDM_priority_filtered_unique[outcome_columns] == 1, na.rm = TRUE)

#Create new data frame. Extracts names from the outcome_columns vector, and then corresponding counts. 
summary_table <- data.frame(
  Outcome_Type = names(outcome_counts),
  Count = as.integer(outcome_counts)
)


#==================================================
#STEP 7 - META ANALYSIS - MULTIPLE SUBGROUP STUDIES  
#===================================================

preDM_priority_filtered_multi_forMETA <- preDM_priority_filtered_multi %>% 
  select(Author, duplicate, def_priority, HR_mort_F:UCI_mort_M, HR_cardiomort_F:UCI_cardiomort_M, HR_stroke_F:UCI_stroke_M, HR_iscstroke_F:UCI_iscstroke_M,HR_hemstroke_F:UCI_hemstroke_M,HR_CVD_F:UCI_CVD_M,HR_CHD_F:UCI_CHD_M,HR_CKD_F:UCI_CKD_M,HR_HF_F:UCI_HF_M)

preDM_priority_filtered_multi_forMETA<-as.data.frame(preDM_priority_filtered_multi_forMETA)

#---------
#MORTALITY
#---------
#Mortality, women
preDM_priority_filtered_multi_forMETA <- preDM_priority_filtered_multi_forMETA %>% 
  mutate(
    log_HR_mort_F = log(HR_mort_F),
    se_log_HR_mort_F = (log(UCI_mort_F) - log(LCI_mort_F)) / (2*1.96)
  ) %>% 
  relocate(log_HR_mort_F, .after = HR_mort_F) %>% 
  relocate(se_log_HR_mort_F, .after = UCI_mort_F)

mort_F_metaresult_multi <- metagen(
  studlab = Author,
  sort = preDM_priority_filtered_multi_forMETA$duplicate,
  TE = log_HR_mort_F,
  seTE = se_log_HR_mort_F,
  data = preDM_priority_filtered_multi_forMETA,
  backtransf = T,
  sm="HR",
  method.tau = "DL",
  subgroup = preDM_priority_filtered_multi_forMETA$duplicate
)

#Mortality, men
preDM_priority_filtered_multi_forMETA <- preDM_priority_filtered_multi_forMETA %>% 
  mutate(
    log_HR_mort_M = log(HR_mort_M),
    se_log_HR_mort_M = (log(UCI_mort_M) - log(LCI_mort_M)) / (2*1.96)
  ) %>% 
  relocate(log_HR_mort_M, .after = HR_mort_M) %>% 
  relocate(se_log_HR_mort_M, .after = UCI_mort_M)

mort_M_metaresult_multi <- metagen(
  studlab = Author,
  sort = preDM_priority_filtered_multi_forMETA$duplicate,
  TE = log_HR_mort_M,
  seTE = se_log_HR_mort_M,
  data = preDM_priority_filtered_multi_forMETA,
  backtransf = T,
  sm="HR",
  method.tau = "DL",
  subgroup = preDM_priority_filtered_multi_forMETA$duplicate
)


#---------------
#CARDIOMORTALITY
#---------------
#cardiomortality, women
preDM_priority_filtered_multi_forMETA <- preDM_priority_filtered_multi_forMETA %>% 
  mutate(
    log_HR_cardiomort_F = log(HR_cardiomort_F),
    se_log_HR_cardiomort_F = (log(UCI_cardiomort_F) - log(LCI_cardiomort_F)) / (2*1.96)
  ) %>% 
  relocate(log_HR_cardiomort_F, .after = HR_cardiomort_F) %>% 
  relocate(se_log_HR_cardiomort_F, .after = UCI_cardiomort_F)

cardiomort_F_metaresult_multi <- metagen(
  studlab = Author,
  sort = preDM_priority_filtered_multi_forMETA$duplicate,
  TE = log_HR_cardiomort_F,
  seTE = se_log_HR_cardiomort_F,
  data = preDM_priority_filtered_multi_forMETA,
  backtransf = T,
  sm="HR",
  method.tau = "DL",
  subgroup = preDM_priority_filtered_multi_forMETA$duplicate
)

#cardiomortality, men
preDM_priority_filtered_multi_forMETA <- preDM_priority_filtered_multi_forMETA %>% 
  mutate(
    log_HR_cardiomort_M = log(HR_cardiomort_M),
    se_log_HR_cardiomort_M = (log(UCI_cardiomort_M) - log(LCI_cardiomort_M)) / (2*1.96)
  ) %>% 
  relocate(log_HR_cardiomort_M, .after = HR_cardiomort_M) %>% 
  relocate(se_log_HR_cardiomort_M, .after = UCI_cardiomort_M)

cardiomort_M_metaresult_multi <- metagen(
  studlab = Author,
  sort = preDM_priority_filtered_multi_forMETA$duplicate,
  TE = log_HR_cardiomort_M,
  seTE = se_log_HR_cardiomort_M,
  data = preDM_priority_filtered_multi_forMETA,
  backtransf = T,
  sm="HR",
  method.tau = "DL",
  subgroup = preDM_priority_filtered_multi_forMETA$duplicate
)

#---
#CVD
#---
#cvd, women
preDM_priority_filtered_multi_forMETA <- preDM_priority_filtered_multi_forMETA %>% 
  mutate(
    log_HR_CVD_F = log(HR_CVD_F),
    se_log_HR_CVD_F = (log(UCI_CVD_F) - log(LCI_CVD_F)) / (2*1.96)
  ) %>% 
  relocate(log_HR_CVD_F, .after = HR_CVD_F) %>% 
  relocate(se_log_HR_CVD_F, .after = UCI_CVD_F)

CVD_F_metaresult_multi <- metagen(
  studlab = Author,
  sort = preDM_priority_filtered_multi_forMETA$duplicate,
  TE = log_HR_CVD_F,
  seTE = se_log_HR_CVD_F,
  data = preDM_priority_filtered_multi_forMETA,
  backtransf = T,
  sm="HR",
  method.tau = "DL",
  subgroup = preDM_priority_filtered_multi_forMETA$duplicate
)

#cvd, men
preDM_priority_filtered_multi_forMETA <- preDM_priority_filtered_multi_forMETA %>% 
  mutate(
    log_HR_CVD_M = log(HR_CVD_M),
    se_log_HR_CVD_M = (log(UCI_CVD_M) - log(LCI_CVD_M)) / (2*1.96)
  ) %>% 
  relocate(log_HR_CVD_M, .after = HR_CVD_M) %>% 
  relocate(se_log_HR_CVD_M, .after = UCI_CVD_M)

CVD_M_metaresult_multi <- metagen(
  studlab = Author,
  sort = preDM_priority_filtered_multi_forMETA$duplicate,
  TE = log_HR_CVD_M,
  seTE = se_log_HR_CVD_M,
  data = preDM_priority_filtered_multi_forMETA,
  backtransf = T,
  sm="HR",
  method.tau = "DL",
  subgroup = preDM_priority_filtered_multi_forMETA$duplicate
)

#---
#CHD
#---
#CHD, women
preDM_priority_filtered_multi_forMETA <- preDM_priority_filtered_multi_forMETA %>% 
  mutate(
    log_HR_CHD_F = log(HR_CHD_F),
    se_log_HR_CHD_F = (log(UCI_CHD_F) - log(LCI_CHD_F)) / (2*1.96)
  ) %>% 
  relocate(log_HR_CHD_F, .after = HR_CHD_F) %>% 
  relocate(se_log_HR_CHD_F, .after = UCI_CHD_F)

CHD_F_metaresult_multi <- metagen(
  studlab = Author,
  sort = preDM_priority_filtered_multi_forMETA$duplicate,
  TE = log_HR_CHD_F,
  seTE = se_log_HR_CHD_F,
  data = preDM_priority_filtered_multi_forMETA,
  backtransf = T,
  sm="HR",
  method.tau = "DL",
  subgroup = preDM_priority_filtered_multi_forMETA$duplicate
)

#CHD, men
preDM_priority_filtered_multi_forMETA <- preDM_priority_filtered_multi_forMETA %>% 
  mutate(
    log_HR_CHD_M = log(HR_CHD_M),
    se_log_HR_CHD_M = (log(UCI_CHD_M) - log(LCI_CHD_M)) / (2*1.96)
  ) %>% 
  relocate(log_HR_CHD_M, .after = HR_CHD_M) %>% 
  relocate(se_log_HR_CHD_M, .after = UCI_CHD_M)

CHD_M_metaresult_multi <- metagen(
  studlab = Author,
  sort = preDM_priority_filtered_multi_forMETA$duplicate,
  TE = log_HR_CHD_M,
  seTE = se_log_HR_CHD_M,
  data = preDM_priority_filtered_multi_forMETA,
  backtransf = T,
  sm="HR",
  method.tau = "DL",
  subgroup = preDM_priority_filtered_multi_forMETA$duplicate
)


#------
#STROKE
#------
#STROKE, women
preDM_priority_filtered_multi_forMETA <- preDM_priority_filtered_multi_forMETA %>% 
  mutate(
    log_HR_stroke_F = log(HR_stroke_F),
    se_log_HR_stroke_F = (log(UCI_stroke_F) - log(LCI_stroke_F)) / (2*1.96)
  ) %>% 
  relocate(log_HR_stroke_F, .after = HR_stroke_F) %>% 
  relocate(se_log_HR_stroke_F, .after = UCI_stroke_F)

stroke_F_metaresult_multi <- metagen(
  studlab = Author,
  sort = preDM_priority_filtered_multi_forMETA$duplicate,
  TE = log_HR_stroke_F,
  seTE = se_log_HR_stroke_F,
  data = preDM_priority_filtered_multi_forMETA,
  backtransf = T,
  sm="HR",
  method.tau = "DL",
  subgroup = preDM_priority_filtered_multi_forMETA$duplicate
)

#STROKE, men
preDM_priority_filtered_multi_forMETA <- preDM_priority_filtered_multi_forMETA %>% 
  mutate(
    log_HR_stroke_M = log(HR_stroke_M),
    se_log_HR_stroke_M = (log(UCI_stroke_M) - log(LCI_stroke_M)) / (2*1.96)
  ) %>% 
  relocate(log_HR_stroke_M, .after = HR_stroke_M) %>% 
  relocate(se_log_HR_stroke_M, .after = UCI_stroke_M)

stroke_M_metaresult_multi <- metagen(
  studlab = Author,
  sort = preDM_priority_filtered_multi_forMETA$duplicate,
  TE = log_HR_stroke_M,
  seTE = se_log_HR_stroke_M,
  data = preDM_priority_filtered_multi_forMETA,
  backtransf = T,
  sm="HR",
  method.tau = "DL",
  subgroup = preDM_priority_filtered_multi_forMETA$duplicate
)

#-------------------
#PUT INTO DATA FRAME
#-------------------

y<-unique(preDM_priority_filtered_multi[,c("Author","duplicate", "def_priority")])
x <- data.frame(
  
  duplicate = mort_F_metaresult_multi$subgroup.levels,
  
  log_HR_mort_F = mort_F_metaresult_multi$TE.fixed.w,
  se_log_HR_mort_F = mort_F_metaresult_multi$seTE.fixed.w,
  log_HR_mort_M = mort_M_metaresult_multi$TE.fixed.w,
  se_log_HR_mort_M = mort_M_metaresult_multi$seTE.fixed.w,
  
  log_HR_cardiomort_F = cardiomort_F_metaresult_multi$TE.fixed.w,
  se_log_HR_cardiomort_F = cardiomort_F_metaresult_multi$seTE.fixed.w,
  log_HR_cardiomort_M = cardiomort_M_metaresult_multi$TE.fixed.w,
  se_log_HR_cardiomort_M = cardiomort_M_metaresult_multi$seTE.fixed.w,
  
  log_HR_CVD_F = CVD_F_metaresult_multi$TE.fixed.w,
  se_log_HR_CVD_F = CVD_F_metaresult_multi$seTE.fixed.w,
  log_HR_CVD_M = CVD_M_metaresult_multi$TE.fixed.w,
  se_log_HR_CVD_M = CVD_M_metaresult_multi$seTE.fixed.w,
  
  log_HR_CHD_F = CHD_F_metaresult_multi$TE.fixed.w,
  se_log_HR_CHD_F = CHD_F_metaresult_multi$seTE.fixed.w,
  log_HR_CHD_M = CHD_M_metaresult_multi$TE.fixed.w,
  se_log_HR_CHD_M = CHD_M_metaresult_multi$seTE.fixed.w,
  
  log_HR_stroke_F = stroke_F_metaresult_multi$TE.fixed.w,
  se_log_HR_stroke_F = stroke_F_metaresult_multi$seTE.fixed.w,
  log_HR_stroke_M = stroke_M_metaresult_multi$TE.fixed.w,
  se_log_HR_stroke_M = stroke_M_metaresult_multi$seTE.fixed.w
)
pooled_multi<-left_join(y, x, by="duplicate")

 

#calculate logHRS and SE for all the single ones
preDM_priority_filtered_single_forMETA <- preDM_priority_filtered_single %>% 
  select(Author, duplicate, def_priority, HR_mort_F:UCI_mort_M, HR_cardiomort_F:UCI_cardiomort_M, HR_stroke_F:UCI_stroke_M, HR_iscstroke_F:UCI_iscstroke_M,HR_hemstroke_F:UCI_hemstroke_M,HR_CVD_F:UCI_CVD_M,HR_CHD_F:UCI_CHD_M,HR_CKD_F:UCI_CKD_M,HR_HF_F:UCI_HF_M)

#mortality
#Calculate log HR of mort HR for women AND SE so we can use it in the meta analysis
preDM_priority_filtered_single_forMETA <- preDM_priority_filtered_single_forMETA %>% 
  mutate(
    log_HR_mort_F = log(HR_mort_F),
    se_log_HR_mort_F = (log(UCI_mort_F) - log(LCI_mort_F)) / (2*1.96)
  ) %>% 
  relocate(log_HR_mort_F, .after = HR_mort_F) %>% 
  relocate(se_log_HR_mort_F, .after = UCI_mort_F)

preDM_priority_filtered_single_forMETA<-as.data.frame(preDM_priority_filtered_single_forMETA) # always best to just change to data frame

#MEN
#Calculate log HR of mort HR for women AND SE. 
preDM_priority_filtered_single_forMETA <- preDM_priority_filtered_single_forMETA %>% 
  mutate(
    log_HR_mort_M = log(HR_mort_M),
    se_log_HR_mort_M = (log(UCI_mort_M) - log(LCI_mort_M)) / (2*1.96)
  ) %>% 
  relocate(log_HR_mort_M, .after = HR_mort_M) %>% 
  relocate(se_log_HR_mort_M, .after = UCI_mort_M)

#cardiomortality
#Calculate log HR of CVD mort HR for women AND SE so we can use it in the meta analysis
preDM_priority_filtered_single_forMETA <- preDM_priority_filtered_single_forMETA %>% 
  mutate(
    log_HR_cardiomort_F = log(HR_cardiomort_F),
    se_log_HR_cardiomort_F = (log(UCI_cardiomort_F) - log(LCI_cardiomort_F)) / (2*1.96)
  ) %>% 
  relocate(log_HR_cardiomort_F, .after = HR_cardiomort_F) %>% 
  relocate(se_log_HR_cardiomort_F, .after = UCI_cardiomort_F)

#Calculate log HR of CVD mort HR for women AND SE so we can use it in the meta analysis
preDM_priority_filtered_single_forMETA <- preDM_priority_filtered_single_forMETA %>% 
  mutate(
    log_HR_cardiomort_M = log(HR_cardiomort_M),
    se_log_HR_cardiomort_M = (log(UCI_cardiomort_M) - log(LCI_cardiomort_M)) / (2*1.96)
  ) %>% 
  relocate(log_HR_cardiomort_M, .after = HR_cardiomort_M) %>% 
  relocate(se_log_HR_cardiomort_M, .after = UCI_cardiomort_M)

#CVD
#Calculate log HR of CVD HR for women AND SE so we can use it in the meta analysis
preDM_priority_filtered_single_forMETA <- preDM_priority_filtered_single_forMETA %>% 
  mutate(
    log_HR_CVD_F = log(HR_CVD_F),
    se_log_HR_CVD_F = (log(UCI_CVD_F) - log(LCI_CVD_F)) / (2*1.96)
  ) %>% 
  relocate(log_HR_CVD_F, .after = HR_CVD_F) %>% 
  relocate(se_log_HR_CVD_F, .after = UCI_CVD_F)

#Calculate log HR of CVD HR for women AND SE so we can use it in the meta analysis
preDM_priority_filtered_single_forMETA <- preDM_priority_filtered_single_forMETA %>% 
  mutate(
    log_HR_CVD_M = log(HR_CVD_M),
    se_log_HR_CVD_M = (log(UCI_CVD_M) - log(LCI_CVD_M)) / (2*1.96)
  ) %>% 
  relocate(log_HR_CVD_M, .after = HR_CVD_M) %>% 
  relocate(se_log_HR_CVD_M, .after = UCI_CVD_M)

#Calculate log HR of CHD HR for women AND SE so we can use it in the meta analysis
preDM_priority_filtered_single_forMETA <- preDM_priority_filtered_single_forMETA %>% 
  mutate(
    log_HR_CHD_F = log(HR_CHD_F),
    se_log_HR_CHD_F = (log(UCI_CHD_F) - log(LCI_CHD_F)) / (2*1.96)
  ) %>% 
  relocate(log_HR_CHD_F, .after = HR_CHD_F) %>% 
  relocate(se_log_HR_CHD_F, .after = UCI_CHD_F)

#CHD - MEN
#Calculate log HR of CHD HR for women AND SE so we can use it in the meta analysis
preDM_priority_filtered_single_forMETA <- preDM_priority_filtered_single_forMETA %>% 
  mutate(
    log_HR_CHD_M = log(HR_CHD_M),
    se_log_HR_CHD_M = (log(UCI_CHD_M) - log(LCI_CHD_M)) / (2*1.96)
  ) %>% 
  relocate(log_HR_CHD_M, .after = HR_CHD_M) %>% 
  relocate(se_log_HR_CHD_M, .after = UCI_CHD_M)

#stroke
#Calculate log HR of STROKE HR for women AND SE so we can use it in the meta analysis
preDM_priority_filtered_single_forMETA <- preDM_priority_filtered_single_forMETA %>% 
  mutate(
    log_HR_stroke_F = log(HR_stroke_F),
    se_log_HR_stroke_F = (log(UCI_stroke_F) - log(LCI_stroke_F)) / (2*1.96)
  ) %>% 
  relocate(log_HR_stroke_F, .after = HR_stroke_F) %>% 
  relocate(se_log_HR_stroke_F, .after = UCI_stroke_F)

#Calculate log HR of STROKE HR for women AND SE so we can use it in the meta analysis
preDM_priority_filtered_single_forMETA <- preDM_priority_filtered_single_forMETA %>% 
  mutate(
    log_HR_stroke_M = log(HR_stroke_M),
    se_log_HR_stroke_M = (log(UCI_stroke_M) - log(LCI_stroke_M)) / (2*1.96)
  ) %>% 
  relocate(log_HR_stroke_M, .after = HR_stroke_M) %>% 
  relocate(se_log_HR_stroke_M, .after = UCI_stroke_M)



#keep 1:3, and then variables that start with se or log
pooled_single <- preDM_priority_filtered_single_forMETA %>% 
  select(1:3, matches("^(se_|log_)"))


final_table <- bind_rows(pooled_single, pooled_multi)
  



#==================================================
#STEP 8 - META ANALYSIS - FINAL TABLE  
#===================================================

#---------
#MORTALITY
#---------
#Mortality, women
mort_F_metaresult <- metagen(
  TE = log_HR_mort_F,
  seTE = se_log_HR_mort_F,
  data = final_table,
  backtransf = T,
  sm="HR",
  method.tau = "DL",
  subgroup = final_table$def_priority
)

#Mortality, men
mort_M_metaresult <- metagen(
  TE = log_HR_mort_M,
  seTE = se_log_HR_mort_M,
  data = final_table,
  backtransf = T,
  sm="HR",
  method.tau = "DL",
  subgroup = final_table$def_priority
)

#---------------
#CARDIOMORTALITY
#---------------

#CVD Mortality, women
cardiomort_F_metaresult <- metagen(
  TE = log_HR_cardiomort_F,
  seTE = se_log_HR_cardiomort_F,
  data = final_table,
  backtransf = T,
  sm="HR",
  method.tau = "DL",
  subgroup = final_table$def_priority
)

#CVD MORTALITY - MEN
cardiomort_M_metaresult <- metagen(
  TE = log_HR_cardiomort_M,
  seTE = se_log_HR_cardiomort_M,
  data = final_table,
  backtransf = T,
  sm="HR",
  method.tau = "DL",
  subgroup = final_table$def_priority
)

#---
#CVD
#---

#CVD, women
CVD_F_metaresult <- metagen(
  TE = log_HR_CVD_F,
  seTE = se_log_HR_CVD_F,
  data = final_table,
  backtransf = T,
  sm="HR",
  method.tau = "DL",
  subgroup = final_table$def_priority
)

#CVD - MEN
CVD_M_metaresult <- metagen(
  TE = log_HR_CVD_M,
  seTE = se_log_HR_CVD_M,
  data = final_table,
  backtransf = T,
  sm="HR",
  method.tau = "DL",
  subgroup = final_table$def_priority
)

#---
#CHD
#---

#CHD, women
CHD_F_metaresult <- metagen(
  TE = log_HR_CHD_F,
  seTE = se_log_HR_CHD_F,
  data = final_table,
  backtransf = T,
  sm="HR",
  method.tau = "DL",
  subgroup = final_table$def_priority
)

#CHD, men
CHD_M_metaresult <- metagen(
  TE = log_HR_CHD_M,
  seTE = se_log_HR_CHD_M,
  data = final_table,
  backtransf = T,
  sm="HR",
  method.tau = "DL",
  subgroup = final_table$def_priority
)

#------
#STROKE
#------

#STROKE, women
stroke_F_metaresult <- metagen(
  TE = log_HR_stroke_F,
  seTE = se_log_HR_stroke_F,
  data = final_table,
  backtransf = T,
  sm="HR",
  method.tau = "DL",
  subgroup = final_table$def_priority
)

#STROKE - MEN
stroke_M_metaresult <- metagen(
  TE = log_HR_stroke_M,
  seTE = se_log_HR_stroke_M,
  data = final_table,
  backtransf = T,
  sm="HR",
  method.tau = "DL",
  subgroup = final_table$def_priority
)


#F_results <- exp(c(mort_F_metaresult$TE.random, cardiomort_F_metaresult$TE.random, CVD_F_metaresult$TE.random, CHD_F_metaresult$TE.random, stroke_F_metaresult$TE.random))
#M_results <- exp(c(mort_M_metaresult$TE.random, cardiomort_M_metaresult$TE.random, CVD_M_metaresult$TE.random, CHD_M_metaresult$TE.random, stroke_M_metaresult$TE.random))

#diff = F_results/M_results












