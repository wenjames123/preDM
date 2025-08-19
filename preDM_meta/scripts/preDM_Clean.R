library(tidyverse)
library(meta)

preDM <- read_csv("./Practice_Data.csv")


#================================================
#EXTRA 1  - TOTAL SAMPLE SIZE PER DEFINITION
#================================================
samplesize_bydefinition <- preDM %>% 
  filter(!duplicated(duplicate)) %>% 
  group_by(preDM_def) %>% 
  summarize(sum = sum(n_participants))


#================================================
#EXTRA 2  - OUTCOME COUNTS
#================================================
outcome_counts <- preDM %>% 
  filter(!duplicated(duplicate)) %>% 
  summarize(across(
    mort_report:CKD_report,
    ~ sum(.x, na.rm=TRUE)
  ))

#================================================
#PART 1 - DETERMINE DEFINITION PRIORITY
#to do this: 
#1. count the # of studies per definition      --> right now the count is driving the priority. could also pick global pop per study, or greatest per study.
#2. order them from most to least frequent 
#================================================

#count number of unique studies under each definition, then match it to def_priority (from 1-6)
preDM_definition_counts <- preDM %>% 
  group_by(preDM_def) %>% 
  filter(!duplicated(duplicate)) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  mutate(def_priority = row_number())

#Join preDM and preDM_definiton_counts BY preDM_def --> map each definition to its priority 
preDM_with_priority <- preDM %>% 
  left_join(preDM_definition_counts, by = "preDM_def") %>% 
  select(-count) %>% 
  relocate(def_priority, .after = preDM_def)

#MN
# temp <- preDM %>%
#   mutate(
#     #Priority was based on .....
#     def_priority = case_when(
#       preDM_def == 5 ~ 1,
#       preDM_def == 0 ~ 2,
#       preDM_def == 2 ~ 3,
#       preDM_def == 1 ~ 4,
#       preDM_def == 3 ~ 5,
#       preDM_def == 4 ~ 6),
#     .after = preDM_def
#   )

#==================================================
#STEP 2 - META ANALYSIS - MULTIPLE SUBGROUP STUDIES
#===================================================

#For each study, take the highest priority (smalest value of def_priority), and take studies that have >1 subgroup PER DEFINITION
preDM_forMETA_multi <- preDM_with_priority %>% 
  group_by(duplicate) %>% 
  filter(def_priority == min(def_priority)) %>% 
  filter(n()>1) %>% 
  select(Author, duplicate, def_priority, HR_mort_F:UCI_mort_M, HR_cardiomort_F:UCI_cardiomort_M, HR_stroke_F:UCI_stroke_M, HR_iscstroke_F:UCI_iscstroke_M,HR_hemstroke_F:UCI_hemstroke_M,HR_CVD_F:UCI_CVD_M,HR_CHD_F:UCI_CHD_M,HR_CKD_F:UCI_CKD_M,HR_HF_F:UCI_HF_M) %>% 
  ungroup()


#RUN THE META ANALYSIS --> need to learn map function
temp <- preDM_forMETA_multi %>%  
  mutate(n = 1:nrow(.), .before = Author) %>% 
  pivot_longer(., -c(n, Author, duplicate, def_priority)) %>% 
  separate(name, into = c("var", "outcome", "sex"), sep = "_") %>% 
  pivot_wider(., names_from = var, values_from = value) %>% 
  mutate(
    log_HR = log(HR),
    se_log_HR = (log(UCI) - log(LCI)) / (2*1.96)) %>% 
  group_by(outcome, sex) %>% 
  nest() %>% 
  mutate(
    # Remove Missing - if you want to silence the warning messages, remove outcomes with no data. Wont work as is. 
    #dataclean = map(data, ~drop_na(.x)))
    
    # Metagen function - in this code i am running this ....
    meta = map(data, ~metagen(
      studlab = .x$Author,
      sort = .x$duplicate,
      TE = .x$log_HR,
      seTE = .x$se_log_HR,
      data = .x,
      backtransf = T,
      sm="HR",
      method.tau = "DL",
      subgroup = .x$duplicate
    )),
    
    #Extract estimates and put into separate tibble 
    estimate = map(meta, ~tibble(
      Study = unique(.x$studlab), 
      duplicate = unique(.x$subgroup),
      TEfixed = .x$TE.fixed.w,
      TEse = .x$seTE.fixed.w))
    )

# Step 2: Extract and Clean
temp2 <- temp %>% 
  select(outcome, sex, estimate) %>% 
  unnest(., c(estimate)) %>% 
  unite(., "var", c(outcome, sex)) %>% 
  pivot_wider(., names_from = var, values_from = c(TEfixed, TEse))


#what do i want to do here?


#for each outcome:
# 1. calculate the logHR and the se_logHR  
#2. store both as new columns in the preMD_forMETA_multi
#3. 


#so map inputs a list, and then does an operation for every single element, and then returns a new list right?






se_log_HR_mort_F = (log(UCI_mort_F) - log(LCI_mort_F)) / (2*1.96)









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


F_results <- exp(c(mort_F_metaresult$TE.random, cardiomort_F_metaresult$TE.random, CVD_F_metaresult$TE.random, CHD_F_metaresult$TE.random, stroke_F_metaresult$TE.random))
M_results <- exp(c(mort_M_metaresult$TE.random, cardiomort_M_metaresult$TE.random, CVD_M_metaresult$TE.random, CHD_M_metaresult$TE.random, stroke_M_metaresult$TE.random))

#diff = F_results/M_results







#MN ----


### Idea to replace mulitple renaming ----
definition_0 <- preDM %>% filter(preDM_def == 0)
definition_0_unique <- definition_0[!duplicated(definition_0$duplicate),]
n_samplesize_0 <- sum(definition_0_unique$n_participants)

temp <- preDM %>% 
  filter(preDM_def == 0) %>% 
  filter(!duplicated(duplicate)) %>%
  summarise(sum = sum(n_participants, na.rm = TRUE)) %>% 
  pull(sum)




#Loops
# For loops 
# MAP function (tidyverse)

# 3 components to a basical for loop structure
output = vector("double", 10)    # 1) Output
for(i in 1:10){                  # 2) Sequence
  output[i] = i                  # 3) Body
}

# Basic example
for(i in 1:10){
  print(i)
}



# Examples of defining the sequence - all yield same output
1:117
seq_along(preDM)
1:ncol(preDM)


# Incorporating Subsetting into for loop
for(i in 1:10){
  name = preDM[, i]
  print(name)
}


# Another example - subseting to run table functions
for(i in seq_along(df)){
  print(table(df[,i ]))
}


# More advanced idea - use for loops to iterate sample size calculation
samplesize <- data.frame(
  cohort_def = c(0, 1, 2, 3, 4, 5),
  samplesize = NA
)

i=1
for(i in 1:nrow(samplesize)){
  samplesize[i, "samplesize"] <- preDM %>% 
    filter(preDM_def == samplesize[i, "cohort_def"]) %>% 
    filter(!duplicated(duplicate)) %>%
    summarise(sum = sum(n_participants, na.rm = TRUE)) %>% 
    pull(sum)
}


# Advanced example using map function 
# Not fully correct. I remove duplicates and therefore my subsets are wrong. But should get the general idea. 
# Run this using the preDM_priority_filtered_multi_forMETA that does not have log values calculated
temp <- preDM_priority_filtered_multi_forMETA %>% 
  # This is an assumption. I dont know why there are multiple rows
  distinct(Author, .keep_all = TRUE) %>% 
  pivot_longer(., -c(Author, duplicate, def_priority)) %>% 
  separate(name, into = c("var", "outcome", "sex"), sep = "_") %>% 
  pivot_wider(., names_from = var, values_from = value) %>% 
  # Remove CKD since it is empty?
  filter(outcome != "CKD") %>% 
  mutate(
    log_HR = log(HR),
    se_log_HR = (log(UCI) - log(LCI)) / (2*1.96)) %>% 
  group_by(outcome, sex) %>% 
  nest() %>% 
  mutate(
    # Remove Missing
    data = map(data, ~drop_na(.x)),

    # Metagen function
    meta = map(data, ~metagen(
      studlab = .x$Author,
      sort = .x$duplicate,
      TE = .x$log_HR,
      seTE = .x$se_log_HR,
      data = .x,
      backtransf = T,
      sm="HR",
      method.tau = "DL",
      subgroup = .x$duplicate
    )))
  


         