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

#doing the nesting below removes the priority, so this saves it to be leftjoined later.
def_priority_save1 <- preDM_forMETA_multi %>% 
  select(duplicate,def_priority) %>% 
  filter(!duplicated(duplicate))

#RUN THE META ANALYSIS --> need to learn map function
temp <- preDM_forMETA_multi %>%  
  
  #Give each row a unique identifier, so there's no identical row errors
  mutate(n = 1:nrow(.), .before = Author) %>% 
  
  #pivot all columns to rows, except for c(n, Author, duplicate, def_priority)
  pivot_longer(., -c(n, Author, duplicate, def_priority)) %>% 
  
  #Separate HR_mort_F into 3 variables: HR, mort, F
  separate(name, into = c("var", "outcome", "sex"), sep = "_") %>% 
  
  #pivot HR, LCI, UCI to columns
  pivot_wider(., names_from = var, values_from = value) %>% 
  
  #calculate log_HR and se_log_HR
  mutate(
    logHR = log(HR),
    selogHR = (log(UCI) - log(LCI)) / (2*1.96)) %>% 
  
  #group by outcome & sex combinations, and then nest the corresponding data frames
  group_by(outcome, sex) %>% 
  nest() %>% 
  
  
  mutate(
    # Remove Missing - if you want to silence the warning messages, remove outcomes with no data. Wont work as is. 
    #dataclean = map(data, ~drop_na(.x)))
    
    # Add new column called meta -> input is nested data, output is the meta analysis results (as a list)
    # ~ means calling an existing function, .x means just the inputed data frame
    meta = map(data, ~metagen(
      studlab = .x$Author,
      sort = .x$duplicate,
      TE = .x$logHR,
      seTE = .x$selogHR,
      data = .x,
      backtransf = T,
      sm="HR",
      method.tau = "DL",
      subgroup = .x$duplicate
    )),
    
    #Add new column called 'estimate' -> inputs meta results, output is tibble with the fixed results
    estimate = map(meta, ~tibble(
      
      #take unique study name & subgroup #
      Author = unique(.x$studlab), 
      duplicate = unique(.x$subgroup),
      
      #extract fixed TE & se
      logHR = .x$TE.fixed.w,
      selogHR = .x$seTE.fixed.w
      )
      )
    )

# Step 2: Extract and Clean
temp2 <- temp %>% 
  
  #just take columns we want
  select(outcome, sex, estimate) %>% 
  
  #unnest (uncollapse) the estimate nested tables
  unnest(., c(estimate)) %>% 
  
  #turn mort and F into mort_F
  unite(., "var", c(outcome, sex)) %>% 
  
  #make log_HR_mort_F and se_HR_mort_F
  pivot_wider(., names_from = var, values_from = c(logHR, selogHR))

temp2 <- temp2 %>% 
  left_join(.,def_priority_save, by = "duplicate") %>% 
  relocate(def_priority, .after = duplicate)


#==================================================
#STEP 3 - MAKE FINAL TABLE - combine single & multi
#===================================================

#1. calc log_HR and se_log_HR for all outcomes

#Filter by highest priority, then studies with NO SUBGROUPS per definition
preDM_forMETA_single <- preDM_with_priority %>%
  group_by(duplicate) %>% 
  filter(def_priority == min(def_priority)) %>% 
  filter(n()==1) %>% 
  select(Author, duplicate, def_priority, HR_mort_F:UCI_mort_M, HR_cardiomort_F:UCI_cardiomort_M, HR_stroke_F:UCI_stroke_M, HR_iscstroke_F:UCI_iscstroke_M,HR_hemstroke_F:UCI_hemstroke_M,HR_CVD_F:UCI_CVD_M,HR_CHD_F:UCI_CHD_M,HR_CKD_F:UCI_CKD_M,HR_HF_F:UCI_HF_M) %>% 
  ungroup()


temp3 <- preDM_forMETA_single %>% 
  
  #pivot longer, then separate HR,LCI,UCI so they're all separate columns, then calc HR and SE
  pivot_longer(.,-c(Author,duplicate,def_priority)) %>% 
  separate(name, into = c("var","outcome","sex"), sep = "_") %>% 
  pivot_wider(.,names_from = c(var), values_from = value) %>% 
  mutate(
    logHR = log(HR),
    selogHR = (log(UCI) - log(LCI)) / (2*1.96)
  ) %>% 
  
  #reunite and pivot back
  unite("var", c(outcome,sex)) %>% 
  select(Author, duplicate,def_priority, var, logHR, selogHR) %>% 
  pivot_wider(names_from = var, values_from = c(logHR, selogHR))


#combine the two tables vertically
final_table <- bind_rows(temp2,temp3)

#save matchings for all studies
def_priority_save2 <- final_table %>% 
  select(duplicate,def_priority) %>% 
  filter(!duplicated(duplicate))

#==================================================
#STEP 4 - RUN META ANALYSIS ON FINAL TABLE
#===================================================

temp4 <- final_table %>% 
  pivot_longer(-c(Author,duplicate,def_priority)) %>% 
  separate(name, into = c("var", "outcome", "sex"), sep = "_") %>% 
  pivot_wider(names_from = c(var), values_from = value) %>% 
  drop_na() %>% 
  group_by(outcome,sex) %>% 
  nest() %>% 
  mutate(
    
    metaresult = map(data, ~metagen(
      
      TE = .x$logHR,
      seTE = .x$selogHR,
      data = .x,
      backtransf = T,
      sm="HR",
      method.tau = "DL",
      subgroup = .x$def_priority
    )),
    
    summaryEffect = map(metaresult,~tibble(
      
      logHR_pooled = .x$TE.random,
      selogHR_pooled = .x$seTE.random,
    
    )),
    
    subgroupEffects = map(metaresult, ~tibble(

      def_priority = .x$subgroup.levels,
      logHR_byDefinition = .x$TE.random.w,
      selogHR_byDefinition = .x$seTE.random.w

    ))
    
  )

#extract & clean
pooledResults <- temp4 %>% 
  select(outcome,sex,summaryEffect) %>% 
  unnest(summaryEffect) %>% 
  mutate(
    HR_pooled = exp(logHR_pooled),
    LCI = exp(logHR_pooled - 1.96*selogHR_pooled),
    UCI = exp(logHR_pooled + 1.96*selogHR_pooled)
  ) %>% 
  select(-c(logHR_pooled,selogHR_pooled)) %>% 
  pivot_wider(names_from = sex, values_from = c(HR_pooled,LCI,UCI)) %>% 
  select(outcome, ends_with("_F"),ends_with("_M"))

  
subgroupResults <- temp4 %>% 
  select(outcome,sex,subgroupEffects) %>% 
  unnest(subgroupEffects) %>% 
  mutate(
    HR_byDefinition = exp(logHR_byDefinition),
    LCI = exp(logHR_byDefinition - 1.96*selogHR_byDefinition),
    UCI = exp(logHR_byDefinition + 1.96*selogHR_byDefinition)
  ) %>% 
  select(-c(logHR_byDefinition,selogHR_byDefinition)) %>% 
  pivot_wider(names_from = sex, values_from = c(HR_byDefinition,LCI,UCI)) %>% 
  select(outcome, def_priority, ends_with("_F"),ends_with("_M"))


#last step --> calculate RHRs and run final metas

pooledRHR <- temp4 %>% 
  select(outcome,sex,summaryEffect) %>% 
  unnest(summaryEffect) %>% 
  pivot_wider(names_from = sex, values_from = c(logHR_pooled,selogHR_pooled)) %>% 
  mutate(
    logRHR      = logHR_pooled_F - logHR_pooled_M,
    se_logRHR   = sqrt(selogHR_pooled_F^2 + selogHR_pooled_M^2),
    RHR_pooled  = exp(logRHR),
    LCI         = exp(logRHR - 1.96 * se_logRHR),
    UCI         = exp(logRHR + 1.96 * se_logRHR)
  ) %>% 
  select(outcome, logRHR, LCI, UCI)













  
    mutate(
    
    mortFmeta = map(.x, metagen(
      
      TE = log_HR_mort_F,
      seTE = se_log_HR_mort_F,
      data = final_table,
      backtransf = T,
      sm="HR",
      method.tau = "DL",
      subgroup = final_table$def_priority
      
      
      
      
      
    )) 
    
  )


















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
  


         