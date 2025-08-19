




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
