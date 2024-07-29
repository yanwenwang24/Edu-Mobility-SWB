#===============================================================================
# 29/07/2024
# Sample selection
# Educational Assortative Mating and Subjective Well-Being
# Yanwen Wang, yanwenwang@u.nus.edu
#===============================================================================

######################################################################
# Loading packages
######################################################################
library(tidyverse)
library(readxl)
library(haven)
library(zoo)

######################################################################
# Loading data
######################################################################
cfps2010_adult_filepath <- "to-be-filled"
cfps2010_familyr_filepath <- "to-be-filled"

# Individual-level adult data
cfps2010_adult <- read_stata(cfps2010_adult_filepath) %>% 
  mutate_all(funs(replace(., .<0, NA)))

# Household data
cfps2010_familyr <- read_stata(cfps2010_familyr_filepath) %>% 
  mutate_all(funs(replace(., .<0, NA)))



# Sample B: Children ------------------------------------------------------

# Select and construct relevant variables
child <- cfps2010_adult %>% 
  select(pid, qb1,
         gender, qa1age, qe1, urban, qa5code, 
         co_f, co_m, alive_a_f, alive_a_m,
         cfps2010edu_best, feduc, meduc,
         qm403, qk802, qp3, qd3) %>% 
  rename(RGENDER = gender,
         RSIB = qb1,
         RAGE = qa1age,
         RMSTAT = qe1,
         RHUKOU = urban,
         RMINORITY = qa5code,
         REDU = cfps2010edu_best,
         FEDU = feduc,
         MEDU = meduc,
         RSATISFY = qm403,
         RHAPPY = qk802,
         RHEALTH = qp3,
         RSCHOOL = qd3) %>% 
  mutate(RMSTAT = ifelse(RMSTAT == 2, 1, 0)) %>% 
  mutate(RRESIDE = case_when(is.na(co_f) ~ co_m,
                             is.na(co_m) ~ co_f,
                             co_f == 0 & co_m == 0 ~ 0,
                             co_f == 1 | co_m == 1 ~ 1)) %>% 
  mutate(PALIVE = case_when(is.na(alive_a_f) ~ alive_a_m,
                            is.na(alive_a_m) ~ alive_a_f,
                            alive_a_f == 0 & alive_a_m == 0 ~ 0,
                            alive_a_f == 1 | alive_a_m == 1 ~ 1)) %>% 
  mutate(RMINORITY = ifelse(RMINORITY == 1, 0, 1)) %>% 
  mutate(RPARTY = ifelse(pid %in% party_members, 1, 0)) %>% 
  # Identify only children without siblings
  mutate(RSINGLE = ifelse(RSIB == 0, 1, 0))

# Restrict age
child <- child %>% 
  # Exclude those still in school
  filter(RSCHOOL == 0 | is.na(RSCHOOL)) %>% 
  filter(RAGE >= 20, RAGE <= 50)

# Construct mobility indicators
child <- child %>% 
  # Identify the highest educated parent
  mutate(PEDU = case_when(FEDU >= MEDU ~ FEDU,
                          FEDU < MEDU ~ MEDU,
                          is.na(FEDU) ~ MEDU,
                          is.na(MEDU) ~ FEDU)) %>% 
  mutate(PEDUA = case_when(PEDU <= 2 ~ 1,
                           PEDU == 3 ~ 2,
                           PEDU == 4 ~ 3,
                           PEDU >= 5 ~ 4),
         REDUA = case_when(REDU <= 2 ~ 1,
                           REDU == 3 ~ 2,
                           REDU == 4 ~ 3,
                           REDU >= 5 ~ 4)) %>% 
  mutate(moba = case_when(REDUA == PEDUA ~ "staya",
                          REDUA - PEDUA  > 0 ~ "upa",
                          REDUA - PEDUA  < 0 ~ "downa")) %>% 
  mutate(upa = ifelse(moba == "upa", 1, 0),
         downa = ifelse(moba == "downa", 1, 0),
         moba = ifelse(moba == "staya", 0, 1)) %>% 
  mutate(PEDUA = factor(PEDUA),
         REDUA = factor(REDUA))

# Sample restriction
child <- child %>% 
  select(pid, RGENDER, RRESIDE, RSINGLE,
         RSATISFY, RAGE, RMSTAT, RHUKOU, RMINORITY, RPARTY, 
         REDUA, PEDUA, moba, upa, downa) %>%
  na.omit()

# Save sample
save(child, file = "to-be-filled")