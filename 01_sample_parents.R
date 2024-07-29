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


# Sample A: parents with one child ----------------------------------------

# Select all parents with only one child
parents1 <- cfps2010_adult %>% 
  select(pid, starts_with("code_a_c")) %>% 
  mutate_at(c(seq(2, 11)), list(~ ifelse(!is.na(.), 1, NA))) %>% 
  mutate(children = rowSums(select(., starts_with("code_a_c")), na.rm = TRUE)) %>% 
  filter(children == 1)

# Select and construct relevant variables
parents <- cfps2010_adult %>% 
  filter(pid %in% parents1$pid) %>% 
  select(pid, pid_s, pid_c1,
         gender, qa1age, qe1, urban, qa5code,
         cfps2010edu_best, sedu, tb4_a_c1, 
         tb2_a_c1, tb1b_a_c1, co_c1, alive_a_c1,
         qm403, qk802, qp3) %>% 
  rename(RGENDER = gender,
         RAGE = qa1age,
         RMSTAT = qe1,
         RHUKOU = urban,
         RMINORITY = qa5code,
         REDU = cfps2010edu_best,
         SEDU = sedu,
         CGENDER = tb2_a_c1,
         CAGE = tb1b_a_c1,
         CRESIDE = co_c1,
         CEDU = tb4_a_c1,
         CALIVE = alive_a_c1,
         RSATISFY = qm403,
         RHAPPY = qk802,
         RHEALTH = qp3) %>% 
  mutate(RMSTAT = ifelse(RMSTAT == 2, 1, 0)) %>% 
  mutate(RMINORITY = ifelse(RMINORITY == 1, 0, 1))

# Idenitfy Party membership
party_members <- cfps2010_adult %>% 
  select(pid, starts_with("qa7_s")) %>% 
  pivot_longer(cols = -pid,
               values_to = "party",
               names_to = "number") %>% 
  mutate(party = ifelse(party == 1, 1, 0)) %>% 
  filter(party == 1) %>% 
  pull(pid)

parents <- parents %>% 
  mutate(RPARTY = ifelse(pid %in% party_members, 1, 0))

# Construct mobility indicators
parents <- parents %>% 
  # Identify the highest education of parents
  mutate(PEDU = case_when(REDU >= SEDU ~ REDU,
                          REDU < SEDU ~ SEDU,
                          is.na(REDU) ~ SEDU,
                          is.na(SEDU) ~ REDU)) %>% 
  mutate(PEDUA = case_when(PEDU <= 2 ~ 1,
                           PEDU == 3 ~ 2,
                           PEDU == 4 ~ 3,
                           PEDU >= 5 ~ 4),
         REDUA = case_when(REDU <= 2 ~ 1,
                           REDU == 3 ~ 2,
                           REDU == 4 ~ 3,
                           REDU >= 5 ~ 4),
         CEDUA = case_when(CEDU <= 2 ~ 1,
                           CEDU == 3 ~ 2,
                           CEDU == 4 ~ 3,
                           CEDU >= 5 ~ 4)) %>% 
  mutate(moba = case_when(CEDUA == PEDUA ~ "staya",
                          CEDUA - PEDUA > 0 ~ "upa",
                          CEDUA - PEDUA < 0 ~ "downa")) %>% 
  mutate(upa = ifelse(moba == "upa", 1, 0),
         downa = ifelse(moba == "downa", 1, 0),
         moba = ifelse(moba == "staya", 0, 1)) %>% 
  mutate(PEDUA = factor(PEDUA),
         CEDUA = factor(CEDUA))

# Sample restriction
parents_edua <- parents %>% 
  select(pid, REDUA)

parents <- parents %>% 
  select(pid, RGENDER,
         RSATISFY, RAGE, RMSTAT, RHUKOU, RMINORITY, RPARTY, CGENDER, CRESIDE,
         PEDUA, CEDUA, moba, upa, downa) %>% 
  na.omit() %>% 
  left_join(parents_edua,
            by = "pid")

# Save sample
save(parents, file = "to-be-filled")