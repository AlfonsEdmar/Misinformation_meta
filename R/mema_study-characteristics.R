################################################################################

# Misinformation Meta-analysis - Study Characteristics

################################################################################

# Load packages ----------------------------------------------------------------

library(tidyverse)

# Load data --------------------------------------------------------------------

df <- read_csv("data/complete_data_cleaned.csv")

# Country and number of studies------------------------------------------------- 

summary_country <- df %>% 
  group_by(country) %>%
  summarise(number_of_studies = n_distinct(id_study)) %>% 
  arrange(desc(number_of_studies))
summary_country

#Items total--------------------------------------------------------------------

total_items <- df %>% 
  summarise(mean      = mean(items_total, na.rm = T),
            median    = median(items_total, na.rm = T),
            sd        = sd(items_total, na.rm = T),
            n_number  = n_distinct(items_total, na.rm = T))

#Postevent retention interval---------------------------------------------------

postev_ret <- df %>% 
  filter(postevent_retention_interval >= 1) %>% 
  summarise(mean      = mean(postevent_retention_interval, na.rm = T),
            median    = median(postevent_retention_interval, na.rm = T),
            sd        = sd(postevent_retention_interval, na.rm = T),
            min       = min(postevent_retention_interval, na.rm = T),
            max       = max(postevent_retention_interval, na.rm = T),
            n_number  = n_distinct(id_study, na.rm = T))

# Postexposure retention interval ----------------------------------------------

postex_ret <- df %>% 
  filter(postexposure_retention_interval >= 1) %>% 
  summarise(mean      = mean(postexposure_retention_interval, na.rm = T),
            median    = median(postexposure_retention_interval, na.rm = T),
            sd        = sd(postexposure_retention_interval, na.rm = T),
            min       = min(postexposure_retention_interval, na.rm = T),
            max       = max(postexposure_retention_interval, na.rm = T),
            n_number  = n_distinct(id_study, na.rm = T))

# Proportion of female participants --------------------------------------------

gender_prop <- df %>% 
  summarise(mean      = mean(gender_female_prop, na.rm = T),
            median    = median(items_control, na.rm = T),
            sd_co     = sd(items_control, na.rm = T),
            n_number  = n_distinct(items_control, na.rm = T))

# Age Categories ---------------------------------------------------------------

age_cat_preschool <- df %>% 
  filter(age_mean <= 5) %>% 
  summarise(mean      = mean(age_mean, na.rm = T),
            median    = median(age_mean, na.rm = T),
            sd        = sd(age_mean, na.rm = T),
            n_number  = n_distinct(id_study))

age_cat_school <- df %>% 
  filter(age_mean > 5 & age_mean <= 12) %>% 
  summarise(mean      = mean(age_mean, na.rm = T),
            median    = median(age_mean, na.rm = T),
            sd        = sd(age_mean, na.rm = T),
            n_number  = n_distinct(id_study))

age_cat_teen <- df %>% 
  filter(age_mean > 13 & age_mean <= 17) %>% 
  summarise(mean      = mean(age_mean, na.rm = T),
            median    = median(age_mean, na.rm = T),
            sd        = sd(age_mean, na.rm = T),
            n_number  = n_distinct(id_study))

age_cat_adult <- df %>% 
  filter(age_mean > 18 & age_mean <= 40) %>% 
  summarise(mean      = mean(age_mean, na.rm = T),
            median    = median(age_mean, na.rm = T),
            sd        = sd(age_mean, na.rm = T),
            n_number  = n_distinct(id_study))

age_cat_aged <- df %>% 
  filter(age_mean > 40) %>% 
  summarise(mean      = mean(age_mean, na.rm = T),
            median    = median(age_mean, na.rm = T),
            sd        = sd(age_mean, na.rm = T),
            n_number  = n_distinct(id_study))

# Total number of experiments --------------------------------------------------

n_experiments <- df %>% 
  summarise(n = n_distinct(id_study))

# Total number of studies ------------------------------------------------------

n_records <- df %>% 
  summarise(n = n_distinct(id_record))


# Total number of effects ------------------------------------------------------

n_effects <- nrow(df)

# Control Item type-------------------------------------------------------------

control_type <- df %>% 
  group_by(control_type) %>%
  summarise(n_number = n_distinct(id_study))

# n number of proportion designs------------------------------------------------

df %>% 
  filter(!is.na(accuracy_control_prop)) %>% 
  NROW()


# Items control ----------------------------------------------------------------

control_items <- df %>% 
  filter(is.na(accuracy_control_prop)) %>% 
  summarise(mean_co   = mean(items_control, na.rm = T),
            median_co = median(items_control, na.rm = T),
            sd_co     = sd(items_control, na.rm = T),
            n_number  = n_distinct(items_control, na.rm = T))

# Items misled -----------------------------------------------------------------

misled_items <- df %>% 
  filter(is.na(accuracy_control_prop)) %>% 
  summarise(mean      = mean(items_misled, na.rm = T),
            median    = median(items_misled, na.rm = T),
            sd        = sd(items_misled, na.rm = T),
            n_number  = n_distinct(items_misled, na.rm = T))
misled_items

# Number of initial test studies ----------------------------------------------- 

RES_design <- df %>% 
  filter(postevent_recall >= 1) %>% 
  summarise(n_number  = n_distinct(id_study, na.rm = T),
            mean      = mean(postevent_recall),
            median    = median(postevent_recall),
            min       = min(postevent_recall),
            max       = max(postevent_recall))
  

#number of post-exposure test studies------------------------------------------- 

postex_test <- df %>% 
  filter(postexposure_recall >= 1) %>% 
  summarise(n_number  = n_distinct(id_study, na.rm = T),
            mean      = mean(postexposure_recall),
            median    = median(postexposure_recall),
            min       = min(postexposure_recall),
            max       = max(postexposure_recall))

# Materials --------------------------------------------------------------------

materials <- df %>% 
  group_by(event_materials) %>%
  summarise(n_number = n_distinct(id_study))

materials %>% 
  filter(n_number < 8) %>% 
  summarise(n = sum(n_number))

# Event Medium -----------------------------------------------------------------

event_medium <- df %>% 
  group_by(event_medium) %>%
  summarise(n_number = n_distinct(id_study))

event_total <- sum(event_medium$n_number)

# Post event imformation -------------------------------------------------------

PEI <- df %>% 
  group_by(exposure_method) %>%
  summarise(n_number = n_distinct(id_study))
PEI

# Post event information--------------------------------------------------------

test_type <- df %>% 
  group_by(test_type) %>%
  summarise(n_number = n_distinct(id_study))
  
# Within/between ---------------------------------------------------------------

bet <- data %>% filter(within_between == 'between')
wit <- data %>% filter(within_between == 'within')

# Open-science practices -------------------------------------------------------

open_data <- df %>% 
  group_by(open_data) %>%
  summarise(n_number = n_distinct(id_study))

open_data_c <- df %>% 
  group_by(open_data_claimed) %>%
  summarise(n_number = n_distinct(id_study))

pre_reg <- df %>% 
  group_by(preregistered) %>%
  summarise(n_number = n_distinct(id_study))

open_material <- df %>% 
  group_by(open_materials) %>%
  summarise(n_number = n_distinct(id_study))
