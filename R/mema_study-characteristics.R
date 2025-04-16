################################################################################

# Misinformation Meta-analysis - Study Characteristics

################################################################################

# Load packages ----------------------------------------------------------------

library(tidyverse)
library(metafor)
library(flextable)

# Load data --------------------------------------------------------------------

# All studies fit in the primary model
df <- read_csv("data/misinformation_included_data.csv")
df_2 <- readxl::read_xlsx("data/misinformation_full_data_2024.xlsx")
mema_primary <- read_rds('output/mema_primary.rds')
# Country and number of studies------------------------------------------------- 

summary_country <- df %>% 
  group_by(country) %>%
  summarise(number_of_records = n_distinct(id_record),
            number_of_studies = n_distinct(id_study),
            number_of_effects = n_distinct(id_effect)) %>% 
  arrange(desc(number_of_studies))

flextable(summary_country) %>% 
  bold(part = "header", bold = TRUE) %>% 
  theme_apa()

df %>% summarise(number_of_records = n_distinct(id_record),
                 number_of_studies = n_distinct(id_study),
                 number_of_control = n_distinct(id_control),
                 number_of_effects = NROW(df))%>%
  glimpse()

df %>%
  distinct(id_study, .keep_all = TRUE) %>% 
  group_by(population) %>% 
  summarise(tota_sample_size = sum(n_total))

df %>%
  group_by(category) %>% 
  summarise(n_studies = n_distinct(id_record) )

na_category <- df %>%  filter(is.na(df$category))
#Items total--------------------------------------------------------------------

df %>% 
  summarise(mean      = mean(items_total, na.rm = T),
            median    = median(items_total, na.rm = T),
            sd        = sd(items_total, na.rm = T),
            n_number  = n_distinct(items_total, na.rm = T))%>%
  glimpse()

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

df %>% 
  summarise(female_mean = mean(gender_female_prop + .5, na.rm = T),
            female_median = median(gender_female_prop + .5, na.rm = T),
            female_sd = sd(gender_female_prop + .5, na.rm = T))


# Age Categories ---------------------------------------------------------------

df %>% 
  summarise(mean      = mean(age_mean, na.rm = T),
            median    = median(age_mean, na.rm = T))

df %>% 
  filter(age_mean <= 5) %>% 
  summarise(mean      = mean(age_mean, na.rm = T),
            median    = median(age_mean, na.rm = T),
            sd        = sd(age_mean, na.rm = T),
            n_number  = n_distinct(id_study),
            n_number_perc  = n_distinct(id_study)/428)

df %>% 
  filter(age_mean > 5 & age_mean <= 12) %>% 
  summarise(mean      = mean(age_mean, na.rm = T),
            median    = median(age_mean, na.rm = T),
            sd        = sd(age_mean, na.rm = T),
            n_number  = n_distinct(id_study),
            n_number_perc  = n_distinct(id_study)/428)

df %>% 
  filter(age_mean > 13 & age_mean <= 17) %>% 
  summarise(mean      = mean(age_mean, na.rm = T),
            median    = median(age_mean, na.rm = T),
            sd        = sd(age_mean, na.rm = T),
            n_number  = n_distinct(id_study),
            n_number_perc  = n_distinct(id_study)/428)

df %>% 
  filter(age_mean > 18 & age_mean <= 40) %>% 
  summarise(mean      = mean(age_mean, na.rm = T),
            median    = median(age_mean, na.rm = T),
            sd        = sd(age_mean, na.rm = T),
            n_number  = n_distinct(id_study),
            n_number_perc  = n_distinct(id_study)/428)

df %>% 
  filter(age_mean > 40) %>% 
  summarise(mean      = mean(age_mean, na.rm = T),
            median    = median(age_mean, na.rm = T),
            sd        = sd(age_mean, na.rm = T),
            n_number  = n_distinct(id_study),
            n_number_perc  = n_distinct(id_study)/428)

# Note: we miss age means for 1114 effects
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
event_medium
event_total <- sum(event_medium$n_number)

# Post event information -------------------------------------------------------

PEI <- df %>% 
  group_by(exposure_method) %>%
  summarise(n_number = n_distinct(id_study))
PEI

# Control item types -----------------------------------------------------------
control_item_type<- df %>% 
  group_by(control_type) %>%
  summarise(n_number = n_distinct(id_study))
control_item_type
# Post event information--------------------------------------------------------

test_type <- df %>% 
  group_by(test_type) %>%
  summarise(n_number = n_distinct(id_study))


# Within/between ---------------------------------------------------------------

bet <- df %>% filter(within_between == 'between')
wit <- df %>% filter(within_between == 'within')

# Open-science practices -------------------------------------------------------

df %>% 
  group_by(open_data) %>%
  summarise(n_number = n_distinct(id_record))

open_data_c <- df %>% 
  group_by(open_data_claimed) %>%
  summarise(n_number = n_distinct(id_study))

df %>% 
  group_by(preregistered) %>%
  summarise(n_number = n_distinct(id_record))

df %>% 
  group_by(open_materials) %>%
  summarise(n_number = n_distinct(id_record))

# Incentives--------------------------------------------------------------------
df %>% 
  group_by(incentives) %>% 
  summarise(incentives_rec = n_distinct(id_record),
            incentives_stu = n_distinct(id_study),
            incentives_effect = n_distinct(id_effect))

# Outcome-----------------------------------------------------------------------

