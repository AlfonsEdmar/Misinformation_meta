################################################################################

# Misinformation Meta-analysis - Characteristics of the Literature

################################################################################

# Load packages ----------------------------------------------------------------

library(tidyverse)
library(dplyr)

# Load data --------------------------------------------------------------------

chr <- read_csv("./data/misinformation_clean_data.csv")

# Population characteristics ----------------------------------------------------------------

# Study-level total N

unique_study_n <- chr %>% distinct(id_study, .keep_all = TRUE)

study_total_n <- sum(unique_study_n$n_total, na.rm = TRUE)
print(study_total_n)

# Age

summary(chr$age_mean)
summary(na.omit(chr$age_mean))
clean_age_mean <- na.omit(chr$age_mean)
na.omit(chr$age_mean)
as.numeric(chr$age_mean)
summary(chr$age_mean)
summary(is.na(chr$age_mean))
mean(chr$age_mean, na.rm = TRUE) 


# Gender distribution (prop. female)

mean(chr$gender_female_prop, na.rm = TRUE) 
sd(chr$gender_female_prop, na.rm = TRUE) 
median(chr$gender_female_prop, na.rm = TRUE)

# Countries
chr %>%
  distinct(id_study, .keep_all = TRUE) %>%
  group_by(country) %>%
  summarize(total_n = sum(n_total, na.rm = TRUE), count = n(), .groups = 'drop')
print()

#Population recruitment
unique_studies <- chr %>%
  distinct(id_study, .keep_all = TRUE)

population_summary <- unique_studies %>%
  group_by(population) %>%
  summarize(total_n = sum(n_total, na.rm = TRUE), count = n(), .groups = 'drop')
print(population_summary)

#Incentives
unique_incentives <- chr %>%
  distinct(id_study, .keep_all = TRUE)

incentives_study <- unique_incentives %>%
  group_by(incentives) %>%
  summarize(count = n(), .groups = 'drop')

total_incentives <- unique_incentives %>%
  summarize(total_count = n())
print(incentives_study)


# Record characteristics
----------------------------------------------------------------
  
#Preregistration
chr%>%
  filter(preregistered == 1) %>%
  distinct(id_record, .keep_all = TRUE)

#Open data
chr %>%
  filter(open_data == 1) %>%
  distinct(id_study, .keep_all = TRUE)

#Open materials
chr %>%
  filter(open_materials == 1) %>%
  distinct(id_study, .keep_all = TRUE)

#Year of publication

conflicting_values <- chr %>%
  filter(!is.na(publication_year) & !is.na(year) & publication_year != year)
print(conflicting_values)

unique_records <- chr %>%
  distinct(id_record, .keep_all = TRUE)

year_record <- unique_records %>%
  group_by(year) %>%
  summarize(count = n(), .groups = 'drop') %>%
  arrange(desc(count))
print(year_record, n = Inf)


#Modality
unique_studies <- chr %>%
  distinct(id_study, .keep_all = TRUE)

modality_study <- unique_modality %>%
  group_by(modality) %>%
  summarize(count = n(), .groups = 'drop')

total_studies <- unique_modality %>%
  summarize(total_count = n())
print(modality_study)

#Retention interval 



