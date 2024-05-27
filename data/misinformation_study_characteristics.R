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

#Countries

id_record <- as.numeric(chr$id_record)

clean_record_id <-data.frame(unique(chr$id_record))
table(clean_record_id$country)

chr %>%
  clean_record_id <- data.frame(
    distinct(id_record))
  

#Modality

#Incentives

#Retention interval 







