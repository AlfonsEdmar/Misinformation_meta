################################################################################

# Misinformation Meta-analysis - Characteristics of the Literature

################################################################################

# Load packages ----------------------------------------------------------------

library(tidyverse)

# Load data --------------------------------------------------------------------

chr <- read_csv("./data/misinformation_data_raw.csv")

# Population characteristics ----------------------------------------------------------------

# Study-level total N


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

chr[575, "gender_female_prop"] = 0.788
chr[576, "gender_female_prop"] = 0.788
chr[577, "gender_female_prop"] = 0.788
chr[578, "gender_female_prop"] = 0.788
chr[425, "gender_female_prop"] = 0.714

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







