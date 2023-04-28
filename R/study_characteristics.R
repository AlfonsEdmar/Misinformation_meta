library(tidyverse)
library(flextable)


df <- read_csv("data/complete_data_cleaned.csv")

# Population and age table 
summary_population<- df %>%
  select(n_total, population, age_mean) %>% 
  group_by(as.factor(population)) %>%
  summarise(total_sample = sum(n_total),
            mean_sample = mean(n_total),
            sd_sample = sd(n_total)) 

summary_age <- df %>%
  select(age_mean, population) %>% 
  group_by(as.factor(population)) %>% 
  filter(complete.cases(age_mean) == T)%>% 
  summarise(mean_age = (mean(age_mean)),
            sd_age = sd(age_mean))

summary_population <- cbind(summary_age, summary_population)[-c(4)]
summary_population[,2:6] <- round(summary_population[,2:6], 3)
ft_pop <- flextable(summary_population)

# Add formatting to the table
ft_pop <- ft_pop %>%
  width(j = 1:4, width = 1.3) %>%
  height(height = 0.5, part = "header") %>%
  bold(part = "header") %>%
  align(align = "center", part = "all")

print(ft_pop)

# country and number of studies 

summary_country <- chr %>% 
  select(n_total, country, id_record) %>% 
  group_by(country) %>%
  summarise(number_of_studies = n_distinct(id_record)) %>% 
  arrange(number_of_studies)

ft_country <- flextable(summary_country)
print(ft_country)


#Items total 
total_items <- df %>% 
  summarise(mean      = mean(items_total, na.rm = T),
            median    = median(items_total, na.rm = T),
            sd        = sd(items_total, na.rm = T),
            n_number  = n_distinct(items_total, na.rm = T))

#Postevent retention intervall 
postev_ret <- df %>% 
  filter(postevent_retention_interval >= 1) %>% 
  summarise(mean      = mean(postevent_retention_interval, na.rm = T),
            median    = median(postevent_retention_interval, na.rm = T),
            sd        = sd(postevent_retention_interval, na.rm = T),
            min       = min(postevent_retention_interval, na.rm = T),
            max       = max(postevent_retention_interval, na.rm = T),
            n_number  = n_distinct(id_study, na.rm = T))
postev_ret


#Postexposure retention interval 
postex_ret <- df %>% 
  filter(postexposure_retention_interval >= 1) %>% 
  summarise(mean      = mean(postexposure_retention_interval, na.rm = T),
            median    = median(postexposure_retention_interval, na.rm = T),
            sd        = sd(postexposure_retention_interval, na.rm = T),
            min       = min(postexposure_retention_interval, na.rm = T),
            max       = max(postexposure_retention_interval, na.rm = T),
            n_number  = n_distinct(id_study, na.rm = T))
postex_ret


# female prop
gender_prop <- df %>% 
  summarise(mean      = mean(gender_female_prop, na.rm = T),
            median    = median(items_control, na.rm = T),
            sd_co     = sd(items_control, na.rm = T),
            n_number  = n_distinct(items_control, na.rm = T))

# Age Categories 
age_cat_preschool <- df %>% 
  filter(age_mean <= 5) %>% 
  summarise(mean      = mean(age_mean, na.rm = T),
            median    = median(age_mean, na.rm = T),
            sd        = sd(age_mean, na.rm = T),
            n_number  = n_distinct(id_study))
age_cat_preschool

age_cat_school <- df %>% 
  filter(age_mean > 5 & age_mean <= 12) %>% 
  summarise(mean      = mean(age_mean, na.rm = T),
            median    = median(age_mean, na.rm = T),
            sd        = sd(age_mean, na.rm = T),
            n_number  = n_distinct(id_study))
age_cat_school

age_cat_teen <- df %>% 
  filter(age_mean > 13 & age_mean <= 17) %>% 
  summarise(mean      = mean(age_mean, na.rm = T),
            median    = median(age_mean, na.rm = T),
            sd        = sd(age_mean, na.rm = T),
            n_number  = n_distinct(id_study))
age_cat_teen

age_cat_adult <- df %>% 
  filter(age_mean > 18 & age_mean <= 40) %>% 
  summarise(mean      = mean(age_mean, na.rm = T),
            median    = median(age_mean, na.rm = T),
            sd        = sd(age_mean, na.rm = T),
            n_number  = n_distinct(id_study))
age_cat_adult

age_cat_aged <- df %>% 
  filter(age_mean > 40) %>% 
  summarise(mean      = mean(age_mean, na.rm = T),
            median    = median(age_mean, na.rm = T),
            sd        = sd(age_mean, na.rm = T),
            n_number  = n_distinct(id_study))
age_cat_aged

# total number of experiments 
n_experiments <- df %>% 
  summarise(n = n_distinct(id_study))
n_experiments

# total number of studies
n_records <- df %>% 
  summarise(n = n_distinct(id_record))
n_records

# total number of effects
n_effects <- NROW(df)

# Control Item type
control_type <- df %>% 
  group_by(control_type) %>%
  summarise(n_number = n_distinct(id_record))
control_type

# n number of proportion designs 
df %>% 
  filter(!is.na(accuracy_control_prop)) %>% 
  NROW()


#Items control 
control_items <- df %>% 
  filter(is.na(accuracy_control_prop)) %>% 
  summarise(mean_co   = mean(items_control, na.rm = T),
            median_co = median(items_control, na.rm = T),
            sd_co     = sd(items_control, na.rm = T),
            n_number  = n_distinct(items_control, na.rm = T))
control_items

#Items misled 
misled_items <- df %>% 
  filter(is.na(accuracy_control_prop)) %>% 
  summarise(mean      = mean(items_misled, na.rm = T),
            median    = median(items_misled, na.rm = T),
            sd        = sd(items_misled, na.rm = T),
            n_number  = n_distinct(items_misled, na.rm = T))
misled_items

#number of initial test studies  
RES_design <- df %>% 
  filter(postevent_recall >= 1) %>% 
  summarise(n_number  = n_distinct(id_study, na.rm = T),
            mean      = mean(postevent_recall),
            median    = median(postevent_recall),
            min       = min(postevent_recall),
            max       = max(postevent_recall))
RES_design  

#number of post-exposure test studies  
postex_test <- df %>% 
  filter(postexposure_recall >= 1) %>% 
  summarise(n_number  = n_distinct(id_study, na.rm = T),
            mean      = mean(postexposure_recall),
            median    = median(postexposure_recall),
            min       = min(postexposure_recall),
            max       = max(postexposure_recall))
postex_test












