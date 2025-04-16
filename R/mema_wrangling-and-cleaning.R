################################################################################

# Misinformation Meta-analysis - initial cleaning v2

################################################################################

# Load packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(missForest)
library(metafor)

# Load data --------------------------------------------------------------------

raw <- read_xlsx("data/misinformation_full_data_2024.xlsx", sheet = 1)

# Redo IDs ---------------------------------------------------------------------

records_old_new <- data.frame(
  old = unique(raw$id_record), 
  new = str_pad(1:length(unique(raw$id_record)), width = 3, pad = "0")
)

raw <- raw %>% 
  extract(
    col = "id_control",
    regex = ".*_(.*_.*)",
    into = "id_temp",
    remove = FALSE
  )

raw <- raw %>% 
  left_join(records_old_new, by = c("id_record" = "old"))

raw$id_control <- paste(raw$new, raw$id_temp, sep = "_")

raw$id_record <- raw$new

raw <- raw %>% 
  extract(
    col = "id_control",
    regex = "(.*_.*)_.*",
    into = "id_study",
    remove = FALSE
  )

raw <- raw %>% 
  select(
    id_record, id_study, id_control,
    everything(),
    -id_temp, -new
  )

raw$id_effect <- str_pad(1:nrow(raw), width = 4, pad = "0")

# Replace NA with 0 for open_science variables ---------------------------------

raw$preregistered[is.na(raw$preregistered)] <- 0

raw$open_data[is.na(raw$open_data)] <- 0

raw$open_data_claimed[is.na(raw$open_data_claimed)] <- 0

raw$open_materials[is.na(raw$open_materials)] <- 0

# Recode population variable ---------------------------------------------------

raw <- raw %>% 
  mutate(
    population = case_when(
      population == "students" | population == "student" ~ "students", 
      population == "mturk" | population == "MTurk"      ~  "mturk", 
      population == "prolific"                           ~ "prolific",
      TRUE                                               ~ "community"
    )
  )

raw <- raw %>% 
  mutate(
    population = case_when(
      population == "students" & age_mean < 18           ~ "community",
      population == "students" & age_mean >= 18          ~ "students",
      TRUE                                               ~ population
    )
  )

# Check for typos --------------------------------------------------------------

unique_values <- apply(raw, 2, unique)

raw$within_between[raw$within_between == "betwen"] <- "between"

raw$incentives[raw$incentives == "course_credit_or_ money"] <- "course_credit_or_money"
raw$incentives[raw$incentives == "credit_or_money"] <- "course_credit_or_money"
raw$incentives[raw$incentives == "assignment"] <- "required"
raw$incentives[raw$incentives == "other"] <- "goods"

raw$accuracy_type[raw$accuracy_type == "propotion"] <- "proportion"

raw$control_type[raw$control_type == "nautral"] <- "neutral"
raw$control_type[raw$control_type == "no misinformation"] <- "no_misinformation"
raw$control_type[raw$control_type == "constistent"] <- "consistent"

raw$event_medium[raw$event_medium == "audivisual"] <- "audiovisual"
raw$event_medium[raw$event_medium == "audio_visual"] <- "audiovisual"
raw$event_medium[raw$event_medium == "various"] <- NA
raw$event_medium[raw$event_medium == "NA"] <- NA
raw$event_medium[raw$event_medium == "taste"] <- "live"

raw$test_type[raw$test_type == "cued_recal"] <- "cued_recall"
raw$test_type[raw$test_type == "modified"] <- "modified_test"
raw$test_type[raw$test_type == "MMFI"] <- "MMFR"
raw$test_type[raw$test_type == "prop-selection"] <- "recognition"
raw$test_type[raw$test_type == "recogition"] <- "recognition"


raw$event_materials[
  raw$event_materials == "theft_narrative_mccloskey_zaragoza_1985a"] <- 
  "theft_narrative_mccloskey_zaragoza_1985" 

raw$event_materials[
  raw$event_materials == "theft_narrative_mccloskey_zaragoza_1985a"] <- 
  "theft_narrative_mccloskey_zaragoza_1985"
raw$event_materials[
  raw$event_materials == "minor_theft_films_adams_price_1990"] <- 
  "minor_theft_films_adams_price_1989"
raw$event_materials[
  raw$event_materials == "minor_theft_films_adams_price_1991"] <- 
  "minor_theft_films_adams_price_1989"
raw$event_materials[
  raw$event_materials == "minor_theft_films_adams_price_1992"] <- 
  "minor_theft_films_adams_price_1989"
raw$event_materials[
  raw$event_materials == "minor_theft_films_adams_price_1993"] <- 
  "minor_theft_films_adams_price_1989"
raw$event_materials[
  raw$event_materials == "minor_theft_films_adams_price_1994"] <- 
  "minor_theft_films_adams_price_1989"
raw$event_materials[
  raw$event_materials == "minor_theft_films_adams_price_1995"] <- 
  "minor_theft_films_adams_price_1989"
raw$event_materials[
  raw$event_materials == "minor_theft_films_adams_price_1996"] <- 
  "minor_theft_films_adams_price_1989"
raw$event_materials[
  raw$event_materials == "minor_theft_films_adams_price_1997"] <- 
  "minor_theft_films_adams_price_1989"
raw$event_materials[
  raw$event_materials == "minor_theft_films_adams_price_1998"] <- 
  "minor_theft_films_adams_price_1989"
raw$event_materials[
  raw$event_materials == "minor_theft_films_adams_price_1999"] <- 
  "minor_theft_films_adams_price_1989"
raw$event_materials[
  raw$event_materials == "minor_theft_films_adams_price_2000"] <- 
  "minor_theft_films_adams_price_1989"
raw$event_materials[
  raw$event_materials == "minor_theft_films_adams_price_2001"] <- 
  "minor_theft_films_adams_price_1989"
raw$event_materials[
  raw$event_materials == "minor_theft_films_adams_price_2002"] <- 
  "minor_theft_films_adams_price_1989"
raw$event_materials[
  raw$event_materials == "minor_theft_films_adams_price_2003"] <- 
  "minor_theft_films_adams_price_1989"
raw$event_materials[
  raw$event_materials == "minor_theft_films_adams_price_2004"] <- 
  "minor_theft_films_adams_price_1989"
raw$event_materials[
  raw$event_materials == "minor_theft_films_adams_price_2005"] <- 
  "minor_theft_films_adams_price_1989"
raw$event_materials[
  raw$event_materials == "minor_theft_films_adams_price_2006"] <- 
  "minor_theft_films_adams_price_1989"
raw$event_materials[
  raw$event_materials == "theft_slides_mccloskey_zaragoza_1987"] <- 
  "theft_slides_mccloskey_zaragoza_1986"
raw$event_materials[
  raw$event_materials == "theft_slides_mccloskey_zaragoza_1988"] <- 
  "theft_slides_mccloskey_zaragoza_1986"
raw$event_materials[
  raw$event_materials == "McClosky_Zaragoza_1986"] <- 
  "McClosky_Zaragoza_1985"
raw$event_materials[
  raw$event_materials == "play_session_price_2005"] <- 
  "play_session_price_2004"
raw$event_materials[
  raw$event_materials == "zaragoza_girl_in_park_1992"] <- 
  "zaragoza_girl_in_park_1991"
raw$event_materials[
  raw$event_materials == "zaragoza_girl_in_park_1993"] <- 
  "zaragoza_girl_in_park_1991"
raw$event_materials[
  raw$event_materials == "zaragoza_girl_in_park_1994"] <- 
  "zaragoza_girl_in_park_1991"
raw$event_materials[
  raw$event_materials == "zaragoza_girl_in_park_1995"] <- 
  "zaragoza_girl_in_park_1991"
raw$event_materials[
  raw$event_materials == "zaragoza_girl_in_park_1996"] <- 
  "zaragoza_girl_in_park_1991"

raw$exposure_method[
  raw$exposure_method == "narratvie"] <- "narrative"
raw$exposure_method[
  raw$exposure_method == "questionair"] <- "questionnaire"
raw$exposure_method[
  raw$exposure_method == "questionnair"] <- "questionnaire"


raw$country[raw$country == "en"] <- "gb"
raw$country[raw$country == "UK"] <- "gb"
raw$country[raw$country == "US"] <- "us"

raw$control_type[raw$control_type == "constistent"] <- "consistent"
raw$control_type[raw$control_type == "no misinformation"] <- "no_misinformation"

raw$incentives[raw$incentives == "course_requirement"] <- "required"

# Setting variables classes ----------------------------------------------------

raw$publication_year     <- as.numeric(raw$publication_year)

raw$language_record      <- as.factor(raw$language_record)
raw$language_materials   <- as.factor(raw$language_materials)
raw$country              <- as.factor(raw$country)
raw$population           <- as.factor(raw$population)
raw$modality             <- as.factor(raw$modality)
raw$incentives           <- as.factor(raw$incentives)

raw$n_total              <- as.numeric(raw$n_total)
raw$n_control            <- as.numeric(raw$n_control)
raw$n_mi                 <- as.numeric(raw$n_mi)

raw$within_between       <- as.factor(raw$within_between)
raw$accuracy_type        <- as.factor(raw$accuracy_type)
raw$control_type         <- as.factor(raw$control_type)

raw$age_mean             <- as.numeric(raw$age_mean)
raw$age_median           <- as.numeric(raw$age_median)
raw$age_sd               <- as.numeric(raw$age_sd)
raw$age_min              <- as.numeric(raw$age_min)
raw$age_max              <- as.numeric(raw$age_max)

raw$gender_female_prop   <- as.numeric(raw$gender_female_prop)

raw$preevent_valence     <- as.factor(raw$preevent_valence)

raw$event_medium         <- as.factor(raw$event_medium)

raw$exposure_medium      <- as.factor(raw$exposure_medium)
raw$exposure_method      <- as.factor(raw$exposure_method)
raw$exposure_valence     <- as.factor(raw$exposure_valence)

raw$test_medium          <- as.factor(raw$test_medium)
raw$test_type            <- as.factor(raw$test_type)
raw$item_centrality      <- as.factor(raw$item_centrality)

raw$accuracy_control_mean <- as.numeric(raw$accuracy_control_mean)
raw$accuracy_mi_mean      <- as.numeric(raw$accuracy_mi_mean)
raw$accuracy_control_sd   <- as.numeric(raw$accuracy_control_sd)
raw$accuracy_mi_sd        <- as.numeric(raw$accuracy_mi_sd)
raw$accuracy_control_prop <- as.numeric(raw$accuracy_control_prop)
raw$accuracy_mi_prop      <- as.numeric(raw$accuracy_mi_prop)

# Imputing missing variances ---------------------------------------------------

set.seed(12343)

# Dividing sample proportion and mean accuracy output data

prop_data <- raw %>%
  filter(!is.na(accuracy_control_prop))

mean_data <- raw %>%
  filter(!is.na(accuracy_control_mean))

# Constucting a predictor and missing observations matrix

data <- mean_data %>%
  filter(is.na(accuracy_control_prop)) %>%
  select(accuracy_control_mean, accuracy_mi_mean,
         accuracy_control_sd , accuracy_mi_sd ,
         n_total, n_control, n_mi)

# Imputing data with missForest

imp            <- missForest(as.matrix(data))
index          <- ifelse(is.na(data$accuracy_control_sd), 1, 0)
imp_data       <- cbind(as.data.frame(imp$ximp), index)
imp_data$index <- factor(imp_data$index)

# Checking output 

ggplot(imp_data %>% 
         filter(accuracy_control_sd < 15), 
       aes(
         col = index, 
         group = index)
  ) +
  geom_point(
    aes(
      x = accuracy_control_sd, 
      y = accuracy_mi_sd), 
    alpha = .4
  ) +
  geom_smooth(
    aes(
      x = accuracy_control_sd,
      y = accuracy_mi_sd), 
    method = 'lm'
  ) +
  theme_bw()


# Wrangling an indicator for imputation

mean_data                         <- cbind(mean_data, imp_data$index)
names(mean_data)[ncol(mean_data)] <-  'sd_imputed'
prop_data$sd_imputed              <- rep(0, nrow(prop_data))

mean_data$accuracy_control_sd <- imp_data$accuracy_control_sd
mean_data$accuracy_mi_sd      <- imp_data$accuracy_mi_sd


# Calculating effect sizes for meta-analysis -----------------------------------

## Calculating log odds ratio

es_prop <- escalc(data    = prop_data,
                  ci      = accuracy_mi_prop * n_mi,
                  di      = (1 - accuracy_mi_prop) * n_mi,
                  ai      = accuracy_control_prop * n_control,
                  bi      = (1 - accuracy_control_prop) * n_control,
                  measure = 'OR')

### Transforming log odds to standardized mean difference

es_prop <- es_prop %>% 
  mutate(yi = yi * (sqrt(3)/pi),
         vi = vi * (3/(pi^2)))

## Calculating Hedges's g (standardized mean difference)

es_mean <- escalc(data    = mean_data,
                  m1i     = accuracy_control_mean,
                  m2i     = accuracy_mi_mean,
                  sd1i    = accuracy_control_sd,
                  sd2i    = accuracy_mi_sd,
                  n1i     = n_control,
                  n2i     = n_mi,
                  measure = 'SMD')

### Reverse effects as necessary

es_mean <- es_mean %>% 
  mutate(
    yi = case_when(
      accuracy_reverse == 0 ~  yi,
      accuracy_reverse == 1 ~ -yi
    )
  )

## Bind data

es_prop$sd_imputed <- as.factor(es_prop$sd_imputed)

data_es <- rbind(es_mean, es_prop)

# There are some missing values

miss <- data_es %>% 
  filter(is.na(yi))

data_es <- data_es %>%
  filter(!is.na(yi)) %>%
  # Whaley (1988) has one effect that is misreported. The mean accuracy exceeds
  # the reported total number of items on the test. This caused a large effect
  # to be estimated, which we are removing here.
  filter(yi > -7)

# Exporting cleaned data -------------------------------------------------------

# Write a csv file

write.csv(data_es, 'data/misinformation_clean_data.csv')
