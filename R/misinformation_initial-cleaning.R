################################################################################

# Misinformation Meta-analysis - initial cleaning

################################################################################

# Load packages ----------------------------------------------------------------

library(tidyverse)

# Load data --------------------------------------------------------------------

raw <- read_csv("./data/misinformation_data_raw.csv")

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
raw$accuracy_type[raw$accuracy_type == "no misinformation"] <- "no_misinformation"
raw$accuracy_type[raw$accuracy_type == "constistent"] <- "consistent"

raw$event_medium[raw$event_medium == "audivisual"] <- "audiovisual"
raw$event_medium[raw$event_medium == "audio_visual"] <- "audiovisual"
raw$event_medium[raw$event_medium == "various"] <- NA
raw$event_medium[raw$event_medium == "taste"] <- "live"

raw$test_type[raw$test_type == "cued_recal"] <- "cued_recall"
raw$test_type[raw$test_type == "modified"] <- "modified_test"
raw$test_type[raw$test_type == "MMFI"] <- "MMFR"
raw$test_type[raw$test_type == "prop-selection"] <- "recognition"
raw$test_type[raw$test_type == "recogition"] <- "recognition"


