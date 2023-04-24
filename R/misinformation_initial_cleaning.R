################################################################################

# Misinformation Meta-analysis - initial cleaning

################################################################################

# Load packages ----------------------------------------------------------------

library(tidyverse)
library(rcrossref)
library(bib2df)

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

raw$publication_year[raw$publication_year == "1993a"] <- "1993"
raw$publication_year[raw$publication_year == "1999a"] <- "1999"
raw$publication_year[raw$publication_year == "1999b"] <- "1999"
raw$publication_year[raw$publication_year == "2003a"] <- "2003"
raw$publication_year[raw$publication_year == "2003b"] <- "2003"
raw$publication_year[raw$publication_year == "2006b"] <- "2006"
raw$publication_year[raw$publication_year == "2011a"] <- "2011"
raw$publication_year[raw$publication_year == "2015a"] <- "2015"
raw$publication_year[raw$publication_year == "2015B"] <- "2015"
raw$publication_year[raw$publication_year == "2021b"] <- "2021"

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

raw$country[raw$country == "en"] <- "gb"

# Setting variables classes-----------------------------------------------------

raw$publication_year     <- as.numeric(raw$publication_year)

raw$language_record      <- as.factor(raw$language_record)
raw$language_materials   <- as.factor(raw$language_materials)
raw$country              <- as.factor(raw$country)
raw$population           <- as.factor(raw$population)
raw$modality             <- as.factor(raw$modality)
raw$incentives           <- as.factor(raw$incentives)

raw$within_between       <- as.factor(raw$within_between)
raw$accuracy_type        <- as.factor(raw$accuracy_type)
raw$control_type         <- as.factor(raw$control_type)

raw$age_mean             <- as.numeric(raw$age_mean)

raw$preevent_valence     <- as.factor(raw$preevent_valence)

raw$event_medium         <- as.factor(raw$event_medium)

raw$exposure_medium      <- as.factor(raw$exposure_medium)
raw$exposure_method      <- as.factor(raw$exposure_method)
raw$exposure_valence     <- as.factor(raw$exposure_valence)

raw$test_medium          <- as.factor(raw$test_medium)
raw$test_type            <- as.factor(raw$test_type)
raw$item_centrality      <- as.factor(raw$item_centrality)

# Retrieving reference information from DOIs -----------------------------------

if (!file.exists("data/misinformation_citations.bib")) {
  
  doi_unique <- unique(raw$doi)
  
  citations <- cr_cn(
    dois = doi_unique,
    format = "bibtex",
    style = "apa"
  )
  
  writeLines(unlist(citations), "data/misinformation_citations.bib")
  
}

citation_data <- bib2df("data/misinformation_citations.bib")

colnames(citation_data) <- tolower(colnames(citation_data))

citation_data$category <- tolower(citation_data$category)

citation_clean <- citation_data %>% 
  select(
    bibtexkey, category, author, title, year, journal, doi
  )

raw_cite <- raw %>% 
  select(
    -title, -container, -record_type
  ) %>% 
  left_join(citation_clean, by = "doi") %>% 
  select(
    id_record, id_study, id_control, id_effect,
    authors, publication_year,
    bibtexkey, category, author, title, year, journal, doi,
    everything()
    )

## Transform author list column into a character column

raw <- raw %>% 
  select(
    -title, -container, -record_type
  ) %>% 
  left_join(citation_clean, by = "doi") %>% 
  select(
    id_record, id_study, id_control, id_effect,
    authors, publication_year,
    bibtexkey, category, author, title, year, journal, doi,
    everything()
  )

raw$author <- map_chr(raw_cite$author, function(x) {paste(unlist(x), sep = " ", collapse = "; ")})

# Exporting cleaned data--------------------------------------------------------

# Write a csv file

write.csv(raw, 'data/misinformation_data_cleaned.csv')

# Write an RDS file

saveRDS(raw_cite, "data/misinformation_data_cleaned.rds")
