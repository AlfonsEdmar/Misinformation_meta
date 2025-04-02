################################################################################

# Misinformation Meta-analysis - Subgroup analysis  

################################################################################

# Load data --------------------------------------------------------------------

data_es <- read_csv('data/misinformation_clean_data.csv')

# Data transformations ---------------------------------------------------------

# Calculate and center moderator variables

data_es <- data_es %>% 
  mutate(
    gender_female_prop          = gender_female_prop - .50,
    publication_year            = publication_year - mean(publication_year, na.rm = TRUE),
    # Calculate control accuracy
    control_acc                 = case_when(
      !is.na(accuracy_control_mean) ~ accuracy_control_mean,
      is.na(accuracy_control_mean) ~ accuracy_control_prop
    ),
    control_acc = case_when(
      accuracy_type == "count"      ~ control_acc / items_control,
      accuracy_type == "proportion" ~ control_acc,
    ),
    # Handle edge cases where accuracy was calculated using total items instead
    # of specific item types
    control_acc = case_when(
      control_acc >  1 ~ accuracy_control_mean / items_total,
      control_acc <= 1 ~ control_acc
    ), 
    control_acc = control_acc - mean(control_acc, na.rm = TRUE)
  )

# Random effect missingness

data_es$event_materials[is.na(data_es$event_materials)] <- "missing"
data_es$country[is.na(data_es$country)]                 <- "missing"
data_es$control_type[is.na(data_es$control_type)]       <- "missing"
data_es$modality[is.na(data_es$modality)]               <- "missing"
data_es$population[is.na(data_es$population)]           <- "missing"
data_es$test_type[is.na(data_es$test_type)]             <- "missing"
data_es$test_medium[is.na(data_es$test_medium)]         <- "missing"
data_es$exposure_medium[is.na(data_es$exposure_medium)] <- "missing"

# Age analysis------------------------------------------------------------------ 

age_data <- data_es %>% 
  filter(!is.na(age_mean)) %>% 
  mutate(
    age_mean = age_mean - mean(age_mean, na.rm = TRUE)
  )

if (!file.exists('output/mema_age.rds')) {
  
  meta_age   <- rma.mv(yi      = yi, 
                       V       = vi,
                       random  = list(~1|id_record/id_study/id_control, 
                                      ~1|event_materials,
                                      ~1|country,
                                      ~1|control_type,
                                      ~1|modality,
                                      ~1|population,
                                      ~1|test_type,
                                      ~1|test_medium,
                                      ~1|exposure_medium),
                       mods    = ~ postevent_retention_interval
                       + age_mean
                       + postexposure_retention_interval
                       + preevent_warning
                       + postevent_warning
                       + postexposure_warning
                       + control_acc
                       + postevent_recall
                       + postexposure_recall
                       + publication_year
                       + preregistered,
                       data    = age_data,
                       method  = "REML", 
                       control = list(
                         iter.max  = 1000,
                         rel.tol   = 1e-8
                       ),
                       verbose = TRUE)
  
  saveRDS(meta_age, 'output/mema_age.rds')
  
} else {
  
  meta_age <- readRDS('output/mema_age.rds')
  
}

if (!file.exists('output/mema_quad_age.rds')) {
  
  meta_age_quad  <- rma.mv(yi      = yi, 
                           V       = vi,
                           random  = list(~1|id_record/id_study/id_control, 
                                          ~1|event_materials,
                                          ~1|country,
                                          ~1|control_type,
                                          ~1|modality,
                                          ~1|population,
                                          ~1|test_type,
                                          ~1|test_medium,
                                          ~1|exposure_medium),
                           mods    = ~ postevent_retention_interval
                           + age_mean
                           + I(age_mean^2)
                           + postexposure_retention_interval
                           + preevent_warning
                           + postevent_warning
                           + postexposure_warning
                           + control_acc
                           + postevent_recall
                           + postexposure_recall
                           + publication_year
                           + preregistered,
                           data    = age_data,
                           method  = "REML", 
                           control = list(
                             iter.max  = 1000,
                             rel.tol   = 1e-8
                           ),
                           verbose = TRUE)
  
  saveRDS(meta_age_quad, 'output/mema_quad_age.rds')
  
} else {
  
  meta_age_quad <- readRDS('output/mema_quad_age.rds')
  
}

if (!file.exists('output/mema_age_no_accuracy.rds')) {
  
  meta_age_no_acc  <- rma.mv(yi      = yi, 
                             V       = vi,
                             random  = list(~1|id_record/id_study/id_control, 
                                            ~1|event_materials,
                                            ~1|country,
                                            ~1|control_type,
                                            ~1|modality,
                                            ~1|population,
                                            ~1|test_type,
                                            ~1|test_medium,
                                            ~1|exposure_medium),
                             mods    = ~ postevent_retention_interval
                             + age_mean
                             + postexposure_retention_interval
                             + preevent_warning
                             + postevent_warning
                             + postexposure_warning
                             + postevent_recall
                             + postexposure_recall
                             + publication_year
                             + preregistered,
                             data    = age_data,
                             method  = "REML", 
                             control = list(
                               iter.max  = 1000,
                               rel.tol   = 1e-8
                             ),
                             verbose = FALSE)
  
  saveRDS(meta_age_no_acc, 'output/mema_age_no_accuracy.rds')
  
} else {
  
  meta_age_no_acc <- readRDS('output/mema_age_no_accuracy.rds')
  
}

# Categorical age

breaks <- c(0, 5, 18, 40, max(data_es$age_mean, na.rm = TRUE))
labels <- c("0-5", "6-17", "17-40", "41+")
age_data$age_cat <- cut(age_data$age_mean, breaks = breaks, labels = labels, include.lowest = TRUE)
age_data$age_cat <- relevel(age_data$age_cat, '17-40')

if (!file.exists('output/mema_age_cat.rds')) {
  
  meta_age_cat   <- rma.mv(yi      = yi, 
                           V       = vi,
                           random  = list(~1|id_record/id_study/id_control, 
                                          ~1|event_materials,
                                          ~1|country,
                                          ~1|control_type,
                                          ~1|modality,
                                          ~1|population,
                                          ~1|test_type,
                                          ~1|test_medium,
                                          ~1|exposure_medium),
                           mods    = ~ postevent_retention_interval
                           + age_cat
                           + postexposure_retention_interval
                           + preevent_warning
                           + postevent_warning
                           + postexposure_warning
                           + control_acc
                           + postevent_recall
                           + postexposure_recall
                           + publication_year
                           + preregistered,
                           data    = age_data,
                           method  = "REML", 
                           control = list(
                             iter.max  = 1000,
                             rel.tol   = 1e-8
                           ),
                           verbose = FALSE)
  
  saveRDS(meta_age_cat, 'output/mema_age_cat.rds')
  
} else {
  
  meta_age_cat <- readRDS('output/mema_age_cat.rds')
  
}

# Proportion female ------------------------------------------------------------

if (!file.exists("output/mema_prop_female.rds")) {
  
  meta_prop_female   <- rma.mv(yi      = yi, 
                               V       = vi,
                               random  = list(~1|id_record/id_study/id_control, 
                                              ~1|event_materials,
                                              ~1|country,
                                              ~1|control_type,
                                              ~1|modality,
                                              ~1|population,
                                              ~1|test_type,
                                              ~1|test_medium,
                                              ~1|exposure_medium),
                               mods    = ~ postevent_retention_interval
                               + postexposure_retention_interval
                               + preevent_warning
                               + postevent_warning
                               + postexposure_warning
                               + control_acc
                               + postevent_recall
                               + postexposure_recall
                               + publication_year
                               + preregistered
                               + gender_female_prop,
                               data    = data_es,
                               method  = "REML", 
                               control = list(
                                 iter.max  = 1000,
                                 rel.tol   = 1e-8
                               ),
                               verbose = TRUE)
  
  saveRDS(meta_prop_female, "output/mema_prop_female.rds")
  
} else {
  
  meta_prop_female <- readRDS("output/mema_prop_female.rds")
  
}

# Incentives -------------------------------------------------------------------

incent_data <- data_es %>% filter(!is.na(data_es$incentives))
incent_data$incentives <- as.factor(incent_data$incentives)

# Setting the reference to be no incentive

incent_data <- within(incent_data, incentives <- relevel(incentives, 6))

if (!file.exists("output/mema_incent.rds")) {
  
  meta_incent <- rma.mv(yi      = yi, 
                        V       = vi,
                        random  = list(~1|id_record/id_study/id_control, 
                                       ~1|event_materials,
                                       ~1|country,
                                       ~1|control_type,
                                       ~1|modality,
                                       ~1|population,
                                       ~1|test_type,
                                       ~1|test_medium,
                                       ~1|exposure_medium),
                        mods    = ~ postevent_retention_interval
                        + incentives
                        + postexposure_retention_interval
                        + preevent_warning
                        + postevent_warning
                        + postexposure_warning
                        + control_acc
                        + postevent_recall
                        + postexposure_recall
                        + publication_year
                        + preregistered,
                        data    = incent_data,
                        method  = "REML", 
                        control = list(
                          iter.max  = 1000,
                          rel.tol   = 1e-8
                        ),
                        verbose = FALSE)
  
  saveRDS(meta_incent, "output/mema_incent.rds")
  
} else {
  
  meta_incent <- readRDS("output/mema_incent.rds")
  
}

# Item Centrality --------------------------------------------------------------

cent_data <- data_es %>% 
  filter(!is.na(item_centrality))

cent_data$item_centrality <- as.factor(cent_data$item_centrality)

if (!file.exists("output/mema_centrality.rds")) {
  
  meta_centrality <- rma.mv(yi      = yi, 
                            V       = vi,
                            random  = list(~1|id_record/id_study/id_control, 
                                           ~1|event_materials,
                                           ~1|country,
                                           ~1|control_type,
                                           ~1|modality,
                                           ~1|population,
                                           ~1|test_type,
                                           ~1|test_medium,
                                           ~1|exposure_medium),
                            mods    = ~ postevent_retention_interval
                            + item_centrality
                            + postexposure_retention_interval
                            + preevent_warning
                            + postevent_warning
                            + postexposure_warning
                            + control_acc
                            + postevent_recall
                            + postexposure_recall
                            + publication_year
                            + preregistered,
                            data    = cent_data,
                            method  = "REML", 
                            control = list(
                              iter.max  = 1000,
                              rel.tol   = 1e-8
                            ),
                            verbose = FALSE)
  
  saveRDS(meta_centrality, "output/mema_centrality.rds")
  
} else {
  
  meta_centrality <- readRDS("output/mema_centrality.rds")
  
}

if (!file.exists("output/mema_centrality_no_acc.rds")) {
  
  meta_centrality_no_acc <- rma.mv(yi      = yi, 
                                   V       = vi,
                                   random  = list(~1|id_record/id_study/id_control, 
                                                  ~1|event_materials,
                                                  ~1|country,
                                                  ~1|control_type,
                                                  ~1|modality,
                                                  ~1|population,
                                                  ~1|test_type,
                                                  ~1|test_medium,
                                                  ~1|exposure_medium),
                                   mods    = ~ postevent_retention_interval
                                   + item_centrality
                                   + postexposure_retention_interval
                                   + preevent_warning
                                   + postevent_warning
                                   + postexposure_warning
                                   + postevent_recall
                                   + postexposure_recall
                                   + publication_year
                                   + preregistered,
                                   data    = cent_data,
                                   method  = "REML", 
                                   control = list(
                                     iter.max  = 1000,
                                     rel.tol   = 1e-8
                                   ),
                                   verbose = FALSE)
  
  saveRDS(meta_centrality_no_acc, "output/mema_centrality_no_acc.rds")
  
} else {
  
  meta_centrality_no_acc <- readRDS("output/mema_centrality_no_acc.rds")
  
}

# RES designs ------------------------------------------------------------------

# Create subgroup of studies that feature more than zero post-event recall tests

res_studies <- data_es %>% 
  filter(postevent_recall > 0) %>% 
  select(id_study) %>% 
  unique()

res_data <- data_es %>% 
  filter(id_study %in% res_studies$id_study)

if (!file.exists('output/mema_res.rds')) {
  
  meta_res   <- rma.mv(yi      = yi, 
                       V       = vi,
                       random  = list(~1|id_record/id_study/id_control, 
                                      ~1|event_materials,
                                      ~1|country,
                                      ~1|control_type,
                                      ~1|modality,
                                      ~1|population,
                                      ~1|test_type,
                                      ~1|test_medium,
                                      ~1|exposure_medium),
                       mods    = ~ postevent_retention_interval
                       + postexposure_retention_interval
                       + preevent_warning
                       + postevent_warning
                       + postexposure_warning
                       + control_acc
                       + postevent_recall
                       + postexposure_recall
                       + publication_year
                       + preregistered,
                       data    = res_data,
                       method  = "REML", 
                       control = list(
                         iter.max  = 1000,
                         rel.tol   = 1e-8
                       ),
                       verbose = TRUE)
  
  saveRDS(meta_res, 'output/mema_res.rds')
  
} else {
  
  meta_res <- readRDS('output/mema_res.rds')
  
}

# Only comparing zero tests to one test

res_data_01 <- res_data %>% 
  filter(postevent_recall < 2)

if (!file.exists('output/mema_res_01.rds')) {
  
  meta_res_01   <- rma.mv(yi      = yi, 
                          V       = vi,
                          random  = list(~1|id_record/id_study/id_control, 
                                         ~1|event_materials,
                                         ~1|country,
                                         ~1|control_type,
                                         ~1|modality,
                                         ~1|population,
                                         ~1|test_type,
                                         ~1|test_medium,
                                         ~1|exposure_medium),
                          mods    = ~ postevent_retention_interval
                          + postexposure_retention_interval
                          + preevent_warning
                          + postevent_warning
                          + postexposure_warning
                          + control_acc
                          + postevent_recall
                          + postexposure_recall
                          + publication_year
                          + preregistered,
                          data    = res_data_01,
                          method  = "REML", 
                          control = list(
                            iter.max  = 1000,
                            rel.tol   = 1e-8
                          ),
                          verbose = TRUE)
  
  saveRDS(meta_res_01, 'output/mema_res_01.rds')
  
} else {
  
  meta_res_01 <- readRDS('output/mema_res_01.rds')
  
}

# Removing control accuracy

if (!file.exists('output/mema_res_no_acc.rds')) {
  
  meta_res_no_acc   <- rma.mv(yi      = yi, 
                             V       = vi,
                             random  = list(~1|id_record/id_study/id_control, 
                                            ~1|event_materials,
                                            ~1|country,
                                            ~1|control_type,
                                            ~1|modality,
                                            ~1|population,
                                            ~1|test_type,
                                            ~1|test_medium,
                                            ~1|exposure_medium),
                             mods    = ~ postevent_retention_interval
                             + postexposure_retention_interval
                             + preevent_warning
                             + postevent_warning
                             + postexposure_warning
                             + postevent_recall
                             + postexposure_recall
                             + publication_year
                             + preregistered,
                             data    = res_data,
                             method  = "REML", 
                             control = list(
                               iter.max  = 1000,
                               rel.tol   = 1e-8
                             ),
                             verbose = TRUE)
  
  saveRDS(meta_res_no_acc, 'output/mema_res_no_acc.rds')
  
} else {
  
  meta_res_no_acc <- readRDS('output/mema_res_no_acc.rds')
  
}

