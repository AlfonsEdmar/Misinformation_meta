################################################################################

# Misinformation Meta-analysis - Subgroup analysis  

################################################################################

# Loading data

data_es <- read_csv('data/misinformation_clean_data.csv')

# Data transformations ---------------------------------------------------------

# Center moderator variables

data_es <- data_es %>% 
  mutate(
    gender_female_prop          = gender_female_prop - .50,
    publication_year            = publication_year - mean(publication_year, na.rm = TRUE),
    control_acc                 = case_when(
      !is.na(total_accuracy_control_mean) ~ total_accuracy_control_mean,
      is.na(total_accuracy_control_mean) ~ total_accuracy_control_prop
    ),
    control_acc = control_acc - mean(control_acc, na.rm = TRUE),
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

age_data <- data_es %>% filter(!is.na(data_es$age_mean))

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
                     + publication_year,
                     data    = age_data,
                     method  = "REML", 
                     control = list(
                       iter.max  = 1000,
                       rel.tol   = 1e-8
                     ),
                     verbose = TRUE)

saveRDS(meta_age, 'output/mema_age.rds')

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
                         + I(age_mean^2)
                         + postexposure_retention_interval
                         + preevent_warning
                         + postevent_warning
                         + postexposure_warning
                         + control_acc
                         + postevent_recall
                         + postexposure_recall
                         + publication_year,
                         data    = age_data,
                         method  = "REML", 
                         control = list(
                           iter.max  = 1000,
                           rel.tol   = 1e-8
                         ),
                         verbose = TRUE)

saveRDS(meta_age_quad, 'output/mema_quad_age.rds')

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
                           + publication_year,
                           data    = age_data,
                           method  = "REML", 
                           control = list(
                             iter.max  = 1000,
                             rel.tol   = 1e-8
                           ),
                           verbose = FALSE)

saveRDS(meta_age_no_acc, 'output/mema_age_no_accuracy.rds')


#Categorical age
breaks <- c(0, 5, 18, 40, max(age_data$age_mean))
labels <- c("0-5", "6-17", "17-40", "41+")
age_data$age_cat <- cut(age_data$age_mean, breaks = breaks, labels = labels, include.lowest = TRUE)
age_data$age_cat <- relevel(age_data$age_cat, '17-40')

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
                         + publication_year,
                         data    = age_data,
                         method  = "REML", 
                         control = list(
                           iter.max  = 1000,
                           rel.tol   = 1e-8
                         ),
                         verbose = FALSE)

saveRDS(meta_age_cat, 'output/mema_age_cat.rds')

# Incentives -------------------------------------------------------------------

incent_data <- data_es %>% filter(!is.na(data_es$incentives))
incent_data$incentives <- as.factor(incent_data$incentives)

# Setting the reference to be no incentive

incent_data <- within(incent_data, incentives <- relevel(incentives, 6))

meta_incent<- rma.mv(yi      = yi, 
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
                     + publication_year,
                     data    = incent_data,
                     method  = "REML", 
                     control = list(
                       iter.max  = 1000,
                       rel.tol   = 1e-8
                     ),
                     verbose = FALSE)

saveRDS(meta_incent, "output/mema_incent.rds")

# Item Centrality --------------------------------------------------------------

cent_data <- data_es %>% filter(!is.na(item_centrality))
cent_data$item_centrality <- as.factor(cent_data$item_centrality)

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
                          + publication_year,
                          data    = cent_data,
                          method  = "REML", 
                          control = list(
                            iter.max  = 1000,
                            rel.tol   = 1e-8
                          ),
                          verbose = FALSE)

saveRDS(meta_centrality, "output/mema_centrality.rds")

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
                          + publication_year,
                          data    = cent_data,
                          method  = "REML", 
                          control = list(
                            iter.max  = 1000,
                            rel.tol   = 1e-8
                          ),
                          verbose = FALSE)

saveRDS(meta_centrality_no_acc, "output/mema_centrality_no_acc.rds")
