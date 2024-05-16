################################################################################

# Misinformation Meta-analysis - Subgroup analysis  

################################################################################
## Age analysis----------------------------------------------------------------- 
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
saveRDS(meta_age, 'output/misinformation_meta_age.rds')

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
saveRDS(meta_age_quad, 'output/misinformation_meta_quad_age.rds')

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
saveRDS(meta_age_no_acc, 'output/misinformation_meta_age_no_accuracy.rds')


#Categorical age
breaks <- c(0, 5, 18, 40, max(age_data$age_mean))
labels <- c("0-5", "6-17", "17-40", "41+")
age_data$age_cat <- cut(age_data$age_mean, breaks = breaks, labels = labels, include.lowest = TRUE)

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
                         verbose = F)
saveRDS(meta_age_no_acc, 'output/misinformation_meta_age_cat.rds')

## Incentives ------------------------------------------------------------------
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
                     verbose = F)
saveRDS(meta_incent, "output/misinformation_meta_incent.rds")

## Item Centrality--------------------------------------------------------------
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
                          verbose = F)

saveRDS(meta_centrality, "output/misinformation_meta_centrality.rds")
