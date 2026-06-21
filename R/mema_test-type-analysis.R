################################################################################

# Misinformation Meta-analysis - Post Hoc Analysis of Test Types

################################################################################

# Packages ---------------------------------------------------------------------

library(tidyverse)
library(metafor)
library(cowplot)
library(ggbeeswarm)

# Load data --------------------------------------------------------------------

data_es <- read_csv('data/mema_clean_data.csv')

# Data transformations ---------------------------------------------------------

# Calculate and center moderator variables

data_es <- data_es %>% 
  mutate(
    gender_female_prop          = gender_female_prop - .50,
    year_raw                    = publication_year,
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

data_es$event_materials[is.na(data_es$event_materials)]         <- "missing"
data_es$country[is.na(data_es$country)]                         <- "missing"
data_es$control_type[is.na(data_es$control_type)]               <- "missing"
data_es$modality[is.na(data_es$modality)]                       <- "missing"
data_es$population[is.na(data_es$population)]                   <- "missing"
data_es$test_type[is.na(data_es$test_type)]                     <- "missing"
data_es$test_medium[is.na(data_es$test_medium)]                 <- "missing"
data_es$exposure_medium[is.na(data_es$exposure_medium)]         <- "missing"

data_es$misinformation_type[data_es$misinformation_type == "unclear"] <- NA
data_es$misinformation_type[is.na(data_es$misinformation_type)] <- "missing"

# Random effects analysis ------------------------------------------------------

# Random slopes for control accuracy by test type

if (!file.exists("output/mema_acc_test.rds")) {
  
  meta_acc_test   <- rma.mv(yi      = yi, 
                            V       = vi,
                            random  = list(~1|id_record/id_study/id_control, 
                                           ~1|event_materials,
                                           ~1|country,
                                           ~1|control_type,
                                           ~1|modality,
                                           ~1|population,
                                           ~1|test_type,
                                           ~control_acc|test_type,
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
                            data    = data_es %>% 
                              filter(complete.cases(control_acc)),
                            method  = "REML", 
                            struct  = "GEN",
                            control = list(
                              iter.max  = 1000,
                              rel.tol   = 1e-8
                            ),
                            verbose = TRUE)
  
  saveRDS(meta_acc_test, "output/mema_acc_test.rds")
  
} else {
  
  meta_acc_test <- readRDS("output/mema_acc_test.rds")
  
}

## Heterogeneity

meta_acc_test_ranef <- ranef(meta_acc_test)

I2_acc_test <- I2_calc(meta_acc_test)

pi_acc_test <- pi_intercept(meta_acc_test)

# Control accuracy visualization, by test type----------------------------------

# Create prediction line for average effect

pred_test_acc <- predict(meta_acc_test,  
                         newmods = cbind(
                           rep(0, 100),
                           rep(0, 100),
                           rep(0, 100),
                           rep(0, 100),
                           rep(0, 100),
                           seq(min(data_es$control_acc, na.rm = TRUE),
                               max(data_es$control_acc, na.rm = TRUE), 
                               length.out = 100),
                           rep(0, 100),
                           rep(0, 100),
                           rep(0, 100),
                           rep(0, 100)
                         ))

pred_test_acc$control_acc <- seq(min(data_es$control_acc, na.rm = TRUE),
                                    max(data_es$control_acc, na.rm = TRUE), 
                                    length.out = 100) - min(data_es$control_acc, na.rm = TRUE)

# Create data frame for plotting of predictions for each test type

model_data <- data_es %>% 
  filter(complete.cases(postevent_retention_interval, 
                        postexposure_retention_interval, 
                        postevent_recall,
                        preevent_warning, 
                        postevent_warning, 
                        postexposure_warning, 
                        control_acc, 
                        postevent_recall,
                        postexposure_recall, 
                        publication_year, 
                        preregistered)) %>% 
  bind_cols(as.data.frame(predict(meta_acc_test)))

# Visualization of control accuracy

scatter_control_acc_rs <-
  ggplot(data_es,
         aes(
           x = control_acc - min(control_acc, na.rm = TRUE),
           y = yi,
           color = 
         )) +
  geom_vline(
    xintercept = abs(min(data_es$control_acc, na.rm = TRUE)),
    linetype   = "dashed"
  ) +
  geom_hline(
    yintercept = meta_acc_test$beta[[1]],
    linetype   = "dashed",
    linewidth  = 1
  ) +
  geom_hline(
    yintercept = 0,
    linetype   = "dotted"
  ) +
  geom_point(
    aes(
      size = 1/vi
    ),
    shape = 1,
    alpha = .25
  ) +
  geom_line(
      data = model_data %>% 
        filter(test_type != "missing"),
      aes(
        y = pred,
        color = test_type,
        group = test_type
      ),
      stat      = "smooth",
      se        = FALSE,
      method    = "lm",
      alpha     = .80,
      linewidth = 1
    ) +
  geom_line(
    data = as.data.frame(pred_test_acc),
    aes(
      x = control_acc,
      y = pred
    ),
    linetype  = "solid",
    alpha     = .80,
    color     = "darkred",
    linewidth = 1.25
  ) +
  geom_line(
    data = as.data.frame(pred_test_acc),
    aes(
      x = control_acc,
      y = ci.lb
    ),
    linetype  = "dashed",
    alpha     = .80,
    color     = "darkred",
    linewidth = 1.25
  ) +
  geom_line(
    data = as.data.frame(pred_test_acc),
    aes(
      x = control_acc,
      y = ci.ub
    ),
    linetype  = "dashed",
    alpha     = .80,
    color     = "darkred",
    linewidth = 1.25
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, .10)
  ) +
  scale_y_continuous(
    breaks = seq(-4, 8, 1),
    limits = c(-4, 8)
  ) +
  labs(
    x = "Control Accuracy",
    y = "Effect size",
    color = "Test Type"
  ) +
  scale_color_discrete(
    labels = c("Cued Recall",
               "Free Recall",
               "Line Up",
               "MMFR",
               "Modified Test",
               "Recognition",
               "Source Monitoring",
               "Total Recall")
  ) +
  guides(
    size = "none"
  ) +
  theme_classic()

save_plot("figures/mema_control-accuracy-plot_random-slopes.png", 
          scatter_control_acc_rs,
          base_height = 6, base_width = 8)

# Subgroup analyses ------------------------------------------------------------

# Recognition

if (!file.exists("output/mema_recognition.rds")) {
  
  meta_recog   <- rma.mv(yi      = yi, 
                         V       = vi,
                         random  = list(~1|id_record/id_study/id_control, 
                                        ~1|event_materials,
                                        ~1|country,
                                        ~1|control_type,
                                        ~1|modality,
                                        ~1|population,
                                        # ~1|test_type,
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
                         data    = data_es %>% 
                           filter(test_type == "recognition"),
                         method  = "REML", 
                         control = list(
                           iter.max  = 1000,
                           rel.tol   = 1e-8
                         ),
                         verbose = TRUE)
  
  saveRDS(meta_recog, "output/mema_recognition.rds")
  
} else {
  
  meta_recog <- readRDS("output/mema_recognition.rds")
  
}

# Free recall

if (!file.exists("output/mema_freerecall.rds")) {
  
  meta_freerecall   <- rma.mv(yi      = yi, 
                              V       = vi,
                              random  = list(~1|id_record/id_study/id_control, 
                                             ~1|event_materials,
                                             ~1|country,
                                             ~1|control_type,
                                             ~1|modality,
                                             ~1|population,
                                             # ~1|test_type,
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
                              data    = data_es %>% 
                                filter(test_type == "free_recall"),
                              method  = "REML", 
                              control = list(
                                iter.max  = 1000,
                                rel.tol   = 1e-8
                              ),
                              verbose = TRUE)
  
  saveRDS(meta_freerecall, "output/mema_freerecall.rds")
  
} else {
  
  meta_freerecall <- readRDS("output/mema_freerecall.rds")
  
}

# Source Monitoring

if (!file.exists("output/mema_sourcemonitoring.rds")) {
  
  meta_source   <- rma.mv(yi      = yi, 
                          V       = vi,
                          random  = list(~1|id_record/id_study/id_control, 
                                         ~1|event_materials,
                                         ~1|country,
                                         ~1|control_type,
                                         ~1|modality,
                                         ~1|population,
                                         # ~1|test_type,
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
                          data    = data_es %>% 
                            filter(test_type == "source_monitoring"),
                          method  = "REML", 
                          control = list(
                            iter.max  = 1000,
                            rel.tol   = 1e-8
                          ),
                          verbose = TRUE)
  
  saveRDS(meta_source, "output/mema_sourcemonitoring.rds")
  
} else {
  
  meta_source <- readRDS("output/mema_sourcemonitoring.rds")
  
}

# Cued recall

if (!file.exists("output/mema_cuedrecall.rds")) {
  
  meta_cuedrecall   <- rma.mv(yi      = yi, 
                              V       = vi,
                              random  = list(~1|id_record/id_study/id_control, 
                                             ~1|event_materials,
                                             ~1|country,
                                             ~1|control_type,
                                             ~1|modality,
                                             ~1|population,
                                             # ~1|test_type,
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
                              data    = data_es %>% 
                                filter(test_type == "cued_recall"),
                              method  = "REML", 
                              control = list(
                                iter.max  = 1000,
                                rel.tol   = 1e-8
                              ),
                              verbose = TRUE)
  
  saveRDS(meta_cuedrecall, "output/mema_cuedrecall.rds")
  
} else {
  
  meta_cuedrecall <- readRDS("output/mema_cuedrecall.rds")
  
}

# Modified test

if (!file.exists("output/mema_modified.rds")) {
  
  meta_modified   <- rma.mv(yi      = yi, 
                            V       = vi,
                            random  = list(~1|id_record/id_study/id_control, 
                                           ~1|event_materials,
                                           ~1|country,
                                           ~1|control_type,
                                           ~1|modality,
                                           ~1|population,
                                           # ~1|test_type,
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
                            data    = data_es %>% 
                              filter(test_type == "modified_test"),
                            method  = "REML", 
                            control = list(
                              iter.max  = 1000,
                              rel.tol   = 1e-8
                            ),
                            verbose = TRUE)
  
  saveRDS(meta_modified, "output/mema_modified.rds")
  
} else {
  
  meta_modified <- readRDS("output/mema_modified.rds")
  
}