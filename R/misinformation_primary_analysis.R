################################################################################

# Misinformation Meta-analysis - Main analysis 

################################################################################

# Packages ---------------------------------------------------------------------

library(tidyverse)
library(metafor)
library(cowplot)

# Load data --------------------------------------------------------------------

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

# Primary analysis--------------------------------------------------------------

if (!file.exists("output/misinformation_meta_primary.rds")) {
  
  meta_primary   <- rma.mv(yi      = yi, 
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
                           data    = data_es,
                           method  = "REML", 
                           control = list(
                             iter.max  = 1000,
                             rel.tol   = 1e-8
                           ),
                           verbose = TRUE)
  
  saveRDS(meta_primary, "output/misinformation_meta_primary.rds")
  
} else {
  
  meta_primary <- readRDS("output/misinformation_meta_primary.rds")
  
}

meta_primary_ranef <- ranef(meta_primary)

# Bias correction --------------------------------------------------------------

# PET

if (!file.exists("output/misinformation_meta_pet.rds")) {
  
  meta_pet       <- rma.mv(yi     = yi, 
                           V      = vi,
                           random = list(~1|id_record/id_study/id_control, 
                                         ~1|event_materials,
                                         ~1|country,
                                         ~1|control_type,
                                         ~1|modality,
                                         ~1|population,
                                         ~1|test_type,
                                         ~1|test_medium,
                                         ~1|exposure_medium),
                           mods   = ~ postevent_retention_interval
                           + postexposure_retention_interval
                           + preevent_warning
                           + postevent_warning
                           + postexposure_warning
                           + control_acc
                           + postevent_recall
                           + postexposure_recall
                           + publication_year
                           + preregistered
                           + I(vi^2),
                           data   = data_es,
                           method = "REML",
                           control = list(
                             iter.max  = 1000,
                             rel.tol   = 1e-8
                           ),
                           verbose = TRUE)
  
  
  saveRDS(meta_pet, "output/misinformation_meta_pet.rds")
  
} else {
  
  meta_pet <- readRDS("output/misinformation_meta_pet.rds")
  
}

meta_pet_ranef <- ranef(meta_pet)

# PEESE

if (!file.exists("output/misinformation_meta_peese.rds")) {
  
  meta_peese     <- rma.mv(yi     = yi, 
                           V      = vi,
                           random = list(~1|id_record/id_study/id_control, 
                                         ~1|event_materials,
                                         ~1|country,
                                         ~1|control_type,
                                         ~1|modality,
                                         ~1|population,
                                         ~1|test_type,
                                         ~1|test_medium,
                                         ~1|exposure_medium),
                           mods   = ~ postevent_retention_interval
                           + postexposure_retention_interval
                           + preevent_warning
                           + postevent_warning
                           + postexposure_warning
                           + control_acc
                           + postevent_recall
                           + postexposure_recall
                           + publication_year
                           + preregistered
                           + I(vi),
                           data   = data_es,
                           method = "REML",
                           control = list(
                             iter.max  = 1000,
                             rel.tol   = 1e-8
                           ),
                           verbose = TRUE)
  
  saveRDS(meta_peese, "output/misinformation_meta_peese.rds")
  
} else {
  
  meta_peese <- readRDS("output/misinformation_meta_peese.rds")
  
}

meta_peese_ranef <- ranef(meta_peese)

# Funnel Plot ------------------------------------------------------------------

data_es <- data_es %>%
  mutate(precision = 1/vi)

prec_seq <- seq(.5, ceiling(max(data_es$precision)), length.out = 100)
se_seq   <- seq(.5, ceiling(max(sqrt(data_es$vi))), length.out = 100)

ub95 <- meta_primary$beta[[1]] + 1/(se_seq*qnorm(.975))
lb95 <- meta_primary$beta[[1]] - 1/(se_seq*qnorm(.975))

funnel_plot <- 
ggplot(data_es, 
       aes(
         x = yi, 
         y = precision)
       ) +
  geom_point(
    shape = 1, 
    color = "black",
    alpha = .15
    ) +
  geom_line(
    data = data.frame(prec_seq, se_seq, lb95, ub95),
    aes(
      y = prec_seq,
      x = lb95
    ),
    linetype = "dashed",
    alpha    = .50
  ) +
  geom_line(
    data = data.frame(prec_seq, se_seq, lb95, ub95),
    aes(
      y = prec_seq,
      x = ub95
    ),
    linetype = "dashed",
    alpha    = .50
  ) +
  geom_vline(
    xintercept = 0, 
    alpha      = .50,
    linetype   = "dotted"
    ) +
  geom_vline(
    xintercept = meta_primary$beta[[1]], 
    alpha      = .50,
    linetype   = "dashed"
    ) +
  scale_x_continuous(
    breaks = seq(-8, 8, .5)
  ) +
  xlab("Effect Size") +
  ylab("Precision") +
  theme_classic()

save_plot("figures/misinformation_funnel-plot.png", funnel_plot,
          base_height = 4.5, base_width = 10)

# Control accuracy exploration -------------------------------------------------

# Create prediction line

pred_control_acc <- predict(meta_primary, 
                            newmods = cbind(
                              rep(0, 100),
                              rep(0, 100),
                              rep(0, 100),
                              rep(0, 100),
                              rep(0, 100),
                              seq(min(data_es$control_acc),
                                  max(data_es$control_acc), 
                                  length.out = 100),
                              rep(0, 100),
                              rep(0, 100),
                              rep(0, 100),
                              rep(0, 100)
                            ))

pred_control_acc$control_acc <- seq(min(data_es$control_acc),
                                    max(data_es$control_acc), 
                                    length.out = 100) - min(data_es$control_acc)

# Visualization of control accuracy

scatter_control_acc <-
ggplot(data_es,
       aes(
         x = control_acc - min(control_acc),
         y = yi
       )) +
  geom_vline(
    xintercept = abs(min(data_es$control_acc)),
    linetype   = "dotted"
  ) +
  geom_hline(
    yintercept = meta_primary$beta[[1]],
    linetype   = "dotted"
  ) +
  geom_point(
    shape = 1,
    alpha = .15
  ) +
  geom_line(
    data = as.data.frame(pred_control_acc),
    aes(
      x = control_acc,
      y = pred
    ),
    linetype = "solid",
    alpha    = .80,
    color    = "darkred"
  ) +
  geom_line(
    data = as.data.frame(pred_control_acc),
    aes(
      x = control_acc,
      y = ci.lb
    ),
    linetype = "dotted",
    alpha    = .80,
    color    = "darkred"
  ) +
  geom_line(
    data = as.data.frame(pred_control_acc),
    aes(
      x = control_acc,
      y = ci.ub
    ),
    linetype = "dotted",
    alpha    = .80,
    color    = "darkred"
  ) +
  geom_hline(
    yintercept = 0,
    linetype   = "dashed"
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, .05)
  ) +
  scale_y_continuous(
    breaks = seq(-6, 8, .5)
  ) +
  labs(
    x = "Control Accuracy",
    y = "Effect size"
  ) +
  theme_classic()

save_plot("figures/misinformation_control-accuracy-plot.png", 
          scatter_control_acc,
          base_height = 8, base_width = 8)

## Modified test

data_es <- data_es %>% 
  mutate(
    modified = case_when(
      test_type == "modified_test"                    ~ "modified",
      test_type != "modified_test" | is.na(test_type) ~ "other"
    )
  )

scatter_control_acc_modified <- 
ggplot(data_es,
       aes(
         x     = control_acc - min(control_acc),
         y     = yi,
         color = modified
       )) +
  geom_vline(
    xintercept = abs(min(data_es$control_acc)),
    linetype   = "dotted"
  ) +
  geom_hline(
    yintercept = meta_primary$beta[[1]],
    linetype   = "dotted"
  ) +
  geom_point(
    shape = 1,
    alpha = .15
  ) +
  geom_point(
    shape = 1,
    alpha = .50
  ) +
  geom_line(
    data = as.data.frame(pred_control_acc),
    aes(
      x = control_acc,
      y = pred
    ),
    linetype = "solid",
    alpha    = .80,
    color    = "#23001E"
  ) +
  geom_line(
    data = as.data.frame(pred_control_acc),
    aes(
      x = control_acc,
      y = ci.lb
    ),
    linetype = "dotted",
    alpha    = .80,
    color    = "#23001E"
  ) +
  geom_line(
    data = as.data.frame(pred_control_acc),
    aes(
      x = control_acc,
      y = ci.ub
    ),
    linetype = "dotted",
    alpha    = .80,
    color    = "#23001E"
  ) +
  geom_line(
    data = as.data.frame(pred_control_acc),
    aes(
      x = control_acc,
      y = pred + meta_primary_ranef$test_type["modified_test", 1]
    ),
    linetype = "solid",
    alpha    = .80,
    color    = "#FF220C"
  ) +
  geom_line(
    data = as.data.frame(pred_control_acc),
    aes(
      x = control_acc,
      y = ci.lb + meta_primary_ranef$test_type["modified_test", 1]
    ),
    linetype = "dotted",
    alpha    = .80,
    color    = "#FF220C"
  ) +
  geom_line(
    data = as.data.frame(pred_control_acc),
    aes(
      x = control_acc,
      y = ci.ub + meta_primary_ranef$test_type["modified_test", 1]
    ),
    linetype = "dotted",
    alpha    = .80,
    color    = "#FF220C"
  ) +
  geom_hline(
    yintercept = 0,
    linetype   = "dashed"
  ) +
  scale_color_manual(
    values = c("#FF220C", "#88958D")
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, .05)
  ) +
  scale_y_continuous(
    breaks = seq(-6, 8, .5)
  ) +
  labs(
    x = "Control Accuracy",
    y = "Effect size",
    color = "Test"
  ) +
  theme_classic()

save_plot("figures/misinformation_control-accuracy-modified-plot.png", 
          scatter_control_acc_modified,
          base_height = 8, base_width = 8)

## PEESE

# Create prediction line

pred_acc_peese   <- predict(meta_peese, 
                            newmods = cbind(
                              rep(0, 100),
                              rep(0, 100),
                              rep(0, 100),
                              rep(0, 100),
                              rep(0, 100),
                              seq(min(data_es$control_acc),
                                  max(data_es$control_acc), 
                                  length.out = 100),
                              rep(0, 100),
                              rep(0, 100),
                              rep(0, 100),
                              rep(0, 100),
                              rep(0, 100)
                            ))

pred_acc_peese$control_acc   <- seq(min(data_es$control_acc),
                                    max(data_es$control_acc), 
                                    length.out = 100) - min(data_es$control_acc)

# Visualization of control accuracy

scatter_acc_peese <-
  ggplot(data_es,
         aes(
           x = control_acc - min(control_acc),
           y = yi
         )) +
  geom_vline(
    xintercept = abs(min(data_es$control_acc)),
    linetype   = "dotted"
  ) +
  geom_hline(
    yintercept = meta_peese$beta[[1]],
    linetype   = "dotted"
  ) +
  geom_point(
    shape = 1,
    alpha = .15
  ) +
  geom_line(
    data = as.data.frame(pred_acc_peese),
    aes(
      x = control_acc,
      y = pred
    ),
    linetype = "solid",
    alpha    = .80,
    color    = "darkred"
  ) +
  geom_line(
    data = as.data.frame(pred_acc_peese),
    aes(
      x = control_acc,
      y = ci.lb
    ),
    linetype = "dotted",
    alpha    = .80,
    color    = "darkred"
  ) +
  geom_line(
    data = as.data.frame(pred_acc_peese),
    aes(
      x = control_acc,
      y = ci.ub
    ),
    linetype = "dotted",
    alpha    = .80,
    color    = "darkred"
  ) +
  geom_hline(
    yintercept = 0,
    linetype   = "dashed"
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, .05)
  ) +
  scale_y_continuous(
    breaks = seq(-6, 8, .5)
  ) +
  labs(
    x = "Control Accuracy",
    y = "Effect size"
  ) +
  theme_classic()

save_plot("figures/misinformation_control-accuracy-peese-plot.png", 
          scatter_acc_peese,
          base_height = 8, base_width = 8)

# Influence analysis -----------------------------------------------------------

# Cook's Distance

cores      <- parallel::detectCores() - 1

cooks_dist <- cooks.distance.rma.mv(meta_primary,
                                    progbar  = TRUE,
                                    parallel = 'snow',
                                    ncpus    = cores) 


data_es <- cbind(data_es, cooks_dist)

n   <- nrow(data_es)

ggplot(data_es, 
       aes(y = cooks_dist, 
           x = id_effect,
           label = id_effect)
       )+
  geom_jitter(alpha = .5) +
  geom_text(
    aes(
    label = ifelse(cooks_dist > 4/n , as.character(id_effect), '')
    ), 
    hjust = -.2, 
    vjust = 0)

round(summary((data_es$cooks_dist)), 7)

# Extracting outliers at the arbitrary 4/n threshold.

outliers <- ifelse(data_es$cooks_dist <= 4/n, 0, 1)

# Sensitivity analysis----------------------------------------------------------











