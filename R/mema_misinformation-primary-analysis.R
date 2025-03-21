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

# Primary analysis--------------------------------------------------------------

if (!file.exists("output/mema_primary.rds")) {
  
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
  
  saveRDS(meta_primary, "output/mema_primary.rds")
  
} else {
  
  meta_primary <- readRDS("output/mema_primary.rds")
  
}

meta_primary_ranef <- ranef(meta_primary)

# Heterogeneity

# This code is adapted from
# https://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate

W <- diag(1/meta_primary$vi)

X <- model.matrix(meta_primary)

P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W

I2 <- 100 * sum(meta_primary$sigma2) / (sum(meta_primary$sigma2) + (meta_primary$k - meta_primary$p)/sum(diag(P)))

I2_components <- round(100 * meta_primary$sigma2 / (sum(meta_primary$sigma2) + (meta_primary$k-meta_primary$p)/sum(diag(P))), 2)

# Bias correction --------------------------------------------------------------

# PET

if (!file.exists("output/mema_pet.rds")) {
  
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
                           + I(sqrt(vi)),
                           data   = data_es,
                           method = "REML",
                           control = list(
                             iter.max    = 1000,
                             rel.tol     = 1e-8, 
                             sigma2.init = meta_primary$sigma2
                           ),
                           verbose = TRUE)
  
  
  saveRDS(meta_pet, "output/mema_pet.rds")
  
} else {
  
  meta_pet <- readRDS("output/mema_pet.rds")
  
}

# PEESE

if (!file.exists("output/mema_peese.rds")) {
  
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
                             iter.max    = 1000,
                             rel.tol     = 1e-8, 
                             sigma2.init = meta_primary$sigma2
                           ),
                           verbose = TRUE)
  
  saveRDS(meta_peese, "output/mema_peese.rds")
  
} else {
  
  meta_peese <- readRDS("output/mema_peese.rds")
  
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
    alpha = .40
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
  ylab("Precision (inverse variance)") +
  theme_classic()

save_plot("figures/mema_funnel-plot.png", funnel_plot,
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
                              seq(min(data_es$control_acc, na.rm = TRUE),
                                  max(data_es$control_acc, na.rm = TRUE), 
                                  length.out = 100),
                              rep(0, 100),
                              rep(0, 100),
                              rep(0, 100),
                              rep(0, 100)
                            ))

pred_control_acc$control_acc <- seq(min(data_es$control_acc, na.rm = TRUE),
                                    max(data_es$control_acc, na.rm = TRUE), 
                                    length.out = 100) - min(data_es$control_acc, na.rm = TRUE)

# Visualization of control accuracy

scatter_control_acc <-
  ggplot(data_es,
         aes(
           x = control_acc - min(control_acc, na.rm = TRUE),
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

save_plot("figures/mema_control-accuracy-plot.png", 
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

save_plot("figures/mema_control-accuracy-modified-plot.png", 
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
                              seq(min(data_es$control_acc, na.rm = TRUE),
                                  max(data_es$control_acc, na.rm = TRUE), 
                                  length.out = 100),
                              rep(0, 100),
                              rep(0, 100),
                              rep(0, 100),
                              rep(0, 100),
                              rep(0, 100)
                            ))

pred_acc_peese$control_acc   <- seq(min(data_es$control_acc, na.rm = TRUE),
                                    max(data_es$control_acc, na.rm = TRUE), 
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

save_plot("figures/mema_control-accuracy-peese-plot.png", 
          scatter_acc_peese,
          base_height = 8, base_width = 8)

# Influence analysis -----------------------------------------------------------

# Leverage

meta_hat <- hatvalues(meta_primary)

leverages <- data.frame(
  id_effect = str_pad(names(meta_hat), 4, "left", "0"),
  leverage  = meta_hat
)

data_es <- data_es %>% 
  left_join(leverages, by = "id_effect")

## Arbitrary conventional cutoff for leverages

leverage_cut <- mean(data_es$leverage, na.rm = TRUE) * 3

# Standardized residuals

meta_res <- as.data.frame(rstandard(meta_primary))

meta_res$id_effect <- row.names(meta_res)
meta_res$id_effect <- str_pad(meta_res$id_effect, 4, "left", "0")

data_es <- data_es %>% 
  left_join(meta_res, by = "id_effect")

# Sensitivity analysis----------------------------------------------------------

# Excluding all cases above leverage cutoff

if (!file.exists("output/mema_leverage.rds")) {
  
  meta_leverage  <- rma.mv(yi      = yi, 
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
                           data    = data_es %>% 
                             filter(leverage < leverage_cut),
                           method  = "REML", 
                           control = list(
                             iter.max    = 1000,
                             rel.tol     = 1e-8, 
                             sigma2.init = meta_primary$sigma2
                           ),
                           verbose = TRUE)
  
  saveRDS(meta_leverage, "output/mema_leverage.rds")
  
} else {
  
  meta_leverage <- readRDS("output/mema_leverage.rds")
  
}

meta_leverage_ranef <- ranef(meta_leverage)

## PET-PEESE with high leverage cases removed

### PET

if (!file.exists("output/mema_pet_leverage.rds")) {
  
  meta_pet_leverage  <- rma.mv(yi      = yi, 
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
                               + I(sqrt(vi)),
                               data    = data_es %>% 
                                 filter(leverage < leverage_cut),
                               method  = "REML", 
                               control = list(
                                 iter.max    = 1000,
                                 rel.tol     = 1e-8, 
                                 sigma2.init = meta_primary$sigma2
                               ),
                               verbose = TRUE)
  
  saveRDS(meta_pet_leverage, "output/mema_pet_leverage.rds")
  
} else {
  
  meta_pet_leverage <- readRDS("output/mema_pet_leverage.rds")
  
}

meta_pet_leverage_ranef <- ranef(meta_pet_leverage)

### PEESE

if (!file.exists("output/mema_peese_leverage.rds")) {
  
  meta_peese_leverage  <- rma.mv(yi      = yi, 
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
                                 + I(vi),
                                 data    = data_es %>% 
                                   filter(leverage < leverage_cut),
                                 method  = "REML", 
                                 control = list(
                                   iter.max    = 1000,
                                   rel.tol     = 1e-8, 
                                   sigma2.init = meta_primary$sigma2
                                 ),
                                 verbose = TRUE)
  
  saveRDS(meta_peese_leverage, "output/mema_peese_leverage.rds")
  
} else {
  
  meta_peese_leverage <- readRDS("output/mema_peese_leverage.rds")
  
}

meta_peese_leverage_ranef <- ranef(meta_peese_leverage)

# Excluding all cases with standardized residuals greater than |1.96|

if (!file.exists("output/mema_resid.rds")) {
  
  meta_resid     <- rma.mv(yi      = yi, 
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
                           data    = data_es %>% 
                             filter(abs(resid) < qnorm(.975)),
                           method  = "REML", 
                           control = list(
                             iter.max    = 1000,
                             rel.tol     = 1e-8, 
                             sigma2.init = meta_primary$sigma2
                           ),
                           verbose = TRUE)
  
  saveRDS(meta_resid, "output/mema_resid.rds")
  
} else {
  
  meta_resid <- readRDS("output/mema_resid.rds")
  
}

meta_resid_ranef <- ranef(meta_resid)

## PET-PEESE with large residual cases removed

### PET

if (!file.exists("output/mema_pet_resid.rds")) {
  
  meta_pet_resid  <- rma.mv(yi      = yi, 
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
                               + I(sqrt(vi)),
                               data    = data_es %>% 
                                 filter(abs(resid) < qnorm(.975)),
                               method  = "REML", 
                               control = list(
                                 iter.max    = 1000,
                                 rel.tol     = 1e-8, 
                                 sigma2.init = meta_primary$sigma2
                               ),
                               verbose = TRUE)
  
  saveRDS(meta_pet_resid, "output/mema_pet_resid.rds")
  
} else {
  
  meta_pet_resid <- readRDS("output/mema_pet_resid.rds")
  
}

meta_pet_resid_ranef <- ranef(meta_pet_resid)

### PEESE

if (!file.exists("output/mema_peese_resid.rds")) {
  
  meta_peese_resid  <- rma.mv(yi      = yi, 
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
                                 + I(vi),
                                 data    = data_es %>%
                                   filter(abs(resid) < qnorm(.975)),
                                 method  = "REML", 
                                 control = list(
                                   iter.max    = 1000,
                                   rel.tol     = 1e-8, 
                                   sigma2.init = meta_primary$sigma2
                                 ),
                                 verbose = TRUE)
  
  saveRDS(meta_peese_resid, "output/mema_peese_resid.rds")
  
} else {
  
  meta_peese_resid <- readRDS("output/mema_peese_resid.rds")
  
}

meta_peese_resid_ranef <- ranef(meta_peese_resid)

# Removal of imputed variance cases --------------------------------------------

if (!file.exists("output/mema_primary_imp.rds")) {
  
  meta_primary_imp   <- rma.mv(yi      = yi, 
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
                               data    = data_es %>% 
                                 filter(sd_imputed == 0),
                               method  = "REML", 
                               control = list(
                                 iter.max    = 1000,
                                 rel.tol     = 1e-8, 
                                 sigma2.init = meta_primary$sigma2
                               ),
                               verbose = TRUE)
  
  saveRDS(meta_primary_imp, "output/mema_primary_imp.rds")
  
} else {
  
  meta_primary_imp <- readRDS("output/mema_primary_imp.rds")
  
}

meta_primary_imp_ranef <- ranef(meta_primary_imp)

# PET

if (!file.exists("output/mema_pet_imp.rds")) {
  
  meta_pet_imp       <- rma.mv(yi     = yi, 
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
                               + I(sqrt(vi)),
                               data   = data_es %>% 
                                 filter(sd_imputed == 0),
                               method = "REML",
                               control = list(
                                 iter.max    = 1000,
                                 rel.tol     = 1e-8, 
                                 sigma2.init = meta_primary$sigma2
                               ),
                               verbose = TRUE)
  
  
  saveRDS(meta_pet_imp, "output/mema_pet_imp.rds")
  
} else {
  
  meta_pet_imp <- readRDS("output/mema_pet_imp.rds")
  
}

# PEESE

if (!file.exists("output/mema_peese_imp.rds")) {
  
  meta_peese_imp     <- rma.mv(yi     = yi, 
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
                               data   = data_es %>% 
                                 filter(sd_imputed == 0),
                               method = "REML",
                               control = list(
                                 iter.max    = 1000,
                                 rel.tol     = 1e-8, 
                                 sigma2.init = meta_primary$sigma2
                               ),
                               verbose = TRUE)
  
  saveRDS(meta_peese_imp, "output/mema_peese_imp.rds")
  
} else {
  
  meta_peese_imp <- readRDS("output/mema_peese_imp.rds")
  
}
_