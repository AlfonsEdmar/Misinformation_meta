################################################################################

# Misinformation Meta-analysis - Main analysis 

################################################################################

# Packages ---------------------------------------------------------------------

library(tidyverse)
library(metafor)
library(cowplot)
library(ggbeeswarm)

# Load data --------------------------------------------------------------------

data_es <- read_csv('data/misinformation_clean_data.csv')

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

ub95_null <-  1/(se_seq*qnorm(.975))
lb95_null <- -1/(se_seq*qnorm(.975))

funnel_plot <- 
  ggplot(data_es, 
         aes(
           x = yi, 
           y = precision)
  ) +
  # Effect size points
  geom_point(
    shape = 1, 
    color = "black",
    alpha = .25
  ) +
  # Confidence bands around point estimate
  geom_line(
    data = data.frame(prec_seq, se_seq, lb95, ub95),
    aes(
      y = prec_seq,
      x = lb95
    ),
    linetype  = "dashed",
    alpha     = .50,
    linewidth = 1
  ) +
  geom_line(
    data = data.frame(prec_seq, se_seq, lb95, ub95),
    aes(
      y = prec_seq,
      x = ub95
    ),
    linetype  = "dashed",
    alpha     = .50,
    linewidth = 1
  ) +
  # Confidence bands around zero
  geom_line(
    data = data.frame(prec_seq, se_seq, lb95_null, ub95_null),
    aes(
      y = prec_seq,
      x = lb95_null
    ),
    linetype = "dotted",
    alpha    = .50
  ) +
  geom_line(
    data = data.frame(prec_seq, se_seq, lb95_null, ub95_null),
    aes(
      y = prec_seq,
      x = ub95_null
    ),
    linetype = "dotted",
    alpha    = .50
  ) +
  # Point estimate and zero lines
  geom_vline(
    xintercept = 0, 
    alpha      = .50,
    linetype   = "dotted"
  ) +
  geom_vline(
    xintercept = meta_primary$beta[[1]], 
    alpha      = .50,
    linetype   = "dashed",
    linewidth  = 1
  ) +
  # x-axis
  scale_x_continuous(
    breaks = seq(-4, 8, 1),
    limits = c(-4, 8)
  ) +
  # Labels and theme
  xlab("Effect Size") +
  ylab("Precision (inverse variance)") +
  theme_classic()

save_plot("figures/mema_funnel-plot.png", funnel_plot,
          base_height = 6, base_width = 6)

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
    xintercept = abs(min(data_es$control_acc, na.rm = TRUE)),
    linetype   = "dashed"
  ) +
  geom_hline(
    yintercept = meta_primary$beta[[1]],
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
    data = as.data.frame(pred_control_acc),
    aes(
      x = control_acc,
      y = pred
    ),
    linetype  = "solid",
    alpha     = .80,
    color     = "darkred",
    linewidth = 1
  ) +
  geom_line(
    data = as.data.frame(pred_control_acc),
    aes(
      x = control_acc,
      y = ci.lb
    ),
    linetype  = "dashed",
    alpha     = .80,
    color     = "darkred",
    linewidth = 1
  ) +
  geom_line(
    data = as.data.frame(pred_control_acc),
    aes(
      x = control_acc,
      y = ci.ub
    ),
    linetype  = "dashed",
    alpha     = .80,
    color     = "darkred",
    linewidth = 1
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
    y = "Effect size"
  ) +
  guides(
    size = "none"
  ) +
  theme_classic()

save_plot("figures/mema_control-accuracy-plot.png", 
          scatter_control_acc,
          base_height = 6, base_width = 6)

# Influence analysis -----------------------------------------------------------

# Leverage

meta_hat <- hatvalues(meta_primary)

leverages <- data.frame(
  id_effect = data_es$id_effect[as.numeric(names(meta_hat))],
  leverage  = meta_hat
)

data_es <- data_es %>% 
  left_join(leverages, by = "id_effect")

## Arbitrary conventional cutoff for leverages

leverage_cut <- mean(data_es$leverage, na.rm = TRUE) * 3

# Standardized residuals

meta_res_df <- as.data.frame(rstandard(meta_primary))

meta_res_df$id_effect <- data_es$id_effect[as.numeric(row.names(meta_res_df))]
meta_res_df$id_effect <- str_pad(meta_res_df$id_effect, 4, "left", "0")

data_es <- data_es %>% 
  left_join(meta_res_df, by = "id_effect")

# Predicted values

meta_pred_df <- as.data.frame(predict(meta_primary))

meta_pred_df$id_effect <- data_es$id_effect[as.numeric(row.names(meta_pred_df))]
meta_pred_df$id_effect <- str_pad(meta_pred_df$id_effect, 4, "left", "0")

meta_pred_df <- meta_pred_df %>% 
  rename(
    se_pred = se
  )

data_es <- data_es %>% 
  left_join(meta_pred_df, by = "id_effect")

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

# Visualization: Predicted vs. residuals ---------------------------------------

plot_pred_res <- 
  ggplot(data_es,
         aes(
           x    = pred,
           y    = resid,
           size = 1/vi
         )) +
  geom_point(
    shape = 1,
    alpha = .25
  ) +
  geom_hline(
    yintercept = 0,
    linetype   = "dotted"
  ) +
  geom_smooth(
    method  = "lm",
    formula = "y ~ x",
    se      = FALSE
  ) +
  guides(
    size = "none"
  ) +
  labs(
    x = "Predicted",
    y = "Standardized residual"
  ) +
  theme_classic()

save_plot("figures/mema_pred-res-plot.png", 
          plot_pred_res,
          base_height = 6, base_width = 6)

# Visualization: Publication history -------------------------------------------

plot_timeline <- 
ggplot(data_es,
       aes(
         x = year_raw,
         y = yi,
         size = 1/vi
       )) +
  geom_quasirandom(
    shape = 1,
    alpha = .25
  ) +
  geom_hline(
    yintercept = meta_primary$beta[[1]],
    linetype   = "dashed",
    linewidth  = 1
  ) +
  geom_hline(
    yintercept = 0,
    linetype   = "dotted"
  ) +
  guides(
    size = "none"
  ) +
  scale_y_continuous(
    breaks = seq(-4, 20, 1),
    limits = c(-4, 8)
  ) +
  scale_x_continuous(
    breaks = seq(1975, 2020, 5)
  ) +
  labs(
    x = "Publication year",
    y = "Effect size"
  ) +
  theme_classic()

save_plot("figures/mema_timeline-plot.png", 
          plot_timeline,
          base_height = 6, base_width = 12)

# Visualization: Albatross -----------------------------------------------------

source("R/mema_albatross.R")

# Visualization: Grid ----------------------------------------------------------

grid_upper <- plot_grid(funnel_plot, plot_pred_res,
                        scatter_control_acc, albatross,
                        labels = c("a", "b", "c", "d"),
                        nrow = 2)

grid_plot <- plot_grid(grid_upper,
                       plot_timeline,
                       labels = c("", "e"),
                       nrow = 2,
                       rel_heights = c(1, .5))

save_plot("figures/mema_grid-full.png",
          grid_plot, 
          base_height = 14,
          base_width  = 9)
