################################################################################

# Misinformation Meta-analysis - Main analysis 

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

## I-squared

# This code is adapted from
# https://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate

I2_calc <- function(meta_model) {
  
  W <- diag(1/meta_model$vi)
  
  X <- model.matrix(meta_model)
  
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  
  I2 <- 100 * sum(meta_model$sigma2) / (sum(meta_model$sigma2) + (meta_model$k - meta_model$p)/sum(diag(P)))
  
  I2_components <- round(100 * meta_model$sigma2 / (sum(meta_model$sigma2) + (meta_model$k-meta_model$p)/sum(diag(P))), 2)
  
  sigma2 <- sum(meta_model$sigma2)
  
  return(list(sigma2 = sigma2, I2 = I2, I2_comp = I2_components))
  
}

I2_primary <- I2_calc(meta_primary)

## Intercept prediction interval

pi_intercept <- function(meta_model, pib = .975) {
  
  b0    <- meta_model$beta[[1]]
  
  sd_pi <- sqrt(sum(meta_model$sigma2) + meta_model$se[[1]]^2)
  
  pi_lb <- b0 - qnorm(.975)*sd_pi
  pi_ub <- b0 + qnorm(.975)*sd_pi
  
  return(c(pi_lb, pi_ub))
  
}

pi_primary <- pi_intercept(meta_primary)

# Quadratic effect of control accuracy

if (!file.exists("output/mema_quad.rds")) {
  
  meta_quad   <- rma.mv(yi      = yi, 
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
                        + I(control_acc^2)
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
  
  saveRDS(meta_quad, "output/mema_quad.rds")
  
} else {
  
  meta_quad <- readRDS("output/mema_quad.rds")
  
}

meta_quad_ranef <- ranef(meta_quad)

I2_quad <- I2_calc(meta_quad)

pi_quad <- pi_intercept(meta_quad)

# Model comparison

if (!file.exists("output/mema_comparison.rds")) {
  
  meta_comparison <- anova(meta_primary, meta_quad, refit = TRUE)
  
  saveRDS(meta_comparison, "output/mema_comparison.rds")
  
} else {
  
  meta_comparison <- readRDS("output/mema_comparison.rds")
  
}

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

I2_leverage <- I2_calc(meta_leverage)

pi_leverage <- pi_intercept(meta_leverage)

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

I2_resid <- I2_calc(meta_resid)

pi_resid <- pi_intercept(meta_resid)

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

I2_imp <- I2_calc(meta_primary_imp)

pi_imp <- pi_intercept(meta_primary_imp)

# Simple model -----------------------------------------------------------------

data_included <- data_es[as.numeric(rownames(as.data.frame(predict(meta_primary)))), ]

write_csv(data_included, "data/mema_data-included.csv")

if (!file.exists("output/mema_simple.rds")) {
  
  meta_simple    <- rma.mv(yi      = yi, 
                           V       = vi,
                           random  = list(~1|id_record/id_study/id_control),
                           data    = data_included,
                           method  = "REML", 
                           control = list(
                             iter.max  = 1000,
                             rel.tol   = 1e-8
                           ),
                           verbose = TRUE)
  
  saveRDS(meta_simple, "output/mema_simple.rds")
  
} else {
  
  meta_simple <- readRDS("output/mema_simple.rds")
  
}

I2_simple <- I2_calc(meta_simple)

pi_simple <- pi_intercept(meta_simple)

if (!file.exists("output/mema_simple-comparison.rds")) {
  
  meta_simple_comparison <- anova(meta_primary, meta_simple, refit = TRUE)
  
  saveRDS(meta_simple_comparison, "output/mema_simple-comparison.rds")
  
} else {
  
  meta_simple_comparison <- readRDS("output/mema_simple-comparison.rds")
  
}

# Moderator and Performance Visualizations -------------------------------------

source("R/mema_performance-moderator-plots.R")

