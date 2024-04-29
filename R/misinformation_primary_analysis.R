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

if (!file.exists("output/misinformation_meta_leverage.rds")) {
  
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
                             iter.max  = 1000,
                             rel.tol   = 1e-8
                           ),
                           verbose = TRUE)
  
  saveRDS(meta_leverage, "output/misinformation_meta_leverage.rds")
  
} else {
  
  meta_leverage <- readRDS("output/misinformation_meta_leverage.rds")
  
}

meta_leverage_ranef <- ranef(meta_leverage)

# Excluding all cases with standardized residuals greater than |1.96|

if (!file.exists("output/misinformation_meta_resid.rds")) {
  
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
                             iter.max  = 1000,
                             rel.tol   = 1e-8
                           ),
                           verbose = TRUE)
  
  saveRDS(meta_resid, "output/misinformation_meta_resid.rds")
  
} else {
  
  meta_resid <- readRDS("output/misinformation_meta_resid.rds")
  
}

meta_resid_ranef <- ranef(meta_resid)

# P-curve analysis -------------------------------------------------------------

# Calculating power and p-values - we assume equal sample sizes

power_1 <- power.t.test(n = data_es$n_control, delta = data_es$yi, 
                        sd = sqrt(data_es$vi), sig.level = .05,
                        type = 'two.sample')

power_2 <- power.t.test(n = data_es$n_control, delta = data_es$yi, 
                      sd = sqrt(data_es$vi), sig.level = .05,
                      type = 'paired')

data_es <- data_es %>% 
  mutate(
    p_value = 2 * (1- pnorm(abs(data_es$yi/sqrt(data_es$vi)))),
    post_hoc_power  = case_when(
      within_between=='between' ~ power_1$power,
      within_between=='within' ~  power_2$power)
    )


table(high_power = ifelse(data_es$post_hoc_power > .8, 1, 0),
      sig = ifelse(data_es$p_value < .05, 1, 0))
# We have no high powered insignificant effects
# We have 244 low powered insignificant effects 
# We have 463 high powered insignificant effects
# We have 1059 high powered significant effects


data_es %>% ggplot(aes(y = p_value, x = post_hoc_power)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  geom_hline(yintercept = .05) +
  geom_vline(xintercept = .8)

set.seed(123)
data_es %>% filter(p_value < .1& p_value > .001) %>% 
  slice_sample(by = id_record) %>% 
  ggplot(aes(x = p_value))+
  geom_histogram(bins = 50)

# Independence of P-values are assumed. Since we have many within study effects,
# we can do a multiverse analysis

# Selecting variables from data_es

study_data <- data_es %>%  
  slice_sample(n = 30) %>% select(id_record, id_effect, p_value)

# Get unique study IDs
study_data %>%  group_by(id_record) %>%  expand.grid()


# Subgroup analyses-------------------------------------------------------------

# Age analysis

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

age_data$age_cat <- cut(age_data$age_mean, 
                        breaks = breaks, 
                        labels = labels, 
                        include.lowest = TRUE)

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

saveRDS(meta_age_no_acc, 'output/misinformation_meta_age_cat.rds')

# Incentives

incent_data <- data_es %>% filter(!is.na(data_es$incentives))
incent_data$incentives <- as.factor(incent_data$incentives)

# # Setting the reference to be no incentive

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

saveRDS(meta_incent, "output/misinformation_meta_incent.rds")

# Item Centrality 

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

saveRDS(meta_centrality, "output/misinformation_meta_centrality.rds")
