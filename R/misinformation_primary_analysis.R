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

# Primary analysis--------------------------------------------------------------

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
                                   + publication_year,
                         data    = data_es,
                         method  = "REML", 
                         control = list(
                           iter.max  = 1000,
                           rel.tol   = 1e-8
                         ),
                         verbose = TRUE)

meta_primary_ranef <- ranef(meta_primary)

saveRDS(meta_primary, "output/misinformation_meta_primary.rds")

# Bias correction --------------------------------------------------------------

# PET

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
                                  + I(vi^2),
                         data   = data_es,
                         method = "REML",
                         control = list(
                           iter.max  = 1000,
                           rel.tol   = 1e-8
                         ),
                         verbose = TRUE)

saveRDS(meta_pet, "output/misinformation_meta_pet.rds")

# PEESE

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
                                  + I(vi),
                         data   = data_es,
                         method = "REML",
                         control = list(
                           iter.max  = 1000,
                           rel.tol   = 1e-8
                         ),
                         verbose = TRUE)

saveRDS(meta_peese, "output/misinformation_meta_peese.rds")

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

# Influence analysis ----------------------------------------------------------

# Cook's Distance

cores      <- parallel::detectCores() - 1

cooks_dist <- cooks.distance.rma.mv(meta_primary_2,
                               progbar  = TRUE,
                               parallel = 'snow',
                               ncpus    = cores) 


data_es <- cbind(data_es, cooks_dist)

n   <- nrow(data_es)

ggplot(data_es, aes(y = cooks_dist, x = id_effect, label = id_effect))+
  geom_jitter(alpha = .5) +
  geom_text(aes(
    label=ifelse(cooks_dist> 4/n ,
                 as.character(id_effect), '')), hjust = -.2, vjust = 0)
#Note. Effect 862 has a relativly large cooks distance.  

round(summary((data_es$cooks_dist)), 7)

# Extracting outliers at the arbitrary 4/n threshold. 
outliers <- ifelse(data_es$cooks_dist <= 4/n, 0, 1)

# Sensitivity analysis----------------------------------------------------------

# Remove control accuracy

meta_no_acc    <- rma.mv(yi      = yi, 
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
                         # + control_acc
                         + postevent_recall
                         + postexposure_recall
                         + publication_year,
                         data    = data_es,
                         method  = "REML", 
                         control = list(
                           iter.max  = 1000
                         ),
                         verbose = TRUE)

# Outlier removal

if (!file.exists('models/meta_primary_outlier.rds')) {
  meta_primary_outlier <- rma.mv(
                          yi       = yi,
                          V        = vi,
                          random   = list(~1|id_record/id_study/id_control, 
                                          ~1|event_materials),
                          data     = filter(data_es, outliers == 0),
                          method   = 'REML'
)

saveRDS(meta_primary_outlier,'models/meta_primary_outlier.rds')
} else {
  
  meta_primary_outlier <- readRDS('models/meta_primary_outlier.rds')
  
}
summary(meta_primary_outlier)
I2_mv(meta_primary_outlier, filter(data_es, outliers == 0))
filter(data_es, outliers == 0)

funnel(meta_primary_2, label = T)
funnel(meta_primary_outlier, label = F)
outlier_2 <- data_es %>% filter(yi > 5)

outlier_2 <- ifelse(data_es$yi >= 5.3, 1, 0)

es_data_outlier_2 <- data_es %>% 
  filter(outlier_2 == 0 & outliers == 0)

if (!file.exists('models/meta_primary_outlier_2.rds')) {
  meta_primary_outlier_2 <- rma.mv(
    yi       = yi,
    V        = vi,
    random   = list(~1|id_record/id_study/id_control, 
                    ~1|event_materials),
    data     = es_data_outlier_2,
    method   = 'REML'
  )
  
  saveRDS(meta_primary_outlier_2,'models/meta_primary_outlier_2.rds')
} else {
  
  meta_primary_outlier_2 <- readRDS('models/meta_primary_outlier_2.rds')
  
}

summary(meta_primary_outlier_2)
I2_mv(meta_primary_outlier_2, es_data_outlier_2)

funnel(meta_primary_outlier_2)










