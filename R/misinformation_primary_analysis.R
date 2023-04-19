################################################################################

# Misinformation Meta-analysis - Main analysis 

################################################################################


# Packages----------------------------------------------------------------------

library(tidyverse)
library(metafor)

# Functions---------------------------------------------------------------------

I2_mv <- function(meta, data) {
  
  # This code is adapted from Viechtbauer (2010)
  # http://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate#dokuwiki__top
  
  W  <- diag(1/data$vi)
  
  X  <- model.matrix(meta)
  
  P  <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  
  I2 <- 100 * sum(meta$sigma2) / (sum(meta$sigma2) + (meta$k - meta$p)/sum(diag(P)))
  
  I2_split <- 100 * meta$sigma2 / (sum(meta$sigma2) + (meta$k - meta$p)/sum(diag(P)))
  
  return(list(I2 = I2, I2_split = I2_split))
  
}

# Load data---------------------------------------------------------------------

data_cleaned <- read_csv("data/complete_data_cleaned.csv")


# Wrangling---------------------------------------------------------------------

prop_data <- data_cleaned %>%
  filter(is.na(total_accuracy_control_mean))

mean_data <- data_cleaned %>%
  filter(is.na(total_accuracy_control_prop))


# Calculating effect sizes------------------------------------------------------


## Calculating log odds ratio

es_prop <- escalc(data    = prop_data,
                  ci      = total_accuracy_mi_prop * n_mi,
                  di      = (1 - total_accuracy_mi_prop) * n_mi,
                  ai      = total_accuracy_control_prop * n_control,
                  bi      = (1 - total_accuracy_control_prop) * n_control,
                  measure = 'OR')

## Transforming log odds

es_prop <- es_prop %>% 
  mutate(yi = yi * (sqrt(3)/pi),
         vi = vi * (3/(pi^2)))

## Calculating hedges G 

es_mean <- escalc(data    = mean_data,
                  m1i     = total_accuracy_control_mean,
                  m2i     = total_accuracy_mi_mean,
                  sd1i    = total_accuracy_control_sd,
                  sd2i    = total_accuracy_mi_sd,
                  n1i     = n_control,
                  n2i     = n_mi,
                  measure = 'SMD')

data_es <- rbind(es_mean, es_prop)

# Primary analysis--------------------------------------------------------------

meta_primary <- rma.mv(yi       = yi, 
                       V        = vi,
                       random   = list(~1|id_record/id_study/id_control),
                       data     = data_es,
                       method   = 'REML'
                      )
saveRDS(meta_primary,'models/meta_primary.rds' )

meta_primary_2 <- rma.mv(yi     = yi, 
                         V      = vi,
                         random = list(~1|id_record/id_study/id_control, 
                                       ~1|event_materials ),
                         data   = data_es,
                         method = 'REML'
                        )
saveRDS(meta_primary,'models/meta_primary_2.rds' )

meta_primary_3 <- rma.mv(yi     = yi, 
                         V      = vi,
                         random = list(~1|id_record/id_study/id_control, 
                                       ~1|event_materials ,
                                       ~1|country),
                         data   = data_es,
                         method = 'REML'
                        )
saveRDS(meta_primary,'models/meta_primary_3.rds' )

meta_primary_3b <- rma.mv(yi     = yi, 
                          V      = vi,
                          random = list(~1|id_record/id_study/id_control, 
                                        ~1|country),
                          data   = data_es,
                          method = 'REML')
saveRDS(meta_primary,'models/meta_primary_3b.rds' )



## Model comparisons
anova(meta_primary,
      meta_primary_2)

anova(meta_primary,
      meta_primary_3b)

anova(meta_primary,
      meta_primary_3)

anova(meta_primary_2,
      meta_primary_3)

## Bias correction 

meta_pet     <- rma.mv(yi       = yi, 
                       V        = vi,
                       mods     = ~ I(vi^2),
                       random   = list(~1|id_record/id_study/id_control),
                       data     = data_es,
                       method   = 'REML'
                      )

meta_peese   <- rma.mv(yi       = yi, 
                       V        = vi,
                       mods     = ~ I(vi),
                       random   = list(~1|id_record/id_study/id_control),
                       data     = data_es,
                       method   = 'REML'
                      )

funnel(meta_primary, label = T)

## Influence analysis 

cores <- parallel::detectCores()-1
inf <- cooks.distance.rma.mv(test, progbar = T,
                             parallel      = 'snow',
                             ncpus         = cores) 

n   <- NROW(inf)

test_dat <- cbind(test_dat, 'cooks_dist' = inf)

ggplot(test_dat, aes(y = cooks_dist, x = id_effect, label = id_effect))+
  geom_point() +
  geom_text(aes(
    label=ifelse(cooks_dist> 4/n ,
                 as.character(id_effect),'')), hjust = 1.5,vjust = 0)


# Moderator analysis------------------------------------------------------------
meta_mod_test_type <-  rma.mv(
                         yi     = yi, 
                         V      = vi,
                         random = list(~1|id_record/id_study/id_control),
                         mods   = ~ as.factor(test_type)-1,
                         data   = data_es,
                         method   = 'REML'
                         )

meta_mod_postev_test <-  rma.mv(
                         yi     = yi, 
                         V      = vi,
                         random = list(~1|id_record/id_study/id_control),
                         mods   = ~ postevent_recall,
                         data   = data_es,
                         method = 'REML'
                         )

meta_mod_age <-  rma.mv(
                         yi     = yi, 
                         V      = vi,
                         random = list(~1|id_record/id_study/id_control),
                         mods   = ~ age_mean,
                         data   = data_es,
                         method = 'REML'
                         )

meta_mod_age_quad <-  rma.mv(
                         yi     = yi, 
                         V      = vi,
                         random = list(~1|id_record/id_study/id_control),
                         mods   = ~ I(age_mean^2),
                         data   = data_es,
                         method = 'REML'
                         )

meta_mod_postev_ret <-  rma.mv(
                         yi     = yi, 
                         V      = vi,
                         random = list(~1|id_record/id_study/id_control),
                         mods   = ~ postevent_retention_interval,
                         data   = data_es,
                         method = 'REML'
                         )

meta_mod_postex_ret <-  rma.mv(
                         yi     = yi, 
                         V      = vi,
                         random = list(~1|id_record/id_study/id_control),
                         mods   = ~ postexposure_retention_interval,
                         data   = data_es,
                         method = 'REML'
                         )

meta_mod_gender <-  rma.mv(
                         yi     = yi, 
                         V      = vi,
                         random = list(~1|id_record/id_study/id_control),
                         mods   = ~ gender_female_prop,
                         data   = data_es,
                         method = 'REML'
                         )

meta_mod_postex_warn <- rma.mv(
                         yi     = yi, 
                         V      = vi,
                         random = list(~1|id_record/id_study/id_control),
                         mods   = ~ postexposure_warning,
                         data   = data_es,
                         method = 'REML'
                         )



meta_mod_year <-  rma.mv(
                         yi     = yi, 
                         V      = vi,
                         random = list(~1|id_record/id_study/id_control),
                         mods   = ~ as.numeric(publication_year),
                         data   = data_es,
                         method = 'REML'
                         )

meta_mod_modality <-  rma.mv(yi = yi, 
                         V      = vi,
                         random = list(~1|id_record/id_study/id_control),
                         mods   = ~ as.factor(modality) -1,
                         data   = data_es,
                         method = 'REML'
                         )

meta_mod_control_acc <-  rma.mv(    
                         yi     = yi, 
                         V      = vi,
                         random = list(~1|id_record/id_study/id_control),
                         mods   = ~ total_accuracy_control_mean,
                         data   = data_es,
                         method = 'REML'
                         )

meta_mod_control_acc_quad <-  rma.mv(
                         yi     = yi, 
                         V      = vi,
                         random = list(~1|id_record/id_study/id_control),
                         mods   = ~ I(total_accuracy_control_mean^2),
                         data   = data_es,
                         method = 'REML'
                         )







