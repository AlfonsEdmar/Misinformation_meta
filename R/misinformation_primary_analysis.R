################################################################################

# Misinformation Meta-analysis - Main analysis 

################################################################################


# Packages ---------------------------------------------------------------------

library(tidyverse)
library(metafor)

# Functions --------------------------------------------------------------------

I2_mv <- function(meta, data) {
  
  # This code is adapted from Viechtbauer (2010)
  # http://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate
  
  W  <- diag(1/data$vi)
  
  X  <- model.matrix(meta)
  
  P  <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  
  I2 <- 100 * sum(meta$sigma2) / (sum(meta$sigma2) + (meta$k - meta$p)/sum(diag(P)))
  
  I2_split <- 100 * meta$sigma2 / (sum(meta$sigma2) + (meta$k - meta$p)/sum(diag(P)))
  
  return(list(I2 = I2, I2_split = I2_split))
  
}

# Load data --------------------------------------------------------------------

data_cleaned <- read_csv("data/complete_data_cleaned.csv")

# Wrangling --------------------------------------------------------------------

prop_data <- data_cleaned %>%
  filter(is.na(total_accuracy_control_mean))

mean_data <- data_cleaned %>%
  filter(is.na(total_accuracy_control_prop))


# Calculating effect sizes -----------------------------------------------------

## Calculating log odds ratio

es_prop <- escalc(data    = prop_data,
                  ci      = total_accuracy_mi_prop * n_mi,
                  di      = (1 - total_accuracy_mi_prop) * n_mi,
                  ai      = total_accuracy_control_prop * n_control,
                  bi      = (1 - total_accuracy_control_prop) * n_control,
                  measure = 'OR')

### Transforming log odds to standardized mean difference

es_prop <- es_prop %>% 
  mutate(yi = yi * (sqrt(3)/pi),
         vi = vi * (3/(pi^2)))

## Calculating Hedges's g (standardized mean difference)

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

# Initial model

if (!file.exists('models/meta_primary.rds')) {
  
  meta_primary <- rma.mv(yi       = yi, 
                         V        = vi,
                         random   = list(~1|id_record/id_study/id_control),
                         data     = data_es,
                         method   = 'REML'
  )
  
  saveRDS(meta_primary,'models/meta_primary.rds')
  
} else {
  
  meta_primary <- readRDS('models/meta_primary.rds')
  
}
summary(meta_primary)
I2_mv(meta_primary, data_es)

# Adding event materials

if (!file.exists('models/meta_primary_2.rds')) {
  
  meta_primary_2 <- rma.mv(yi     = yi, 
                           V      = vi,
                           random = list(~1|id_record/id_study/id_control, 
                                         ~1|event_materials),
                           data   = data_es,
                           method = 'REML'
  )
  
  saveRDS(meta_primary_2,'models/meta_primary_2.rds')
  
} else {
  
  meta_primary_2 <- readRDS('models/meta_primary_2.rds')
  
}
summary(meta_primary_2)
I2_mv(meta_primary_2, data_es)

# Adding event materials and country

if (!file.exists('models/meta_primary_3.rds')) {
  
  meta_primary_3 <- rma.mv(yi     = yi, 
                           V      = vi,
                           random = list(~1|id_record/id_study/id_control, 
                                         ~1|event_materials ,
                                         ~1|country),
                           data   = data_es,
                           method = 'REML'
  )
  
  saveRDS(meta_primary_3,'models/meta_primary_3.rds')
  
} else {
  
  meta_primary_3 <- readRDS('models/meta_primary_3.rds')
  
}
summary(meta_primary_3)
I2_mv(meta_primary_3, data_es)

# Adding country (without event materials)

if (!file.exists('models/meta_primary_3b.rds')) {
  
  meta_primary_3b <- rma.mv(yi     = yi, 
                            V      = vi,
                            random = list(~1|id_record/id_study/id_control, 
                                          ~1|country),
                            data   = data_es,
                            method = 'REML')
  
  saveRDS(meta_primary_3b,'models/meta_primary_3b.rds')
  
} else {
  
  meta_primary_3b <- readRDS('models/meta_primary_3b.rds')
  
}
summary(meta_primary_3b)
I2_mv(meta_primary_3b, data_es)

## Model comparisons

anova(meta_primary,
      meta_primary_2)

anova(meta_primary,
      meta_primary_3b)

anova(meta_primary,
      meta_primary_3)

anova(meta_primary_2,
      meta_primary_3)

anova(meta_primary_2,
      meta_primary_3b)

#Note: meta_primary_2 is the best fitting model.

## Heterogeneity estimates

I2_meta <- I2_mv(meta_primary_2, data_es)

# Bias correction -------------------------------------------------------------- 

if (!file.exists('models/meta_pet.rds')) {
  
  meta_pet     <- rma.mv(yi       = yi, 
                         V        = vi,
                         mods     = ~ I(vi^2),
                         random   = list(~1|id_record/id_study/id_control, 
                                         ~1|event_materials),
                         data     = data_es,
                         method   = 'REML'
                         )
  
  saveRDS(meta_pet,'models/meta_pet.rds')
  
} else {
  
  meta_pet <- readRDS('models/meta_pet.rds')
  
}
summary(meta_pet)

if (!file.exists('models/meta_peese.rds')) {
  
  meta_peese   <- rma.mv(yi       = yi, 
                         V        = vi,
                         mods     = ~ I(vi),
                         random   = list(~1|id_record/id_study/id_control, 
                                         ~1|event_materials),
                         data     = data_es,
                         method   = 'REML'
                         )
  
  saveRDS(meta_peese,'models/meta_peese.rds')
  
} else {
  
  meta_peese <- readRDS('models/meta_peese.rds')
  
}
summary(meta_peese)

funnel(meta_primary, label = TRUE)

# Excluding outliers


if (!file.exists('models/meta_pet_2.rds')) {
  
  meta_pet_2   <- rma.mv(yi       = yi, 
                         V        = vi,
                         mods     = ~ I(vi^2),
                         random   = list(~1|id_record/id_study/id_control, 
                                         ~1|event_materials),
                         data     = es_data_outlier_2,
                         method   = 'REML'
  )
  
  saveRDS(meta_pet_2,'models/meta_pet_2.rds')
  
} else {
  
  meta_pet_2 <- readRDS('models/meta_pet_2.rds')
  
}
summary(meta_pet_2)

if (!file.exists('models/meta_peese_2.rds')) {
  
  meta_peese_2 <- rma.mv(yi       = yi, 
                         V        = vi,
                         mods     = ~ I(vi),
                         random   = list(~1|id_record/id_study/id_control, 
                                         ~1|event_materials),
                         data     = es_data_outlier_2,
                         method   = 'REML'
  )
  
  saveRDS(meta_peese_2,'models/meta_peese_2.rds')
  
} else {
  
  meta_peese_2 <- readRDS('models/meta_peese_2.rds')
  
}
summary(meta_peese_2)


## Influence analysis ----------------------------------------------------------

if (!file.exists('data/cooks_dist_data.csv')) {
  
  cores <- parallel::detectCores() - 1
  inf   <- cooks.distance.rma.mv(meta_primary_2,
                               progbar  = TRUE,
                               parallel = 'snow',
                               ncpus    = cores) 
  
  
  data_es <- cbind(data_es, 'cooks_dist' = inf)
  write.csv(data_es, 'data/cooks_dist_data.csv')
  
} else {
  
  data_cooks <- read.csv('data/cooks_dist_data.csv')
  data_es    <- data.frame(data_es, cooks_dist = data_cooks$cooks_dist)
  
}

n   <- NROW(data_es)
ggplot(data_es, aes(y = cooks_dist, x = id_effect, label = id_effect))+
  geom_jitter(alpha = .5) +
  geom_text(aes(
    label=ifelse(cooks_dist> 4/n ,
                 as.character(id_effect), '')), hjust = -.2, vjust = 0)
#Note. Effect 862 has a relativly large cooks distance.  

round(summary((data_es$cooks_dist)), 7)

# Extracting outliers at the arbitrary 4/n threshold. 
outliers <- ifelse(data_es$cooks_dist <= .00288, 0, 1)

# Sensitivity analysis----------------------------------------------------------

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

outlier_2 <- ifelse(data_es$yi >= 5, 1, 0)

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

# P-value analysis--------------------------------------------------------------

data_es$p_val <- 2*pt(-abs(data_es$yi/sqrt(data_es$vi)), df = Inf)
median(data_es$p_val)
mean(data_es$p_val)

data_es %>% 
  ggplot()+
  geom_histogram(aes(p_val), col = 'white', bins = 80)+
  xlim(0,1)+
  xlab(label = 'P-values')+
  ylim(0,100)+
  ylab(label = 'Count')+
  geom_vline(xintercept = .05, linetype = 'dashed')
ggsave('visuals/p_hist.png', dpi = 300)

data_es %>% 
  filter(p_val <= .1) %>% 
  ggplot()+
  geom_histogram(aes(p_val), col = 'white', bins = 80)+
  xlim(0,.1)+
  xlab(label = 'P-values')+
  ylim(0,80)+
  ylab(label = 'Count')+
  geom_vline(xintercept = .05, linetype = 'dashed')
ggsave('visuals/p_hist_trunc.png', dpi = 300)

# Number of significant effects
data_es %>% 
  filter(p_val <= .05) %>% 
  summarise(n = NROW(p_val))


sim <- data.frame("studlab" = data_es$authors,
                 "TE" = data_es$yi,
                 "seTE" = sqrt(data_es$vi))


if (!file.exists('models/meta_mod_co_type.rds')) {
  
  sim <- data.frame("studlab" = data_es$authors,
                    "TE" = data_es$yi,
                    "seTE" = sqrt(data_es$vi))
  p_curve <- dmetar::pcurve(sim)
  
  saveRDS(p_curve,'models/p_curve.rds')
  
} else {
  
  p_curve <- readRDS('models/p_curve.rds')
  
}

p_curve

d <- (rexp(n = 1385, rate = 11))
ggplot()+
  geom_density(aes(d), col = 'red')+
  geom_density(aes(data_es$p_val))



