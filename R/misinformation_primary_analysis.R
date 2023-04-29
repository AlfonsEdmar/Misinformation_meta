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
write_csv2(data_es, 'data/misinformation_data_es.csv')

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
outliers <- ifelse(data_es$cooks_dist <= 4/n, 0, 1)

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
extreme_cases <- data_es %>% filter(yi > 5)
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

# Fitting the model on the data without imputed variances. 

raw_data <- read_csv("data/misinformation_data_cleaned.csv")

prop_data_raw <- raw_data %>%
  filter(is.na(total_accuracy_control_mean))

mean_data_raw <- raw_data %>%
  filter(is.na(total_accuracy_control_prop))


# Calculating log odds ratio

es_prop_raw <- escalc(data    = prop_data_raw,
                      ci      = total_accuracy_mi_prop * n_mi,
                      di      = (1 - total_accuracy_mi_prop) * n_mi,
                      ai      = total_accuracy_control_prop * n_control,
                      bi      = (1 - total_accuracy_control_prop) * n_control,
                      measure = 'OR')

# Transforming log odds to standardized mean difference

es_prop_raw <- es_prop_raw %>% 
  mutate(yi = yi * (sqrt(3)/pi),
         vi = vi * (3/(pi^2)))

# Calculating Hedges's g (standardized mean difference)

es_mean_raw <- escalc(data    = mean_data_raw,
                      m1i     = total_accuracy_control_mean,
                      m2i     = total_accuracy_mi_mean,
                      sd1i    = total_accuracy_control_sd,
                      sd2i    = total_accuracy_mi_sd,
                      n1i     = n_control,
                      n2i     = n_mi,
                      measure = 'SMD')

data_es_raw <- rbind(es_mean_raw, es_prop_raw)

if (!file.exists('models/meta_primary_raw.rds')) {
  meta_primary_raw <- rma.mv(
    yi       = yi,
    V        = vi,
    random   = list(~1|id_record/id_study/id_control, 
                    ~1|event_materials),
    data     = data_es_raw,
    method   = 'REML'
  )
  
  saveRDS(meta_primary_raw,'models/meta_primary_raw.rds')
} else {
  
  meta_primary_raw <- readRDS('models/meta_primary_raw.rds')
  
}
summary(meta_primary_raw)
funnel(meta_primary_raw)
#Note. the two very large effects come from have very small imputed variances

# Moderator analysis -----------------------------------------------------------

# Test type

if (!file.exists('models/meta_mod_test_type.rds')) {
  
  meta_mod_test_type <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ as.factor(test_type) - 1,
    data   = data_es,
    method = 'REML'
  )
  
 
  
  saveRDS(meta_mod_test_type,'models/meta_mod_test_type.rds')
  
} else {
  
  meta_mod_test_type <- readRDS('models/meta_mod_test_type.rds')
  
}
summary(meta_mod_test_type)
I2_mv(meta_mod_test_type, data_es)

effects_by_recall_type <- data_es %>% 
  group_by(test_type) %>% 
  summarise(n = n_distinct(id_effect))

effects_by_recall_type

# Number of post-event tests

if (!file.exists('models/meta_mod_postev_test.rds')) {
  
  meta_mod_postev_test <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ postevent_recall,
    data   = data_es,
    method = 'REML'
  )
  
  saveRDS(meta_mod_postev_test,'models/meta_mod_postev_test.rds')
  
} else {
  
  meta_mod_postev_test <- readRDS('models/meta_mod_postev_test.rds')
  
}

# Limiting the scope to testing studies

postev_test_studies <- data_es %>% 
  filter(postevent_recall > 0) %>% 
  select(id_record)
postev_test_studies <- as.vector(postev_test_studies$id_record)
postev_test_studies <- unique(postev_test_studies)
noquote(paste(postev_test_studies, collapse = '| id_record == '))
postev_test_studies <- data_es %>% 
  filter(id_record == 006| id_record == 011| id_record == 022| id_record == 026|
           id_record == 068| id_record == 085| id_record == 119| id_record == 137|
           id_record == 021| id_record == 023| id_record == 034| id_record == 035| 
           id_record == 036| id_record == 037| id_record == 042| id_record == 053|
           id_record == 054| id_record == 055| id_record == 056| id_record == 057| 
           id_record == 069| id_record == 091| id_record == 096| id_record == 102| 
           id_record == 118| id_record == 121| id_record == 123| id_record == 131| 
           id_record == 132| id_record == 133| id_record == 136| id_record == 144| 
           id_record == 151| id_record == 174| id_record == 181| id_record == 190| 
           id_record == 195| id_record == 203| id_record == 204| id_record == 205| 
           id_record == 206| id_record == 207| id_record == 208| id_record == 209| 
           id_record == 212| id_record == 213| id_record == 218| id_record == 223| 
           id_record == 224| id_record == 238| id_record == 241| id_record == 247| 
           id_record == 253| id_record == 262| id_record == 263| id_record == 264| 
           id_record == 261)


if (!file.exists('models/meta_mod_postev_test_2.rds')) {
  
  meta_mod_postev_test_2 <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ postevent_recall,
    data   = postev_test_studies,
    method = 'REML'
  )
  
  saveRDS(meta_mod_postev_test_2,'models/meta_mod_postev_test_2.rds')
  
} else {
  
  meta_mod_postev_test_2 <- readRDS('models/meta_mod_postev_test_2.rds')
  
}

summary(meta_mod_postev_test_2)


# Age
if (!file.exists('models/meta_mod_age.rds')) {
  
  meta_mod_age <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ as.numeric(age_mean),
    data   = data_es,
    method = 'REML'
  )
  
  saveRDS(meta_mod_age,'models/meta_mod_age.rds')
  
} else {
  
  meta_mod_age <- readRDS('models/meta_mod_age.rds')
  
}
summary(meta_mod_age)

if (!file.exists('models/meta_mod_age_quad.rds')) {
  
  meta_mod_age_quad <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ I(as.numeric(age_mean^2)),
    data   = data_es,
    method = 'REML'
  )
  
  saveRDS(meta_mod_age_quad,'models/meta_mod_age_quad.rds')
  
} else {
  
  meta_mod_age_quad <- readRDS('models/meta_mod_age_quad.rds')
  
}
summary(meta_mod_age_quad)

if (!file.exists('models/meta_mod_age_full.rds')) {
  
  meta_mod_age_full <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ as.numeric(age_mean) + I(as.numeric(age_mean^2)),
    data   = data_es,
    method = 'REML'
  )
  
  saveRDS(meta_mod_age_full,'models/meta_mod_age_full.rds')
  
} else {
  
  meta_mod_age_full <- readRDS('models/meta_mod_age_full.rds')
  
}
summary(meta_mod_age_full)

## Post-event retention interval

if (!file.exists('models/meta_mod_postev_ret.rds')) {
  
  meta_mod_postev_ret <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ I(postevent_retention_interval/24),
    data   = data_es,
    method = 'REML'
  )
  
  saveRDS(meta_mod_postev_ret,'models/meta_mod_postev_ret.rds')
  
} else {
  
  meta_mod_postev_ret <- readRDS('models/meta_mod_postev_ret.rds')
  
}
summary(meta_mod_postev_ret)

## Reducing the interval to exclude extreme values
if (!file.exists('models/meta_mod_postev_ret_2.rds')) {
  
  meta_mod_postev_ret_2 <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ I(postevent_retention_interval/24),
    data   = filter(data_es, 
                    postevent_retention_interval < 2500 &
                      postevent_retention_interval > 24),
    method = 'REML'
  )
  
  saveRDS(meta_mod_postev_ret_2,'models/meta_mod_postev_ret_2.rds')
  
} else {
  
  meta_mod_postev_ret_2 <- readRDS('models/meta_mod_postev_ret_2.rds')
  
}
summary(meta_mod_postev_ret_2)


## comparing the recall across one week. 
if (!file.exists('models/meta_mod_postev_ret_3.rds')) {
  
  meta_mod_postev_ret_3 <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ I(postevent_retention_interval/24),
    data   = filter(data_es, 
                    postevent_retention_interval < 170),
    method = 'REML'
  )
  
  saveRDS(meta_mod_postev_ret_3,'models/meta_mod_postev_ret_3.rds')
  
} else {
  
  meta_mod_postev_ret_3 <- readRDS('models/meta_mod_postev_ret_3.rds')
  
}
summary(meta_mod_postev_ret_3)

# Post-exposure retention interval

if (!file.exists('models/meta_mod_postex_ret.rds')) {
  
  meta_mod_postex_ret <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ I(postexposure_retention_interval/24),
    data   = data_es,
    method = 'REML'
  )
  
  saveRDS(meta_mod_postex_ret,'models/meta_mod_postex_ret.rds')
  
} else {
  
  meta_mod_postex_ret <- readRDS('models/meta_mod_postex_ret.rds')
  
}

summary(meta_mod_postex_ret)

if (!file.exists('models/meta_mod_postex_ret_2.rds')) {
  
  meta_mod_postex_ret_2 <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ I(postexposure_retention_interval/24),
    data   = filter(data_es, 
                    postexposure_retention_interval < 2500 &
                      postexposure_retention_interval > 24),
    method = 'REML'
  )
  
  saveRDS(meta_mod_postex_ret_2,'models/meta_mod_postex_ret_2.rds')
  
} else {
  
  meta_mod_postex_ret_2 <- readRDS('models/meta_mod_postex_ret_2.rds')
  
}

summary(meta_mod_postex_ret_2)

# Week intervals

if (!file.exists('models/meta_mod_postex_ret_3.rds')) {
  
  meta_mod_postex_ret_3 <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ I(postexposure_retention_interval/24),
    data   = filter(data_es, 
                    postexposure_retention_interval < 170),
    method = 'REML'
  )
  
  saveRDS(meta_mod_postex_ret_3,'models/meta_mod_postex_ret_3.rds')
  
} else {
  
  meta_mod_postex_ret_3 <- readRDS('models/meta_mod_postex_ret_3.rds')
  
}

summary(meta_mod_postex_ret_3)

# Gender (proportion female)

if (!file.exists('models/meta_mod_gender.rds')) {
  
  meta_mod_gender <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ gender_female_prop,
    data   = data_es,
    method = 'REML'
  )
  
  saveRDS(meta_mod_gender,'models/meta_mod_gender.rds')
  
} else {
  
  meta_mod_gender <- readRDS('models/meta_mod_gender.rds')
  
}

summary(meta_mod_gender)

# Post-exposure warnings

if (!file.exists('models/meta_mod_postex_warn.rds')) {
  
  meta_mod_postex_warn <- rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ postexposure_warning,
    data   = data_es,
    method = 'REML'
  )
  
  saveRDS(meta_mod_postex_warn,'models/meta_mod_postex_warn.rds')
  
} else {
  
  meta_mod_postex_warn <- readRDS('models/meta_mod_postex_warn.rds')
  
}

# Limiting scope to only warning studies

postex_warn_studies <- data_es %>% 
  filter(postexposure_warning > 0 & !duplicated(id_record)) %>% 
  select(id_record)
postex_warn_studies <- postex_warn_studies$id_record
noquote(paste(postex_warn_studies, collapse = '| id_record == '))
postex_warn_studies <- data_es %>%
  filter(id_record == 001| id_record == 019| id_record == 089| id_record == 119| 
           id_record == 148| id_record == 012| id_record == 066| id_record == 067|
           id_record == 093| id_record == 098| id_record == 107| id_record == 121|
           id_record == 127| id_record == 173| id_record == 196)

if (!file.exists('models/meta_mod_postex_warn_2.rds')) {
  
  meta_mod_postex_warn_2 <- rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ postexposure_warning,
    data   = postex_warn_studies,
    method = 'REML'
  )
  
  saveRDS(meta_mod_postex_warn_2,'models/meta_mod_postex_warn_2.rds')
  
} else {
  
  meta_mod_postex_warn_2 <- readRDS('models/meta_mod_postex_warn_2.rds')
  
}

summary(meta_mod_postex_warn_2)


# Publication year


if (!file.exists('models/meta_mod_year.rds')) {
  
  meta_mod_year <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ as.numeric(publication_year),
    data   = data_es,
    method = 'REML'
  )
  
  saveRDS(meta_mod_year,'models/meta_mod_year.rds')
  
} else {
  
  meta_mod_year <- readRDS('models/meta_mod_year.rds')
  
}


# Study modality

if (!file.exists('models/meta_mod_modality.rds')) {
  
  meta_mod_modality <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ as.factor(modality) - 1,
    data   = data_es,
    method = 'REML'
  )
  
  saveRDS(meta_mod_modality,'models/meta_mod_modality.rds')
  
} else {
  
  meta_mod_modality <- readRDS('models/meta_mod_modality.rds')
  
}


# Control item accuracy (memory performance)


if (!file.exists('models/meta_mod_control_acc.rds')) {
  
  meta_mod_control_acc <-  rma.mv(    
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ total_accuracy_control_mean,
    data   = data_es,
    method = 'REML'
  )
  
  saveRDS(meta_mod_control_acc,'models/meta_mod_control_acc.rds')
  
} else {
  
  meta_mod_control_acc <- readRDS('models/meta_mod_control_acc.rds')
  
}

summary(meta_mod_control_acc)



if (!file.exists('models/meta_mod_control_acc_outlier.rds')) {
  
  meta_mod_control_acc_outlier <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = total_accuracy_control_mean,
    data   = filter(data_es, yi < 5.3),
    method = 'REML'
  )
  
  saveRDS(meta_mod_control_acc_outlier,
          'models/meta_mod_control_acc_outlier.rds')
  
} else {
  
  meta_mod_control_acc_outlier <- readRDS('models/meta_mod_control_acc_outlier.rds')
  
}

summary(meta_mod_control_acc_outlier)

if (!file.exists('models/meta_mod_control_acc_quad.rds')) {
  
  meta_mod_control_acc_quad <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ I(total_accuracy_control_mean^2),
    data   = data_es,
    method = 'REML'
  )
  
  saveRDS(meta_mod_control_acc_quad,'models/meta_mod_control_acc_quad.rds')
  
} else {
  
  meta_mod_control_acc_quad <- readRDS('models/meta_mod_control_acc_quad.rds')
  
}

summary(meta_mod_control_acc_quad)

if (!file.exists('models/meta_mod_control_acc_full.rds')) {
  
  meta_mod_control_acc_full <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ total_accuracy_control_mean + I(total_accuracy_control_mean^2),
    data   = data_es,
    method = 'REML'
  )
  
  saveRDS(meta_mod_control_acc_full,'models/meta_mod_control_acc_full.rds')
  
} else {
  
  meta_mod_control_acc_full <- readRDS('models/meta_mod_control_acc_full.rds')
  
}
summary(meta_mod_control_acc_full)

if (!file.exists('models/meta_mod_control_acc_full_2.rds')) {
  
  meta_mod_control_acc_full_2 <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ total_accuracy_control_mean +
      I(total_accuracy_control_mean^2),
    data   = filter(data_es, yi < 5.3),
    method = 'REML'
  )
  
  saveRDS(meta_mod_control_acc_full_2,'models/meta_mod_control_acc_full_2.rds')
  
} else {
  
  meta_mod_control_acc_full_2 <- readRDS('models/meta_mod_control_acc_full_2.rds')
  
}
summary(meta_mod_control_acc_full_2)

#Control item accuracy proportion (memory performance)

if (!file.exists('models/meta_mod_control_prop.rds')) {
  
  meta_mod_control_prop <-  rma.mv(    
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ total_accuracy_control_prop,
    data   = data_es,
    method = 'REML'
  )
  
  saveRDS(meta_mod_control_prop,'models/meta_mod_control_prop.rds')
  
} else {
  
  meta_mod_control_prop <- readRDS('models/meta_mod_control_prop.rds')
  
}

summary(meta_mod_control_prop)

if (!file.exists('models/meta_mod_control_prop_quad.rds')) {
  
  meta_mod_control_prop_quad <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ I(total_accuracy_control_prop^2),
    data   = data_es,
    method = 'REML'
  )
  
  saveRDS(meta_mod_control_prop_quad,'models/meta_mod_control_prop_quad.rds')
  
} else {
  
  meta_mod_control_prop_quad <- readRDS('models/meta_mod_control_prop_quad.rds')
  
}

summary(meta_mod_control_prop_quad)

if (!file.exists('models/meta_mod_control_prop_full.rds')) {
  
  meta_mod_control_prop_full <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ total_accuracy_control_prop +
      I(total_accuracy_control_prop^2),
    data   = data_es,
    method = 'REML'
  )
  
  saveRDS(meta_mod_control_prop_full,'models/meta_mod_control_prop_full.rds')
  
} else {
  
  meta_mod_control_prop_full <- readRDS('models/meta_mod_control_prop_full.rds')
  
}
summary(meta_mod_control_prop_full)

# Post-event warnings



if (!file.exists('models/meta_mod_postev_warn.rds')) {
  
  meta_mod_postev_warn <- rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ postevent_warning,
    data   = data_es,
    method = 'REML'
  )
  
  saveRDS(meta_mod_postev_warn,'models/meta_mod_postev_warn.rds')
  
} else {
  
  meta_mod_postev_warn <- readRDS('models/meta_mod_postev_warn.rds')
  
}
summary(meta_mod_postev_warn)

# Limiting scope to only warning studies

postev_warn_studies <- data_es %>% 
  filter(postevent_warning > 0) %>% 
  select(id_record)
postev_warn_studies <- as.vector(postev_warn_studies$id_record)
postev_warn_studies <- unique(postev_warn_studies)
noquote(paste(postev_warn_studies, collapse = '| id_record == '))
postev_warn_studies <- data_es %>% 
  filter(id_record == 074| id_record == 172| id_record == 069| id_record == 157|
           id_record == 160| id_record == 264| id_record == 232)


if (!file.exists('models/meta_mod_postev_warn_2.rds')) {
  
  meta_mod_postev_warn_2 <- rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ postevent_warning,
    data   = postev_warn_studies,
    method = 'REML'
  )
  
  saveRDS(meta_mod_postev_warn_2,'models/meta_mod_postev_warn_2.rds')
  
} else {
  
  meta_mod_postev_warn_2 <- readRDS('models/meta_mod_postev_warn_2.rds')
  
}

summary(meta_mod_postev_warn_2)


