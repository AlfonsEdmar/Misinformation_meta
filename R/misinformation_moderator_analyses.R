################################################################################

# Misinformation Meta-analysis - Moderator analyses

################################################################################


# Packages ---------------------------------------------------------------------

library(tidyverse)
library(metafor)

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

# Moderator analysis -----------------------------------------------------------

## Test type--------------------------------------------------------------------

if (!file.exists('models/meta_mod_test_type.rds')) {
  
  meta_mod_test_type <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ as.factor(test_type) - 1,
    data   = data_es,
    method   = 'REML'
  )
  
  
  
  saveRDS(meta_mod_test_type,'models/meta_mod_test_type.rds')
  
} else {
  
  meta_mod_test_type <- readRDS('models/meta_mod_test_type.rds')
  
}
summary(meta_mod_test_type) 
I2_mv(meta_mod_test_type, data_es)

## Fitting again including the intercept
if (!file.exists('models/meta_mod_test_type_2.rds')) {
  
  meta_mod_test_type_2 <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ as.factor(test_type),
    data   = data_es,
    method   = 'REML'
  )
  
  
  
  saveRDS(meta_mod_test_type_2,'models/meta_mod_test_type_2.rds')
  
} else {
  
  meta_mod_test_type_2 <- readRDS('models/meta_mod_test_type_2.rds')
  
}
summary(meta_mod_test_type_2)

## Number of post-event tests---------------------------------------------------

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
summary(meta_mod_postev_test)

# Looking only at studies employing an RES design. 
res_studies <- data_es %>% filter(postevent_recall >0)
res_id <- unique(res_studies$id_record)
res_studies<- data
cat(paste(shQuote(res_id, type="cmd"), collapse="| id_record =="))

res_studies<- data %>% 
  filter(  id_record =="006"| id_record =="011"| id_record =="022"|
             id_record =="026"| id_record =="068"| id_record =="085"|
             id_record =="119"| id_record =="137"| id_record =="021"| 
             id_record =="023"| id_record =="034"| id_record =="035"| 
             id_record =="036"| id_record =="037"| id_record =="042"| 
             id_record =="053"| id_record =="054"| id_record =="055"| 
             id_record =="056"| id_record =="057"| id_record =="069"| 
             id_record =="091"| id_record =="096"| id_record =="102"| 
             id_record =="118"| id_record =="121"| id_record =="123"|
             id_record =="131"| id_record =="132"| id_record =="133"| 
             id_record =="136"| id_record =="144"| id_record =="151"| 
             id_record =="174"| id_record =="181"| id_record =="190"| 
             id_record =="195"| id_record =="203"| id_record =="204"| 
             id_record =="205"| id_record =="206"| id_record =="207"| 
             id_record =="208"| id_record =="209"| id_record =="212"|
             id_record =="213"| id_record =="218"| id_record =="223"| 
             id_record =="224"| id_record =="238"| id_record =="241"|
             id_record =="247"| id_record =="253"| id_record =="262"| 
             id_record =="263"| id_record =="264"| id_record =="261")

if (!file.exists('models/meta_mod_postev_test_2.rds')) {
  
  meta_mod_postev_test_2 <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ postevent_recall,
    data   = res_studies,
    method = 'REML'
  )
  
  saveRDS(meta_mod_postev_test_2,'models/meta_mod_postev_test_2.rds')
  
} else {
  
  meta_mod_postev_test_2 <- readRDS('models/meta_mod_postev_test_2.rds')
  
}

summary(meta_mod_postev_test_2)

## Age--------------------------------------------------------------------------
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

# Replicating Ceci and Bruck(1993)
data_es <- read_csv2('data/misinformation_data_es.csv')

## Define age groups
age_breaks <- c(0, 5, 17, 40, Inf)

age_factor <- cut(data_es$age_mean, breaks = age_breaks, 
                  labels = c("< 5",
                             "< 17",
                             "< 40",
                             "> 40"))


if (!file.exists('models/meta_mod_age_cat.rds')) {
  
  meta_mod_age_cat <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ age_factor,
    data   = filter(data_es),
    method = 'REML'
  )
  
  saveRDS(meta_mod_age_cat,'models/meta_mod_age_cat.rds')
  
} else {
  
  meta_mod_age_cat <- readRDS('models/meta_mod_age_cat.rds')
  
}
summary(meta_mod_age_cat)

data_es %>% 
  ggplot(aes(y = yi, x = age_factor))+
  geom_boxplot(width = .1, outlier.shape = NA)+
  ggdist::stat_dots(justification = -.1, side = 'right')+
  scale_y_continuous(limits = c(-3, 5), breaks = seq(-3,5,1),
                     name = 'Hedges G')+
  xlab(label = 'Age Categories')+
  theme_classic()+
  coord_flip()

# Replicating Wylie et al (2014)

if (!file.exists('models/meta_mod_elders.rds')) {
  
  meta_mod_age_elders <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ age_mean,
    data   = filter(data_es, age_mean > 40),
    method = 'REML'
  )
  
  saveRDS(meta_mod_age_elders,'models/meta_mod_elders.rds')
  
} else {
  
  meta_mod_elders <- readRDS('models/meta_mod_elders.rds')
  
}
summary(meta_mod_elders)

if (!file.exists('models/meta_mod_adults.rds')) {
  
  meta_mod_adults <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ age_mean,
    data   = filter(data_es, age_mean < 40 & age_mean > 17),
    method = 'REML'
  )
  
  saveRDS(meta_mod_adults,'models/meta_mod_adults.rds')
  
} else {
  
  meta_mod_adults <- readRDS('models/meta_mod_adults.rds')
  
}
summary(meta_mod_adults)

## Post-event retention interval------------------------------------------------

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

# Post-exposure retention interval----------------------------------------------

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


# Quadradic transformation

if (!file.exists('models/meta_mod_postex_ret_quad.rds')) {
  
  meta_mod_postex_ret_quad <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ I((postexposure_retention_interval/24)^2),
    data   = data_es,
    method = 'REML'
  )
  
  saveRDS(meta_mod_postex_ret_quad,'models/meta_mod_postex_ret_quad.rds')
  
} else {
  
  meta_mod_postex_ret_quad <- readRDS('models/meta_mod_postex_ret_quad.rds')
  
}

summary(meta_mod_postex_ret_quad)


# Comparing quadradic and linear
meta_mod_postex_ret_quad_2 <-  rma.mv(
  yi     = yi, 
  V      = vi,
  random = list(~1|id_record/id_study/id_control, 
                ~1|event_materials),
  mods   = ~ I((postexposure_retention_interval/24)^2) + 
    I( postexposure_retention_interval/24),
  data   = data_es,
  method = 'REML'
)

summary(meta_mod_postex_ret_quad_2)

# Constraining the interval

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

# Gender (proportion female)----------------------------------------------------

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

# Post-event warnings-----------------------------------------------------------

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

# looking only at studies utilising post event warnings

postwarn_studies <- data_es %>% filter(postevent_warning >0)
postwarn_id <- unique(postwarn_studies$id_record)
cat(paste(shQuote(postwarn_id, type="cmd"), collapse="| id_record =="))

res_studies<- data %>% 
  filter(id_record =="264"| id_record =="069")

if (!file.exists('models/meta_mod_postev_warn_2.rds')) {
  
  meta_mod_postev_warn_2 <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ postevent_warning,
    data   = res_studies,
    method = 'REML'
  )
  
  saveRDS(meta_mod_postev_warn_2,'models/meta_mod_postev_warn_2.rds')
  
} else {
  
  meta_mod_postev_warn_2 <- readRDS('models/meta_mod_postev_warn_2.rds')
  
}

summary(meta_mod_postev_warn_2)

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


# Publication year--------------------------------------------------------------


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

summary(meta_mod_year)

# Study modality----------------------------------------------------------------

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


# Control item accuracy (memory performance)------------------------------------


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
    data   = filter(data_es, yi < 5),
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
    data   = filter(data_es, yi < 5),
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


# Comparing control type--------------------------------------------------------

if (!file.exists('models/meta_mod_co_type.rds')) {
  
  meta_mod_control_type <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ control_type,
    data   = data_es,
    method = 'REML'
  )
  
  saveRDS(meta_mod_control_type,'models/meta_mod_co_type_type.rds')
  
} else {
  
  meta_mod_control_type <- readRDS('models/meta_mod_co_type.rds')
  
}
summary(meta_mod_control_type)

# removing intercept

if (!file.exists('models/meta_mod_co_type_2.rds')) {
  
  meta_mod_control_type_2 <-  rma.mv(
    yi     = yi, 
    V      = vi,
    random = list(~1|id_record/id_study/id_control, 
                  ~1|event_materials),
    mods   = ~ as.factor(control_type) -1,
    data   = data_es,
    method = 'REML'
  )
  
  saveRDS(meta_mod_control_type_2,'models/meta_mod_co_type_2.rds')
  
} else {
  
  meta_mod_control_type_2 <- readRDS('models/meta_mod_co_type_2.rds')
  
}

summary(meta_mod_control_type_2)



