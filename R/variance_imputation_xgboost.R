################################################################################

# Variance imputation using xgboost

################################################################################


# Packages ---------------------------------------------------------------------
library(caret)
library(xgboost)
library(tidyverse)

# Load data --------------------------------------------------------------------
data <- read_csv("data/misinformation_data_cleaned.csv")


# Creating datasets for missing and present variances --------------------------

test_data <- data %>%
  filter(is.na(total_accuracy_control_sd) & is.na(total_accuracy_control_prop) 
         & !is.na(total_accuracy_control_mean))

train_data <- data %>% 
  filter(!is.na(total_accuracy_mi_sd))


# Specifying the sampling procedure for the training data-----------------------

xgb_tuning <- expand.grid( nrounds = 500
                         , max_depth = 3:6
                         , eta = c(.001, .01)
                         , gamma = c(0, .1, 1)
                         , colsample_bytree = c(.5, .8, 1)
                         , min_child_weight = c(1, 3, 5)
                         , subsample = .7)

train_sampling <- trainControl(  method = 'boot'
                               , number = 2)


# Predicting misled accuracy sd using constrained xgboost-----------------------

set.seed(676)
xg_fit_mi_sd <- train(  total_accuracy_control_sd ~ total_accuracy_control_mean+ 
                        total_accuracy_mi_mean + n_control + n_mi + items_misled  
                      , data        = train_data
                      , method      = 'xgbTree'
                      , trControl   = train_sampling
                      , verbosity   = 0
                      , na.action   = na.pass
                      , tuneGrid    = xgb_tuning)

# Predicting missing misled accuracy sd-----------------------------------------

xg_mi_sd_pred <- predict(xg_fit_mi_sd, test_data)
summary(xg_mi_sd_pred)

# Predicting control accuracy sd using xgboost-----------------------------------

set.seed(6820)
xg_fit_co_sd <- train( total_accuracy_control_sd ~ total_accuracy_control_mean + 
                       total_accuracy_mi_mean + n_control + n_mi + items_misled 
                     , data      = train_data
                     , method    = 'xgbTree'
                     , trControl = train_sampling
                     , verbosity = 0
                     , na.action = na.pass
                     , tuneGrid  = xgb_tuning)

# Predicting missing control accuracy sd----------------------------------------

xg_co_sd_pred <- predict(xg_fit_co_sd, test_data)

# Summary of imputed standard deviations----------------------------------------
cbind(c(summary(predict(xg_fit_mi_sd)))
      ,summary(predict(xg_fit_co_sd)))

# Summary of observed standard deviations---------------------------------------
cbind(c(summary(train_data$total_accuracy_mi_sd))
      ,summary(train_data$total_accuracy_control_sd))

# Density plots for imputed and observed standard deviations--------------------

ggplot()+
  geom_density(aes(xg_co_sd_pred), col = 'blue')+
  geom_density(aes(train_data$accuracy_control_sd), col = 'red')+
  xlim(0,1)+
  theme_bw()

ggplot()+
  geom_density(aes(xg_mi_sd_pred), col = 'blue')+
  geom_density(aes(train_data$accuracy_mi_sd), col = 'red')+
  xlim(0,1)+
  theme_bw()

# Exporting imputations---------------------------------------------------------
imputed_variances <- data.frame(xg_co_sd_pred, xg_mi_sd_pred)
write.csv(imputed_variances, 'data/imputed_variances.csv')
