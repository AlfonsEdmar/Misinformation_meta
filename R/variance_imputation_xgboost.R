################################################################################

# Variance imputation using xgboost

################################################################################


# Packages ---------------------------------------------------------------------
library(caret)
library(tidyverse)

# Load data --------------------------------------------------------------------
data <- read_csv("data/misinformation_data_cleaned.csv")


# Creating datasets for missing and present variances --------------------------

test_data <- data %>%
  filter(is.na(accuracy_control_sd) & is.na(accuracy_control_prop) 
         & !is.na(accuracy_control_mean))

train_data <- data %>% 
  filter(!is.na(accuracy_mi_sd))


# Specifying the sampling procedure for the training data-----------------------

train_sampling <- trainControl(  method = 'boot'
                                 , number = 10)

# Predicting misled accuracy sd using xgboost-----------------------------------

xg_fit_mi_sd <- train(  accuracy_control_sd ~ accuracy_control_mean + 
                        accuracy_mi_mean + n_control + n_mi + items_misled  
                      , data      = train_data
                      , method    = 'xgbTree'
                      , trControl = train_sampling
                      , verbosity = 0
                      , na.action = na.pass)

# Predicting missing misled accuracy sd-----------------------------------------

xg_mi_sd_pred <- predict(xg_fit_mi_sd, test_data)

# Predicting control accuracy sd using xgboost-----------------------------------

xg_fit_co_sd <- train( accuracy_control_sd ~ accuracy_control_mean + 
                       accuracy_mi_mean + n_control + n_mi + items_misled 
                     , data      = train_data
                     , method    = 'xgbTree'
                     , trControl = train_sampling
                     , verbosity = 0
                     , na.action = na.pass)

# Predicting missing control accuracy sd-----------------------------------------

xg_co_sd_pred <- predict(xg_fit_co_sd, test_data)

# imputed standard deviations 
print(cbind(xg_co_sd_pred, xg_mi_sd_pred))
