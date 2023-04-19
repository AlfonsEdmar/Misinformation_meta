################################################################################

# Variance imputation using xgboost

################################################################################


# Packages ---------------------------------------------------------------------
library(caret)
library(xgboost)
library(tidyverse)

# Load data --------------------------------------------------------------------
data <- read_csv("data/misinformation_data_cleaned.csv")


# Creating data sets for missing and present variances -------------------------

test_data <- data %>%
  filter(  is.na(total_accuracy_control_sd) 
         & is.na(total_accuracy_control_prop) 
         & !is.na(total_accuracy_control_mean)
         & !is.na(total_accuracy_control_mean))

train_data <- data %>% 
  filter(!is.na(total_accuracy_mi_sd) & !is.na(total_accuracy_control_sd))

summary(train_data$total_accuracy_mi_sd)
summary(train_data$total_accuracy_control_sd)

prop_data <- data %>%
  filter(is.na(total_accuracy_control_mean))

cbind(  summary(train_data$total_accuracy_control_mean)
      , summary(train_data$total_accuracy_mi_mean))

cbind(  summary(test_data$total_accuracy_control_mean)
        , summary(test_data$total_accuracy_mi_mean))

#NOTE: Since we have a different number of missing obs across mi_sd and co_sd we 
#      will need to impute zeros afterthe imputation

# Specifying the sampling procedure for the training data-----------------------

xgb_tuning <- expand.grid( nrounds          = 500
                         , max_depth        = 1:3
                         , eta              = c(.01, .1, 1)
                         , gamma            = c(0, .01, .1)
                         , colsample_bytree = c(.5, .8, 1)
                         , min_child_weight = c(1, 2, 3)
                         , subsample        = .9)

train_sampling <- trainControl(  method     = 'boot'
                               , number     = 2)

# Predicting misled accuracy sd using constrained xgboost-----------------------

set.seed(676)
xg_fit_mi_sd <- train(  total_accuracy_control_sd ~ total_accuracy_control_mean+ 
                        total_accuracy_mi_mean + n_control + n_mi    
                      , data        = train_data
                      , method      = 'xgbTree'
                      , trControl   = train_sampling
                      , verbosity   = 0
                      , na.action   = na.pass
                      , tuneGrid    = xgb_tuning)

# Predicting missing misled accuracy sd-----------------------------------------

xg_mi_sd_pred <- predict(xg_fit_mi_sd, newdata = test_data)
summary(xg_mi_sd_pred)

# Predicting control accuracy sd using xgboost-----------------------------------

set.seed(6820)
xg_fit_co_sd <- train( total_accuracy_control_sd ~ total_accuracy_control_mean + 
                       total_accuracy_mi_mean + n_control + n_mi 
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
imputed_variances$xg_co_sd_pred[135] <- 0
imputed_variances$xg_co_sd_pred[139] <- 0
write.csv(imputed_variances, 'data/imputed_variances.csv')

# Creating a complete data set--------------------------------------------------

test_data$total_accuracy_control_sd <- imputed_variances$xg_co_sd_pred
test_data$total_accuracy_mi_sd      <- imputed_variances$xg_mi_sd_pred

complete_data <- rbind(test_data, train_data, prop_data)

write.csv(complete_data, 'data/complete_data_cleaned.csv')

