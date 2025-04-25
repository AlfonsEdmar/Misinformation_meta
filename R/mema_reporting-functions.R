################################################################################

# Misinformation Meta-analysis - Reporting functions

################################################################################

# Set up data frame for tabling

extract_table_data <- function(meta, dig = 3, dig_ret = 5) {
  
  # Names
  
  fixed_names <- rownames(meta$beta)
  ranef_names <- meta$s.names
  
  # Fixed effects
  
  beta <- meta$beta
  cilb <- meta$ci.lb
  ciub <- meta$ci.ub
  
  # Rounding rules
  
  digits <- rep(dig, length(beta))
  digits[which(str_detect(rownames(beta), "retention"))] <- dig_ret
  
  ## Format coefficients and CIs
  
  fixed_parms <- paste(
    strtrim(format(round(beta, dig_ret), nsmall = dig_ret), 3 + digits),
    " [",
    strtrim(format(round(cilb, dig_ret), nsmall = dig_ret), 3 + digits),
    ", ",
    strtrim(format(round(ciub, dig_ret), nsmall = dig_ret), 3 + digits),
    "]",
    sep = ""
  )
  
  fixed_df <- data.frame(
    Effect    = "Fixed",
    Parameter = fixed_names,
    Estimate  = fixed_parms
  )
  
  # Random effects
  
  sigma2 <- meta$sigma2
  sigma  <- round(sqrt(sigma2), dig)
  
  ranef_parms <- paste(
    format(sigma, nsmall = dig),
    sep = ""
  )
  
  ranef_df <- data.frame(
    Effect    = "Random",
    Parameter = ranef_names,
    Sigma     = ranef_parms
  )
  
  # Rename parameters
  
  ## Fixed
  
  fixed_df$Parameter <- str_replace(
    fixed_df$Parameter,
    "intrcpt",
    "Intercept (Average Effect)")
  
  fixed_df$Parameter <- str_replace(
    fixed_df$Parameter,
    "postevent_retention_interval",
    "Post-Event Retention")
  
  fixed_df$Parameter <- str_replace(
    fixed_df$Parameter,
    "postexposure_retention_interval",
    "Post-Exposure Retention")
  
  fixed_df$Parameter <- str_replace(
    fixed_df$Parameter,
    "preevent_warning",
    "Pre-Event Warning")
  
  fixed_df$Parameter <- str_replace(
    fixed_df$Parameter,
    "postevent_warning",
    "Post-Event Warning")
  
  fixed_df$Parameter <- str_replace(
    fixed_df$Parameter,
    "postexposure_warning",
    "Post-Exposure Warning")
  
  fixed_df$Parameter <- str_replace(
    fixed_df$Parameter,
    "control_acc",
    "Control Accuracy")
  
  fixed_df$Parameter <- str_replace(
    fixed_df$Parameter,
    "postevent_recall",
    "Post-Event Tests")
  
  fixed_df$Parameter <- str_replace(
    fixed_df$Parameter,
    "postexposure_recall",
    "Post-Exposure Tests")
  
  fixed_df$Parameter <- str_replace(
    fixed_df$Parameter,
    "publication_year",
    "Publication Year")
  
  fixed_df$Parameter <- str_replace(
    fixed_df$Parameter,
    "preregistered",
    "Preregistration")
  
  fixed_df$Parameter <- str_replace(
    fixed_df$Parameter,
    "age_mean",
    "Mean Age")
  
  fixed_df$Parameter <- str_replace(
    fixed_df$Parameter,
    "I(Mean Age^2)",
    "Mean Age Squared")
  
  fixed_df$Parameter <- str_replace(
    fixed_df$Parameter,
    "age_cat0-5",
    "Age 0 to 5")
  
  fixed_df$Parameter <- str_replace(
    fixed_df$Parameter,
    "age_cat6-17",
    "Age 6 to 17")
  
  fixed_df$Parameter <- str_replace(
    fixed_df$Parameter,
    "age_cat41+",
    "Age 41+")
  
  fixed_df$Parameter <- str_replace(
    fixed_df$Parameter,
    "incentives",
    "Incentives: ")
  
  fixed_df$Parameter <- str_replace(
    fixed_df$Parameter,
    "course_credit_or_money",
    "Course Credit or Money")
  
  fixed_df$Parameter <- str_replace(
    fixed_df$Parameter,
    "course_credit",
    "Course Credit")
  
  fixed_df$Parameter <- str_replace(
    fixed_df$Parameter,
    "goods",
    "Goods")
  
  fixed_df$Parameter <- str_replace(
    fixed_df$Parameter,
    "money",
    "Money")
  
  fixed_df$Parameter <- str_replace(
    fixed_df$Parameter,
    "none",
    "None")
  
  fixed_df$Parameter <- str_replace(
    fixed_df$Parameter,
    "Required",
    "Required")
  
  fixed_df$Parameter <- str_replace(
    fixed_df$Parameter,
    "item_centralityperipheral",
    "Item Type: Peripheral")
  
  fixed_df$Parameter <- str_replace(
    fixed_df$Parameter,
    "I(sqrt(vi))",
    "Standard Error")
  
  fixed_df$Parameter <- str_replace(
    fixed_df$Parameter,
    "I(vi)",
    "Standard Error (squared)")
  
  ## Random 
  
  ranef_df$Parameter <- str_replace(
    ranef_df$Parameter,
    "id_record",
    "Record")
  
  ranef_df$Parameter <- str_replace(
    ranef_df$Parameter,
    "Record/id_study",
    "Study")
  
  ranef_df$Parameter <- str_replace(
    ranef_df$Parameter,
    "Study/id_control",
    "Control Group")
  
  ranef_df$Parameter <- str_replace(
    ranef_df$Parameter,
    "event_materials",
    "Event Materials")
  
  ranef_df$Parameter <- str_replace(
    ranef_df$Parameter,
    "country",
    "Country")
  
  ranef_df$Parameter <- str_replace(
    ranef_df$Parameter,
    "control_type",
    "Control Type")
  
  ranef_df$Parameter <- str_replace(
    ranef_df$Parameter,
    "modality",
    "Modality")
  
  ranef_df$Parameter <- str_replace(
    ranef_df$Parameter,
    "population",
    "Population")
  
  ranef_df$Parameter <- str_replace(
    ranef_df$Parameter,
    "test_type",
    "Test Type")
  
  ranef_df$Parameter <- str_replace(
    ranef_df$Parameter,
    "test_medium",
    "Test Medium")
  
  ranef_df$Parameter <- str_replace(
    ranef_df$Parameter,
    "exposure_medium",
    "Exposure Medium")
  
  # Combine data
  
  table_data <- bind_rows(fixed_df, ranef_df)
  
}

# Table function

standard_table <- function(df) {
  
  require(flextable)
  
  as_grouped_data(df, groups = "Effect") %>% 
    as_flextable() %>% 
    bold(part = "header", bold = TRUE) %>% 
    bold(j = 1, i = ~!is.na(Effect), bold = TRUE) %>% 
    align(j = c(2, 3), align = "right") %>% 
    align(part = "header", j = c(2, 3), align = "right") %>% 
    autofit()
  
  
}

