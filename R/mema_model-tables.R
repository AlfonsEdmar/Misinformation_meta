################################################################################

# Misinformation Meta-analysis - Tables for Models

################################################################################

source("R/mema_reporting-functions.R")

# Create report tables ---------------------------------------------------------

# Primary results

report_table_primary     <- extract_table_data(meta_primary) %>% 
  standard_table()

# PET-PEESE

report_table_pet         <- extract_table_data(meta_pet) %>% 
  standard_table()

report_table_peese       <- extract_table_data(meta_peese) %>% 
  standard_table()

# Robustness checks

report_table_leverage    <- extract_table_data(meta_leverage) %>% 
  standard_table()

report_table_pet_lev     <- extract_table_data(meta_pet_leverage) %>% 
  standard_table()

report_table_peese_lev   <- extract_table_data(meta_peese_leverage) %>% 
  standard_table()

report_table_resid       <- extract_table_data(meta_resid) %>% 
  standard_table()

report_table_pet_res     <- extract_table_data(meta_pet_resid) %>% 
  standard_table()

report_table_peese_res   <- extract_table_data(meta_peese_resid) %>% 
  standard_table()

# Subgroup analyses

## Age

report_table_age         <- extract_table_data(meta_age) %>% 
  standard_table()

report_table_age_quad    <- extract_table_data(meta_age_quad) %>% 
  standard_table()

report_table_age_no_acc  <- extract_table_data(meta_age_no_acc) %>% 
  standard_table()

report_table_age_cat     <- extract_table_data(meta_age_cat) %>% 
  standard_table()

## Proportion Female

report_table_female      <- extract_table_data(meta_prop_female) %>% 
  standard_table()

## Incentives

report_table_incent      <- extract_table_data(meta_incent) %>% 
  standard_table()

## Centrality

report_table_cent        <- extract_table_data(meta_centrality) %>% 
  standard_table()

report_table_cent_no_acc <- extract_table_data(meta_centrality_no_acc) %>% 
  standard_table()

## RES

report_table_res         <- extract_table_data(meta_res) %>% 
  standard_table()

report_table_res_01      <- extract_table_data(meta_res_01) %>% 
  standard_table()

report_table_res_no_acc  <- extract_table_data(meta_res_no_acc) %>% 
  standard_table()

# Export tables ----------------------------------------------------------------

# Create folder for tables

if (!dir.exists("output/tables")) {
  
  dir.create("output/tables")
  
}

# Save tables

## Primary results

save_as_docx("Primary" = report_table_primary,
             path      = "output/tables/mema_table_primary.docx")

## PET-PEESE

save_as_docx("PET"     = report_table_pet,
             "PEESE"   = report_table_peese,
             path      = "output/tables/mema_table_pet-peese.docx")

## Robustness checks

save_as_docx("Leverage"         = report_table_leverage,
             "Leverage: PET"    = report_table_pet_lev,
             "Leverage: PEESE"  = report_table_peese_lev,
             "Residuals"        = report_table_resid,
             "Residuals: PET"   = report_table_pet_res,
             "Residuals: PEESE" = report_table_peese_res,
             path        = "output/tables/mema_table_robust.docx")

## Subgroup analyses

### Age

save_as_docx("Age"              = report_table_age,
             "Age: Quadratic"   = report_table_age_quad,
             "Age: No accuracy" = report_table_age_no_acc,
             "Age: Categorical" = report_table_age_cat,
             path        = "output/tables/mema_table_age.docx")

### Proportion Female

save_as_docx("Proportion Female" = report_table_female,
             path         = "output/tables/mema_tables_female.docx")

### Incentives

save_as_docx("Incentives" = report_table_incent,
             path         = "output/tables/mema_tables_incent.docx")

### Centrality

save_as_docx("Centrality"              = report_table_cent,
             "Centrality: No accuracy" = report_table_cent_no_acc,
             path        = "output/tables/mema_table_centrality.docx")

### RES

save_as_docx("RES"              = report_table_res,
             "RES: 0 vs. 1 Test"= report_table_res_01,
             "RES: No accuracy" = report_table_res_no_acc,
             path        = "output/tables/mema_table_res.docx")

