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

report_table_simple      <- extract_table_data(meta_simple) %>% 
  standard_table()

report_table_acc_sq      <- extract_table_data(meta_accsq) %>% 
  standard_table()

report_table_imp         <- extract_table_data(meta_primary_imp) %>% 
  standard_table()

report_table_leverage    <- extract_table_data(meta_leverage) %>% 
  standard_table()

report_table_resid       <- extract_table_data(meta_resid) %>% 
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

## Misinformation type

report_table_type        <- extract_table_data(meta_type) %>% 
  standard_table()

# PET-PEESE supplemental

report_pet_re            <- extract_table_data(meta_pet_re) %>% 
  standard_table()

report_pet_rem           <- extract_table_data(meta_pet_rem) %>% 
  standard_table()

report_pet_rs            <- extract_table_data(meta_pet_rs) %>% 
  standard_table()

report_pet_rma           <- extract_table_data(meta_pet_rma) %>% 
  standard_table()

report_peese_re          <- extract_table_data(meta_peese_re) %>% 
  standard_table()

report_peese_rem         <- extract_table_data(meta_peese_rem) %>% 
  standard_table()

report_peese_rs          <- extract_table_data(meta_peese_rs) %>% 
  standard_table()

report_peese_rma         <- extract_table_data(meta_peese_rma) %>% 
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

save_as_docx("Leverage"             = report_table_leverage,
             "Residuals"            = report_table_resid,
             "Imputations"          = report_table_imp,
             "Simple"               = report_table_simple,
             "Accuracy - Quadratic" = report_table_acc_sq,
             path        = "output/tables/mema_table_robust.docx")

## PET-PEESE supplement

save_as_docx("PET - Reduced random effects"                  = report_pet_re,
             "PET - Reduced random effects, no moderators"   = report_pet_rem,  
             "PET - Random slopes"                           = report_pet_rs,  
             "PET - No random effects, no moderators"        = report_pet_rma,  
             "PEESE - Reduced random effects"                = report_peese_re,  
             "PEESE - Reduced random effects, no moderators" = report_peese_rem,
             "PEESE - Random slopes"                         = report_peese_rs, 
             "PEESE - No random effects, no moderators"      = report_peese_rma,
             path = "output/tables/mema_table_pet-peese_supplement.docx")

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

### Misinformation Type

save_as_docx("Type as Random"   = report_table_type,
             path        = "output/tables/mema_table_type.docx")
