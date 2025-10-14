################################################################################

# Misinformation Meta-analysis - PET-PEESE Supplement

################################################################################

# PET --------------------------------------------------------------------------

## Reduced random effects structure

if (!file.exists("output/mema_pet_re.rds")) {
  
  meta_pet_re       <- rma.mv(yi     = yi, 
                              V      = vi,
                              random = list(~1|id_record/id_study/id_control),
                              mods   = ~ postevent_retention_interval
                              + postexposure_retention_interval
                              + preevent_warning
                              + postevent_warning
                              + postexposure_warning
                              + control_acc
                              + I(control_acc^2)
                              + postevent_recall
                              + postexposure_recall
                              + publication_year
                              + preregistered
                              + I(sqrt(vi)),
                              data   = data_included,
                              method = "REML",
                              control = list(
                                iter.max    = 1000,
                                rel.tol     = 1e-8
                              ),
                              verbose = TRUE)
  
  saveRDS(meta_pet_re, "output/mema_pet_re.rds")
  
} else {
  
  meta_pet_re <- readRDS("output/mema_pet_re.rds")
  
}

## Reduced random effects structure, no moderators

if (!file.exists("output/mema_pet_rem.rds")) {
  
  meta_pet_rem       <- rma.mv(yi     = yi, 
                              V       = vi,
                              random  = list(~1|id_record/id_study/id_control),
                              mods    = ~ I(sqrt(vi)),
                              data    = data_included,
                              method  = "REML",
                              control = list(
                                iter.max    = 1000,
                                rel.tol     = 1e-8
                              ),
                              verbose = TRUE)
  
  saveRDS(meta_pet_rem, "output/mema_pet_rem.rds")
  
} else {
  
  meta_pet_rem <- readRDS("output/mema_pet_rem.rds")
  
}

## Reduced random effects structure, no moderators, random slopes

if (!file.exists("output/mema_pet_rs.rds")) {
  
  meta_pet_rs       <- rma.mv(yi     = yi, 
                              V       = vi,
                              random  = list(~1 |id_record/id_study/id_control,
                                             ~1 + I(sqrt(vi))|id_record),
                              mods    = ~ I(sqrt(vi)),
                              data    = data_included,
                              method  = "REML",
                              control = list(
                                iter.max    = 1000,
                                rel.tol     = 1e-8
                              ),
                              verbose = TRUE)
  
  saveRDS(meta_pet_rs, "output/mema_pet_rs.rds")
  
} else {
  
  meta_pet_rs <- readRDS("output/mema_pet_rs.rds")
  
}

## No random effects and no moderators

if (!file.exists("output/mema_pet_rma.rds")) {
  
  meta_pet_rma       <- rma.mv(yi      = yi, 
                               V       = vi,
                               random  = list(~1|id_effect),
                               mods    = ~  I(sqrt(vi)),
                               data    = data_included,
                               method  = "REML",
                               control = list(
                                 iter.max    = 1000,
                                 rel.tol     = 1e-8
                               ),
                               verbose = TRUE)
  
  saveRDS(meta_pet_rma, "output/mema_pet_rma.rds")
  
} else {
  
  meta_pet_rma <- readRDS("output/mema_pet_rma.rds")
  
}

# PEESE ------------------------------------------------------------------------

## Reduced random effects structure

if (!file.exists("output/mema_peese_re.rds")) {
  
  meta_peese_re     <- rma.mv(yi     = yi, 
                              V      = vi,
                              random = list(~1|id_record/id_study/id_control),
                              mods   = ~ postevent_retention_interval
                              + postexposure_retention_interval
                              + preevent_warning
                              + postevent_warning
                              + postexposure_warning
                              + control_acc
                              + I(control_acc^2)
                              + postevent_recall
                              + postexposure_recall
                              + publication_year
                              + preregistered
                              + I(vi),
                              data   = data_included,
                              method = "REML",
                              control = list(
                                iter.max    = 1000,
                                rel.tol     = 1e-8
                              ),
                              verbose = TRUE)
  
  saveRDS(meta_peese_re, "output/mema_peese_re.rds")
  
} else {
  
  meta_peese_re <- readRDS("output/mema_peese_re.rds")
  
}

## Reduced random effects structure, no moderators

if (!file.exists("output/mema_peese_rem.rds")) {
  
  meta_peese_rem     <- rma.mv(yi      = yi, 
                               V       = vi,
                               random  = list(~1|id_record/id_study/id_control),
                               mods    = ~ I(vi),
                               data    = data_included,
                               method  = "REML",
                               control = list(
                                 iter.max    = 1000,
                                 rel.tol     = 1e-8
                               ),
                               verbose = TRUE)
  
  saveRDS(meta_peese_rem, "output/mema_peese_rem.rds")
  
} else {
  
  meta_peese_rem <- readRDS("output/mema_peese_rem.rds")
  
}

## Reduced random effects structure, no moderators, random slopes

if (!file.exists("output/mema_peese_rs.rds")) {
  
  meta_peese_rs       <- rma.mv(yi     = yi, 
                              V       = vi,
                              random  = list(~1|id_record/id_study/id_control,
                                             ~1 + I(vi)|id_record),
                              mods    = ~ I(vi),
                              data    = data_included,
                              method  = "REML",
                              control = list(
                                iter.max    = 1000,
                                rel.tol     = 1e-8
                              ),
                              verbose = TRUE)
  
  saveRDS(meta_peese_rs, "output/mema_peese_rs.rds")
  
} else {
  
  meta_peese_rs <- readRDS("output/mema_peese_rs.rds")
  
}

## No random effects and no moderators

if (!file.exists("output/mema_peese_rma.rds")) {
  
  meta_peese_rma     <- rma.mv(yi      = yi, 
                               V       = vi,
                               random  = list(~1|id_effect),
                               mods    = ~ I(vi),
                               data    = data_included,
                               method  = "REML",
                               control = list(
                                 iter.max    = 1000,
                                 rel.tol     = 1e-8
                               ),
                               verbose = TRUE)
  
  saveRDS(meta_peese_rma, "output/mema_peese_rma.rds")
  
} else {
  
  meta_peese_rma <- readRDS("output/mema_peese_rma.rds")
  
}
