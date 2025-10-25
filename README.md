# What Fifty Years of Research on the Misinformation Effect Can Tell Us: A Meta-Analytic Review

This repository contains the code and supporting materials for a meta-analysis
of the misinformation effect. Code and data to reproduce all analyses are
available here.

## Reproducing the analyses

The `data` folder contains the raw data file used during data extraction, a
codebook, a post-cleaning data file, and a subset of the cleaned data that only
features effect sizes that were included in the primary model reported in the
paper. Generally, the cleaned data will be most useful for those wanting to
reproduce the analyses or conduct further explorations.

To reproduce all analyses we recommend running the following scripts (found in
the `R` folder) in the following order:

- `mema_misinformation-primary-analysis.R`
- `mema_subgroup-analysis.R`
- `mema_pet-peese-supplement.R`
- `mema_model-tables.R`

By default, these scripts will load the saved model objects from the `output`
folder. This approach allows for inspection of the specific model estimations we
report. However, if you want to reproduce the model estimation process, we
recommend simply removing the contents of the output folder prior to running the
scripts. Note that the model fitting process can take several hours, depending
on the available computational resources.

## Open Science Framework Repository

The OSF repository for this project can be found here: https://osf.io/qxptz/

The OSF repository contains tables reporting the results of the supplemental
models. However, for the sake of space and readability, these tables do not
contain specific information about the random effects (e.g., specific random
intercepts). To access this information, we recommend reproducing the analyses
(either by loading the pre-estimated model objects or refitting them; see
above). The random effects are easily extracted using the `ranef()` function in
`metafor`.

## Preprint

A preprint for this project is available here:
https://osf.io/preprints/psyarxiv/m68kf
