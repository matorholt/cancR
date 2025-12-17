# Package index

## Data management functions

Functions for typical data management tasks

- [`cpR()`](https://matorholt.github.io/cancR/reference/cpR.md) : Fix
  CPR numbers with removed leading zeros
- [`cutR()`](https://matorholt.github.io/cancR/reference/cutR.md) : Cut
  numeric variables into intervals
- [`datR()`](https://matorholt.github.io/cancR/reference/datR.md) :
  Convert dates from character to date format
- [`distributR()`](https://matorholt.github.io/cancR/reference/distributR.md)
  : Assessment of distribution of continuous variables with histograms,
  QQ-plots and the Shapiro-Wilks test
- [`factR()`](https://matorholt.github.io/cancR/reference/factR.md) :
  Factorize variables
- [`groupR()`](https://matorholt.github.io/cancR/reference/groupR.md) :
  Extract first n groups in data frame
- [`missR()`](https://matorholt.github.io/cancR/reference/missR.md) :
  Overview of NAs in a dataframe
- [`pvertR()`](https://matorholt.github.io/cancR/reference/pvertR.md) :
  Format p-values to AMA manual of style
- [`structR()`](https://matorholt.github.io/cancR/reference/structR.md)
  : Convert dates to status indicator and time-to-event
- [`readR()`](https://matorholt.github.io/cancR/reference/readR.md) :
  Load csv, excel and rds files
- [`recodR()`](https://matorholt.github.io/cancR/reference/recodR.md) :
  Recode multiple variables
- [`redcapR()`](https://matorholt.github.io/cancR/reference/redcapR.md)
  : Autoformatting of redcap exports
- [`rowR()`](https://matorholt.github.io/cancR/reference/rowR.md) :
  Perform rowwise operations
- [`summarisR()`](https://matorholt.github.io/cancR/reference/summarisR.md)
  : Graphical overview of an entire dataset

## Statistical Analyses

Functions that perform statistical analysis

- [`incidencR()`](https://matorholt.github.io/cancR/reference/incidencR.md)
  : Estimate absolute risks in a single group with competing risks
- [`estimatR()`](https://matorholt.github.io/cancR/reference/estimatR.md)
  : Absolute risk estimation of time-to-event data with competing risks
- [`iteratR()`](https://matorholt.github.io/cancR/reference/iteratR.md)
  : Perform multiple estimatR analyses
- [`extractR()`](https://matorholt.github.io/cancR/reference/extractR.md)
  : Extraction of key results from the estimatR function
- [`inferencR()`](https://matorholt.github.io/cancR/reference/inferencR.md)
  : Causal inference of time-to-event data
- [`clustR()`](https://matorholt.github.io/cancR/reference/clustR.md) :
  Causal inference on time-to-event data with clustering and competing
  risks
- [`followR()`](https://matorholt.github.io/cancR/reference/followR.md)
  : Calculate median follow-up time using the inverse Kaplan-Meier
  method
- [`ratR()`](https://matorholt.github.io/cancR/reference/ratR.md) :
  Directly standardized incidence rates using the WHO standard
  population

## Tables and figures

Functions for producing and formatting tables and figures

- [`tablR()`](https://matorholt.github.io/cancR/reference/tablR.md) :
  Create baseline table
- [`plotR()`](https://matorholt.github.io/cancR/reference/plotR.md) :
  Autoplot for estimatR, incidencR, inferencR and clustR
- [`collectR()`](https://matorholt.github.io/cancR/reference/collectR.md)
  : Collect multiple plots into one
- [`savR()`](https://matorholt.github.io/cancR/reference/savR.md) : Save
  plots and tables

## Register-specific functions

Functions customized for the danish registers

- [`cci_timR()`](https://matorholt.github.io/cancR/reference/cci_timR.md)
  : Generation of time-dependent CCI and comorbidities from the Charlson
  Comorbidity Index
- [`decodR()`](https://matorholt.github.io/cancR/reference/decodR.md) :
  Decoding of the main codelist for loading and searching in registries
- [`formatR()`](https://matorholt.github.io/cancR/reference/formatR.md)
  : Auto-formatting of a data frame with layout option for typical
  levels and labels
- [`includR()`](https://matorholt.github.io/cancR/reference/includR.md)
  : Inclusion criteria for registry-studies
- [`loadR()`](https://matorholt.github.io/cancR/reference/loadR.md) :
  Load registers
- [`matchR()`](https://matorholt.github.io/cancR/reference/matchR.md) :
  Perform exposure-density matching for multilevel data
- [`reportR()`](https://matorholt.github.io/cancR/reference/reportR.md)
  : Overview of matched and unmatched cases
- [`searchR()`](https://matorholt.github.io/cancR/reference/searchR.md)
  : Find covariates or outcomes from the registers
- [`simulatR()`](https://matorholt.github.io/cancR/reference/simulatR.md)
  : Simulate danish health registers
- [`updatR()`](https://matorholt.github.io/cancR/reference/updatR.md) :
  Update of Charlson Comorbidity Index or covariate values over time

## Built-in datasets

Datasets for testing functions

- [`analysis_df`](https://matorholt.github.io/cancR/reference/analysis_df.md)
  : analysis_df
- [`match_df`](https://matorholt.github.io/cancR/reference/match_df.md)
  : match_df
- [`redcap_df`](https://matorholt.github.io/cancR/reference/redcap_df.md)
  : redcap_df
- [`covariates_df`](https://matorholt.github.io/cancR/reference/covariates_df.md)
  : covariates_df
- [`population_denmark`](https://matorholt.github.io/cancR/reference/population_denmark.md)
  : population_denmark
- [`population_who`](https://matorholt.github.io/cancR/reference/population_who.md)
  : population_who

## Miscellaneous functions

Small utility functions

- [`checkR()`](https://matorholt.github.io/cancR/reference/checkR.md) :
  Detection of positivity violations (empty levels)
- [`closR()`](https://matorholt.github.io/cancR/reference/closR.md) :
  Pick the closest value from a range in vector.
- [`colR()`](https://matorholt.github.io/cancR/reference/colR.md) : Set
  default color palette
- [`combinR()`](https://matorholt.github.io/cancR/reference/combinR.md)
  : Generate all possible cominations/permutations
- [`formatR()`](https://matorholt.github.io/cancR/reference/formatR.md)
  : Auto-formatting of a data frame with layout option for typical
  levels and labels
- [`geom_stepribbon()`](https://matorholt.github.io/cancR/reference/geom_stepribbon.md)
  : Stepribbon geom for survival curves
- [`listR()`](https://matorholt.github.io/cancR/reference/listR.md) :
  Routine modifications of lists
- [`modeR()`](https://matorholt.github.io/cancR/reference/modeR.md) :
  Get the mode (most common value) of a vector.
- [`numbR()`](https://matorholt.github.io/cancR/reference/numbR.md) :
  Format numeric vectors
- [`powR()`](https://matorholt.github.io/cancR/reference/powR.md) :
  Converter from Rscript to powershell format
- [`randomizR()`](https://matorholt.github.io/cancR/reference/randomizR.md)
  : Generate stratified, block randomized allocation sequence
- [`rollR()`](https://matorholt.github.io/cancR/reference/rollR.md) :
  Assign rolling ID.
- [`tickR()`](https://matorholt.github.io/cancR/reference/tickR.md) :
  First timestamp for taking time
- [`tockR()`](https://matorholt.github.io/cancR/reference/tockR.md) :
  Last timestamp for taking time
- [`viewR()`](https://matorholt.github.io/cancR/reference/viewR.md) :
  Graphical overview of the structure of a multilevel list
