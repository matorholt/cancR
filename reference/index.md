# Package index

## Data management functions

Functions for typical data management tasks

- [`cpR()`](cpR.md) : Fix CPR numbers with removed leading zeros
- [`cutR()`](cutR.md) : Cut numeric variables into intervals
- [`datR()`](datR.md) : Convert dates from character to date format
- [`distributR()`](distributR.md) : Assessment of distribution of
  continuous variables with histograms, QQ-plots and the Shapiro-Wilks
  test
- [`factR()`](factR.md) : Factorize variables
- [`groupR()`](groupR.md) : Extract first n groups in data frame
- [`missR()`](missR.md) : Overview of NAs in a dataframe
- [`pvertR()`](pvertR.md) : Format p-values to AMA manual of style
- [`structR()`](structR.md) : Convert dates to status indicator and
  time-to-event
- [`readR()`](readR.md) : Load csv, excel, rds and parquet files
- [`recodR()`](recodR.md) : Recode multiple variables
- [`redcapR()`](redcapR.md) : Autoformatting of redcap exports
- [`rowR()`](rowR.md) : Perform rowwise operations
- [`summarisR()`](summarisR.md) : Graphical overview of an entire
  dataset

## Statistical Analyses

Functions that perform statistical analysis

- [`incidencR()`](incidencR.md) : Directly standardized incidence rates
  using the WHO standard population
- [`estimatR()`](estimatR.md) : Absolute risk estimation of
  time-to-event data with competing risks
- [`iteratR()`](iteratR.md) : Perform multiple estimatR analyses
- [`extractR()`](extractR.md) : Extraction of key results from the
  estimatR function
- [`inferencR()`](inferencR.md) : Causal inference of time-to-event data
- [`clustR()`](clustR.md) : Causal inference on time-to-event data with
  clustering and competing risks
- [`followR()`](followR.md) : Calculate median follow-up time using the
  inverse Kaplan-Meier method

## Tables and figures

Functions for producing and formatting tables and figures

- [`collectR()`](collectR.md) : Collect multiple plots into one
- [`dagR()`](dagR.md) : Draw directed acyclic graphs (DAGS)
- [`plotR()`](plotR.md) : Autoplot for estimatR, inferencR and clustR
- [`savR()`](savR.md) : Save plots and tables
- [`tablR()`](tablR.md) : Create frequency tables

## Register-specific functions

Functions customized for the danish registers

- [`cci_timR()`](cci_timR.md) : Generation of time-dependent CCI and
  comorbidities from the Charlson Comorbidity Index
- [`decodR()`](decodR.md) : Decoding of the main codelist for loading
  and searching in registries
- [`formatR()`](formatR.md) : Auto-formatting of a data frame with
  layout option for typical levels and labels
- [`includR()`](includR.md) : Inclusion criteria for registry-studies
- [`loadR()`](loadR.md) : Load registers
- [`matchR()`](matchR.md) : Perform exposure-density matching for
  multilevel data
- [`reportR()`](reportR.md) : Overview of matched and unmatched cases
- [`searchR()`](searchR.md) : Find covariates or outcomes from the
  registers
- [`simulatR()`](simulatR.md) : Simulate danish health registers
- [`updatR()`](updatR.md) : Update of Charlson Comorbidity Index or
  covariate values over time

## Built-in datasets

Datasets for testing functions

- [`analysis_df`](analysis_df.md) : Simulated dataset for model testing
- [`match_df`](match_df.md) : Simulated dataset for the matchR algorithm
- [`redcap_df`](redcap_df.md) : Simulated Redcap dataset
- [`covariates_df`](covariates_df.md) : Simulated time-dependent
  covariates dataset.
- [`population_denmark`](population_denmark.md) : Population table,
  Denmark
- [`population_who`](population_who.md) : Population table, WHO

## Miscellaneous functions

Small utility functions

- [`checkR()`](checkR.md) : Detection of positivity violations (empty
  levels)
- [`closR()`](closR.md) : Pick the closest value from a range in vector.
- [`colR()`](colR.md) : Set default color palette
- [`combinR()`](combinR.md) : Generate all possible
  cominations/permutations
- [`formatR()`](formatR.md) : Auto-formatting of a data frame with
  layout option for typical levels and labels
- [`geom_stepribbon()`](geom_stepribbon.md) : Stepribbon geom for
  survival curves
- [`listR()`](listR.md) : Routine modifications of lists
- [`modeR()`](modeR.md) : Get the mode (most common value) of a vector.
- [`numbR()`](numbR.md) : Format numeric vectors
- [`powR()`](powR.md) : Converter from Rscript to powershell format
- [`randomizR()`](randomizR.md) : Generate stratified, block randomized
  allocation sequence
- [`rollR()`](rollR.md) : Assign rolling ID.
- [`tickR()`](tickR.md) : First timestamp for taking time
- [`tockR()`](tockR.md) : Last timestamp for taking time
- [`viewR()`](viewR.md) : Graphical overview of the structure of a
  multilevel list
