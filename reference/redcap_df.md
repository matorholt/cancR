# redcap_df

Simulation of a redcap data frame for basic data management

## Usage

``` r
redcap_df
```

## Format

A data frame with 500 rows and 15 columns:

- id:

  unique patient pnr

- sex:

  patient sex, 1 = female, 2 = male

- birth:

  date of birth

- age:

  age at surgery in years

- followup:

  date of last follow-up which can be death, emigration or end of
  database

- date_of_surgery:

  date of surgery

- size:

  size of primary tumor in mm

- type:

  type of tumor, 0 = benign, 1 = in situ, 2 = malignant

- localisation:

  localisation of the primary tumor

- necrosis:

  whether necrosis was present in the primary tumor

- cd10, sox10, ck:

  presence or abscence of immunohistochemical markers

- death_date:

  date of death

- recurrence_date:

  date of first recurrence

- metastasis_date:

  date of first metastasis
