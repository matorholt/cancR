# SNOMED Topography codes

List of SNOMED topograhpy codes with dictionary

## Usage

``` r
t_codes
```

## Format

A data.frame with 8 columns and 2205 rows:

- t.code:

  SNOMED topography code

- danish:

  Danish translation of the topography code

- exact:

  Exact location

- localisation:

  Grouped localisation such as knee, temple, hand

- cluster:

  Related localisations

- loc_spec:

  Whether the localisation is specific (e.g. temple) or non-specific
  ("arm")

- loc_skin:

  Whether the code specifies skin, non-skin or uncertain

- region:

  Low-level grouped localisation (e.g. Head and Neck, Upper Extremity,
  Lower Extremity and Trunk)
