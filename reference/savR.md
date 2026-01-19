# Save plots and tables

Wrapper for ggsave with default settings and automatic saving of
multiple formats

## Usage

``` r
savR(
  object,
  name,
  width = 154,
  height,
  unit = "mm",
  scale = 2,
  dpi = 900,
  device = NULL,
  compression = "lzw",
  format = c("pdf"),
  size = 9,
  table.width = 1,
  folder = "Tables and Figures",
  sep = ";"
)
```

## Arguments

- object:

  object to save

- name:

  File name of saved object without extension

- width:

  width

- height:

  heigth. If missing autoscaling is performed

- unit:

  mm or cm

- scale:

  to fit

- dpi:

  resolution

- device:

  cairo for special occasions

- compression:

  for tiff

- format:

  choose between pdf, svg, tiff, jpg and png

## Value

Saves object automatically in current project folder
