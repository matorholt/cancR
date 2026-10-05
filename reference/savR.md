# Save plots and tables

Wrapper for ggsave with default settings and automatic saving of
multiple formats

## Usage

``` r
savR(
  object,
  name = NULL,
  width = 154,
  height,
  unit = "mm",
  scale = 2,
  dpi = 900,
  device = NULL,
  compression = "lzw",
  format = NULL,
  parquet.format = "zstd",
  parquet.compression = 19,
  size = 9,
  table.width = 1,
  folder = NULL,
  sep = ";",
  verbose = T
)
```

## Arguments

- object:

  object to save. If object = "session" the sessionInfo will be exported
  as a flextable.

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

- parquet.format:

  format for parquet files, default = "zstd"

- parquet.compression:

  compression amount for parquet files, default = 19

- size:

  text size for flextables, default = 9

- table.width:

  table width for flextables, default = 1 (full width)

- folder:

  optional subfolder location, default is working directory

- sep:

  separator for csv files, default = ";"

## Value

Saves object automatically in current project folder
