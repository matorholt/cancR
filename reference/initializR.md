# Initialize new project

Initialize new project

## Usage

``` r
initializR(
  name = NULL,
  path = NULL,
  proj = TRUE,
  script = "standard",
  repo = NULL
)
```

## Arguments

- name:

  optional name of script file, default = "MAIN_SCRIPT"

- path:

  optional name of path. If null a pop-up window for directory selection
  will pop-up

- proj:

  whether a project file should be created (default = T)

- script:

  type of initial script, choose between "standard" and "quarto"
  (default = "standard")

- repo:

  name of github repo if the project should be pushed to github

## Value

an initialized project
