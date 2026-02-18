# Decoding of the main codelist for loading and searching in registries

Decoding of the main codelist for loading and searching in registries

## Usage

``` r
decodR(codelist, regs = c("pop", "sc", "meta", "dsd"), type = "matching")
```

## Arguments

- codelist:

  List of lists with registries, diagnosis codes and labels. See
  example.

- regs:

  additional registries for loading

- type:

  matching or rtmle, depending on the restructurering of the codelist
  (deafault = "matching")

## Value

codes: Original codelist without modification  
regs: The specified registries for the project. First input in loadR  
loadR: The pattern.list in loadR  
searchR: The pattern.list in searchR  
regex: The codelist converted to regex-expressions with \| as separator
