# Efficient Data Management in R

##### Setup

## ----setup----
## Save package names as a vector of strings
pkgs <-
  c(
    "foreign",
    ### read data stored in various formats
    "readstata13",
    ### read data stored by Stata 13-16
    "reshape2",
    ### flexibly reshape data
    "countrycode",
    ### convert country names and country codes
    "lubridate",
    ### dates and time
    "dplyr",
    ### tools for data manipulation
    "magrittr",
    ### piping operations
    "tidyr",
    ### tool to deal with messy data (and get "tidy data")
    "ggplot2",
    ### data visualization using a grammar of graphics
    "fabricatr",
    ### imagine your data before you collect it
    "knitr"            
    ### general-purpose tool for dynamic report generation
  )

## Install uninstalled packages
lapply(pkgs[!(pkgs %in% installed.packages())], install.packages)

## Load all packages to library and adjust options
lapply(pkgs, library, character.only = TRUE)
