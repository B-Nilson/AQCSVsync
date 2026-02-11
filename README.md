
# AQCSVsync

<!-- badges: start -->
<!-- badges: end -->

The goal of AQCSVsync is to enable syncing of the [AQCSV files produced on the AQmap server](https://aqmap.ca/aqmap/data/AQCSV/) to a local directory for supporting internal tool development.

Handles removing duplicates and replacing old files with updated versions.

## Installation

You can install the development version of AQCSVsync like so:

``` r

# install.packages("pak")
pak::pak("B-Nilson/AQCSVsync")

```

## Sync AQCSV Files

``` r

library(AQCSVsync)

# Where to write the files to locally
data_dir <- "./data/" # main directory
sub_dirs <- list( # relative to data_dir
  RAW = 'RAW', # raw PurpleAir data
  COR = 'COR', # bias corrected and qa-qced PurpleAir data
  FEM = 'FEM' # raw PurpleAir data with nearest FEM pm2.5
)

# Download missing files, remove local duplicates, replace outdated files
# (initial run may take a bit to download the full dataset)
data_dir |> sync_aqcsvs(local_dirs = sub_dirs)

```
