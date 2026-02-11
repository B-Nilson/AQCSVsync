#### --- Script Details ---
### Name: aqcsv_sync.R
### Author: B. Nilson (brayden.nilson@ec.gc.ca)

#### --- Purpose ---
### Script to be run every hour on ECCC Sci. Net. to keep
### local AQCSV files in sync with the ones generated
### by AQmap on UNBC servers.

#### --- Actions Performed ---
### Automatically detects duplicated local files and removes the older file(s)
### Automatically detects outdated local files and removes them
### Detects files existing on UNBC server that do not exist locally
### and downloads these files to store locally

#### --- Extra details ---
### AQCSV files contain all purpleair obs on AQmap hour by hour
### 3 sets of AQCSV files exist:
###   1. RAW: raw, uncorrected purpleair data
###   2. COR: corrected (UNBC correction) purpleair data
###   3. FEM: Same as 1. with nearest FEM obs appended

### `|>` is the "pipe" operator
### This "pipes" the output of the left side to the next operation
### i.e. x |> mean() sends `x` to the mean function, calculating the mean of x
### or mydata |> dplyr::group_by(year) |> dplyr::summarise(mean) groups `mydata`
### by the `year` column, then calculates the mean for each group
### `|>` allows for a "pipeline" of operations

# Inputs ------------------------------------------------------------------

logs <- handyr::log_step("Starting AQCSV Sync", header = TRUE)

## --- Local details ---
# Where are the AQCSV files stored locally?
# local_path <- "/fs/homeu2/eccc/oth/airq_west/jna001/Data/PurpleAir"
local_path <- "."
# What are the directory names within `local_path` that store the AQCSVs?
local_dirs <- list(
  RAW = "RAW",
  COR = "COR",
  FEM = "FEM"
)

# Get External (UNBC) File Details ----------------------------------------

logs$external_files <- handyr::log_step("Getting external file details")

# Read in lists of AQCSV files available
server_files <- get_external_file_paths()

# Get file creation dates in case of updates
server_file_dates <- server_files |>
  lapply(get_aqcsv_creation_date)

# Get Local (SciNet) File Details -----------------------------------------

logs$local_files <- handyr::log_step("Getting local file details")

# Get names of local files
local_files <- local_path |>
  get_local_aqcsv_file_paths(local_dirs = local_dirs)

# Get file creation dates in case of updates
local_file_dates <- local_files |>
  lapply(get_aqcsv_creation_date)

# Remove Duplicated Local Files -------------------------------------------

logs$duplicated_files <- handyr::log_step("Removing duplicated local files")

# Get lists of files in each set that have newer versions already locally
duplicated_local_files <- local_files |>
  get_local_duplicates(local_file_dates = local_file_dates)

# Output messaging with information on duplicated files
duplicated_local_files |> summarise_files()

# Remove the local files that are duplicated
if (length(unlist(duplicated_local_files)) > 0) {
  duplicated_local_files |> unlist() |> file.remove()
  local_files <- local_path |>
    get_local_aqcsv_file_paths(local_dirs = local_dirs)
}

# Remove Outdated Local Files ---------------------------------------------

logs$outdated_files <- handyr::log_step("Removing outdated local files")

# Get local file paths that are out of date
outdated_local_files <- seq_along(local_files) |>
  lapply(\(i) {
    local_files[[i]] |>
      get_outdated_files(
        server_file_dates = server_file_dates[[i]],
        local_file_dates = local_file_dates[[i]]
      )
  })

# Output messaging with information on outdated files
outdated_local_files |> summarise_files()

# Remove the local files that are out of date
if (length(unlist(outdated_local_files)) > 0) {
  outdated_local_files |> unlist() |> file.remove()
  local_files <- local_path |>
    get_local_aqcsv_file_paths(local_dirs = local_dirs)
}

# Download External Files Not Found Locally -------------------------------

logs$missing_files <- handyr::log_step("Downloading missing external files")

# Find files that dont exist locally
files_to_get <- seq_along(server_files) |>
  lapply(function(i) {
    overlap <- names(server_files[[i]]) %in% names(local_files[[i]])
    server_files[[i]][!overlap]
  })

# Output messaging with information on outdated files
files_to_get |> summarise_files()

# Download missing files
seq_along(files_to_get) |>
  lapply(function(i) {
    file.path(local_path, local_dirs[[i]]) |>
      dir.create(showWarnings = FALSE, recursive = TRUE)
    files_to_get[[i]] |>
      download_missing_files(
        local_path = local_path,
        local_dir = local_dirs[[i]]
      )
  })

logs$complete <- handyr::log_step("AQCSV Sync Complete.")

logs |> handyr::summarise_logs()
