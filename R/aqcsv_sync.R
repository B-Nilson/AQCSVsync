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
### or mydata |> group_by(year) |> summarise(mean) groups `mydata`
### by the `year` column, then calculates the mean for each group
### `|>` allows for a "pipeline" of operations

# Packages ----------------------------------------------------------------

# Install packages if needed (should run <=1 time per machine)
if (!"dplyr" %in% installed.packages()) {
  install.packages("dplyr")
}
if (!"stringr" %in% installed.packages()) {
  install.packages("stringr")
}
if (!"lubridate" %in% installed.packages()) {
  install.packages("lubridate")
}

# Load packages
require(dplyr, quietly = TRUE) # for data manipulations
require(stringr, quietly = TRUE) # for text manipulation
require(lubridate, quietly = TRUE) # for date manipulation

# Function to print out messages as script progresses
printout = function(msg) cat(format(Sys.time(), "%F, %T"), msg, "\n")

# Function to get local AQCSV file lists
get_local_aqcsv_file_names = function(local_path, local_dirs) {
  # Get names of local files
  local_files = local_dirs |>
    # Loop through each file set directory, preppending the local path
    lapply(function(x) file.path(local_path, x)) |>
    # Loop through these, getting a list of files in each directory
    lapply(list.files)
  local_files = names(local_files) |>
    # Loop through lists of files, preppending the server path and dir to each file
    lapply(function(file_set) {
      file.path(local_path, local_dirs[[file_set]], local_files[[file_set]]) |>
        setNames(local_files[[file_set]])
    })
}

# Function to get file creation timestamp from AQCSV file name
get_aqcsv_creation_date = function(file_names) {
  data_dates = file_names |>
    # Use file name not full path (we set the names previously)
    names() |>
    # Extract datestamp at start of filename
    str_split("_aq\\.txt", simplify = TRUE) |>
    _[, 1] |>
    str_remove_all("COR|FEM|RAW") |>
    # Convert to datetime object
    ymd_hm()
  creation_dates = file_names |>
    # Extract datestamp at end of filename
    str_split("AQCSV:", simplify = TRUE) |>
    _[, 2] |>
    # Convert to datetime object
    ymd_hms()
  return(creation_dates |> setNames(data_dates))
}

# Print startup message
printout("Starting AQCSV Sync")

# Inputs ------------------------------------------------------------------

## --- Server details ---
# Where are the AQCSV files stored on UNBC?
server_path = "https://cyclone.unbc.ca/aqmap/data/AQCSV"
# What are the names of the files which list the AQCSV files on the server?
server_file_lists = c(
  RAW = "file_list_raw.txt",
  COR = "file_list_cor.txt",
  FEM = "file_list_fem.txt"
)
# What are the directory names within `local_path` that store the AQCSVs?
server_dirs = c(
  RAW = "raw",
  COR = "cor",
  FEM = "fem"
)

## --- Local details ---
# Where are the AQCSV files stored locally?
local_path = "/fs/homeu2/eccc/oth/airq_west/jna001/Data/PurpleAir"
# What are the directory names within `local_path` that store the AQCSVs?
local_dirs = list(
  RAW = "RAW",
  COR = "COR",
  FEM = "FEM"
)

# Get External (UNBC) File Details ----------------------------------------

printout("Getting external file details...")

# Read in lists of AQCSV files available
server_files = server_file_lists |>
  # Loop through each file list, preppending the server path
  lapply(function(file_list) file.path(server_path, file_list)) |>
  # Loop through these, reading each in
  lapply(function(url) read.table(url)[, 1])
server_files = names(server_files) |>
  # Loop through lists of files, preppending the server path and dir to each file
  lapply(function(file_set) {
    file.path(
      server_path,
      server_dirs[[file_set]],
      server_files[[file_set]]
    ) |>
      setNames(server_files[[file_set]])
  })

# Get file creation dates in case of updates
server_file_dates = server_files |>
  lapply(get_aqcsv_creation_date)

# Get Local (SciNet) File Details -----------------------------------------

printout("Getting local file details...")

# Get names of local files
local_files = get_local_aqcsv_file_names(local_path, local_dirs)

# Get file creation dates in case of updates
local_file_dates = local_files |>
  lapply(get_aqcsv_creation_date)

# Remove Duplicated Local Files -------------------------------------------

printout("Removing duplicated local files...")

# Get lists of files in each set that have newer versions already locally
duplicated_local_files = 1:length(local_file_dates) |>
  # Loop through RAW/COR/FEM (indexed by `i`)
  lapply(function(i) {
    data.frame(
      data_dates = names(local_file_dates[[i]]),
      creation_dates = local_file_dates[[i]],
      paths = local_files[[i]]
    ) |>
      arrange(desc(creation_dates)) |>
      filter(duplicated(data_dates)) |>
      pull(paths)
  })

# Get count of duplicated files
n_duplicated_files = duplicated_local_files |>
  unlist() |>
  length()

# Output messaging with information on outdated files
cat("   ", n_duplicated_files, "files have newer versions locally\n")
if (n_duplicated_files) {
  # if any duplicated files
  cat("\t", paste(duplicated_local_files |> unlist(), collapse = "\n\t "))
  cat("\n")

  # Remove the local files that are duplicated
  file.remove(duplicated_local_files |> unlist())

  # Regenerate local file list
  local_files = get_local_aqcsv_file_names(local_path, local_dirs)
}

# Remove Outdated Local Files ---------------------------------------------

printout("Removing outdated local files...")

# Get local file paths that are out of date
outdated_local_files = 1:length(local_file_dates) |>
  # Loop through RAW/COR/FEM (indexed by `i`)
  lapply(function(i) {
    # Make a dataframe matching up file data dates and creation times
    dates = data.frame(
      data = names(server_file_dates[[i]]),
      server_creation = server_file_dates[[i]]
    ) |>
      full_join(
        data.frame(
          data = names(local_file_dates[[i]]),
          local_creation = local_file_dates[[i]],
          path = local_files[[i]]
        ),
        by = "data"
      ) |>
      # Get differences in creation dates
      mutate(
        creation_diff = difftime(
          local_creation,
          server_creation,
          units = "hours"
        )
      ) |>
      # Drop entries with no differences
      filter(creation_diff != 0) |>
      # Drop older entries when duplicates on server exist
      arrange(desc(server_creation)) |>
      filter(!duplicated(data)) |>
      # Return local paths of outdated files
      pull(path)
  }) |>
  unlist()

# Get count of outdated files
n_outdated_files = outdated_local_files |>
  length()

# Output messaging with information on outdated files
cat("   ", n_outdated_files, "files are out of date\n")
if (n_outdated_files) {
  # if any outdated files
  cat("\t", paste(outdated_local_files, collapse = "\n\t "))
  cat("\n")

  # Remove the local files that are out of date
  file.remove(outdated_local_files)

  # Regenerate local file list
  local_files = get_local_aqcsv_file_names(local_path, local_dirs)
}

# Download External Files Not Found Locally -------------------------------

printout("Downloading missing external files...")

# Find files that dont exist locally
files_to_get = 1:length(server_files) |>
  # Loop through RAW/COR/FEM (indexed by `i`)
  lapply(function(i) {
    # Find overlapping files between server / local
    overlap = names(server_files[[i]]) %in% names(local_files[[i]])
    # Drop urls which overlap
    server_files[[i]][!overlap]
  })

# Get names of missing files
n_missing_files = files_to_get |>
  unlist() |>
  length()

# Output messaging with information on missing files
cat("   ", n_missing_files, "files to download\n")
if (n_missing_files) {
  # if files to download
  cat("\t", paste(unlist(files_to_get), collapse = "\n\t "))
  cat("\n")
  # Make local file paths for files to download
  where_they_go = 1:length(files_to_get) |>
    lapply(function(i) {
      file.path(local_path, local_dirs[i], names(files_to_get[[i]]))
    })

  # Download files we don't already have and store them locally
  1:length(files_to_get) |>
    # Loop through RAW/COR/FEM (indexed by `i`)
    lapply(function(i) {
      # Get server path
      urls = files_to_get[[i]]
      # Get local path
      lcls = where_they_go[[i]]
      # Attempt to download files, raise warnings if failed
      1:length(urls) |>
        sapply(function(j) {
          tryCatch(
            download.file(urls[j], lcls[j]),
            error = function(e, ...) {
              warning(e)
              NULL
            }
          )
        })
    })
}

printout("AQCSV Sync Complete.")
