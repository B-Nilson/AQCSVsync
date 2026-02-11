#' Sync AQCSV Files Between AQmap Server and ECCC Science Network
#'
#' Syncs the AQCSV files produced on the AQmap server with local files.
#'
#' @param local_path Character string of the local directory to store the files
#' @param local_dirs Named character vector of directory names within `local_path` that store each set of AQCSVs
#' @return invisible NULL (messages output to console for logging)
#' @export
sync_aqcsvs <- function(
  local_path = "./data",
  local_dirs = list(RAW = "RAW", COR = "COR", FEM = "FEM")
) {
  logs <- handyr::log_step("Starting AQCSV Sync", header = TRUE)

  # Get External (UNBC) File Details ----------------------------------------

  logs$external_files <- handyr::log_step("Getting external file details")

  server_files <- get_server_paths()
  server_file_dates <- server_files |> lapply(get_aqcsv_dates)

  # Get Local (SciNet) File Details -----------------------------------------

  logs$local_files <- handyr::log_step("Getting local file details")

  local_files <- local_path |> get_local_paths(local_dirs = local_dirs)
  local_file_dates <- local_files |> lapply(get_aqcsv_dates)

  # Remove Duplicated Local Files -------------------------------------------

  logs$duplicated_files <- handyr::log_step("Removing duplicated local files")

  duplicated_local_files <- local_file_dates |> lapply(get_local_duplicates)
  duplicated_local_files |> summarise_files()

  any_duplicated <- length(unlist(duplicated_local_files)) > 0
  if (any_duplicated) {
    duplicated_local_files |> unlist() |> file.remove()
    local_files <- local_path |> get_local_paths(local_dirs = local_dirs)
    local_file_dates <- local_files |> lapply(get_aqcsv_dates)
  }

  # Remove Outdated Local Files ---------------------------------------------

  logs$outdated_files <- handyr::log_step("Removing outdated local files")

  # Get local file paths that are out of date
  outdated_local_files <- seq_along(local_file_dates) |>
    lapply(\(i) {
      server_file_dates[[i]] |>
        get_outdated_files(local_file_dates = local_file_dates[[i]])
    })

  # Output messaging with information on outdated files
  outdated_local_files |> summarise_files()

  # Remove the local files that are out of date
  if (length(unlist(outdated_local_files)) > 0) {
    outdated_local_files |> unlist() |> file.remove()
    local_files <- local_path |> get_local_paths(local_dirs = local_dirs)
    local_file_dates <- local_files |> lapply(get_aqcsv_dates)
  }

  # Download External Files Not Found Locally -------------------------------

  logs$missing_files <- handyr::log_step("Downloading missing external files")

  # Find files that dont exist locally
  files_to_get <- seq_along(server_files) |>
    lapply(\(i) {
      is_downloaded <- basename(server_files[[i]]) %in%
        basename(local_files[[i]])
      server_files[[i]][!is_downloaded]
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
          local_dir = file.path(local_path, local_dirs[[i]])
        )
    })

  logs$complete <- handyr::log_step("AQCSV Sync Complete.")

  logs |> handyr::summarise_logs()
  invisible()
}
