#' Get all local AQCSV files in the specified directories
#'
#' @param local_path Character string of the local directory to search for files
#' @param local_dirs Named character vector of directory names within `local_path` that store each set of AQCSVs
#' @return List of character vectors of file paths of all local AQCSV files
#' @export
get_local_paths <- function(local_path, local_dirs) {
  local_dirs |>
    lapply(\(local_dir) {
      local_path |> 
        file.path(local_dir) |>
        list.files(recursive = TRUE, full.names = TRUE)
    })
}
