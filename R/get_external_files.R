#' Get file paths for AQCSV files on the AQmap server
#'
#' @param base_url Base URL for server files
#' @param server_dirs Directory names for server files
#' @param server_file_lists File names for server file lists
#'
#' @return List of character vectors of server file paths for each of `server_dirs`
#' @export
get_server_paths <- function(
  base_url = "https://aqmap.ca/aqmap/data/AQCSV",
  server_dirs = c(RAW = "raw", COR = "cor", FEM = "fem"),
  server_file_lists = c(
    RAW = "file_list_raw.txt",
    COR = "file_list_cor.txt",
    FEM = "file_list_fem.txt"
  )
) {
  server_files <- file.path(base_url, server_file_lists) |>
    lapply(\(url) data.table::fread(url, showProgress = FALSE)[, 1])
  seq_along(server_files) |>
    lapply(\(i) base_url |> file.path(server_dirs[[i]], server_files[[i]]))
}
