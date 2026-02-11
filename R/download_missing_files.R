#' Download missing files from AQCSV server
#'
#' @param files_to_get Character vector of file paths to download
#' @param local_path Character string of local directory to store files
#' @return invisible NULL
#' @export
#' @examples
#' files_to_get <- c("file1.csv", "file2.csv")
#' download_missing_files(files_to_get)
download_missing_files <- function(
  files_to_get,
  local_dir = "./COR"
) {
  files_names <- basename(files_to_get)
  where_they_go <- local_dir |> file.path(files_names)

  # Attempt to download files, raise warnings if failed
  seq_along(files_to_get) |>
    sapply(\(i) {
      warn_prefix <- paste("Failed to download file", files_to_get[i])
      files_to_get[i] |>
        download.file(destfile = where_they_go[i], mode = "wb") |>
        handyr::on_error(.return = NULL, .warn = warn_prefix)
    })

  invisible()
}
