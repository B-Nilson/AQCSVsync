download_missing_files <- function(files_to_get, local_path = ".", local_dir = "AQCSVS") {
  where_they_go <- local_path |>
    file.path(local_dirs, names(files_to_get))

  # Attempt to download files, raise warnings if failed
  seq_along(files_to_get) |>
    sapply(\(i) {
      warn_prefix <- paste("Failed to download file", files_to_get[i])
      files_to_get[i] |>
        download.file(where_they_go[i], mode = "wb") |>
        handyr::on_error(.return = NULL, .warn = warn_prefix)
    })
  
  invisible()
}
