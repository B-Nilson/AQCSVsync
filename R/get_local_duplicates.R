get_local_duplicates <- function(file_dates) {
  if (is.null(file_dates)) {
    return(NULL)
  }
  file_dates |>
    dplyr::arrange(dplyr::desc(created)) |>
    dplyr::filter(duplicated(observed)) |>
    dplyr::pull(file_path)
}
