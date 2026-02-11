#' Get local files that have duplicate observation dates
#'
#' @param file_dates Data frame of file details with columns for file path, data observation date, and file creation date
#' @return Character vector of file paths of all but the newest of local files with duplicates
#' @export
get_local_duplicates <- function(file_dates) {
  if (is.null(file_dates)) {
    return(NULL)
  }
  file_dates |>
    dplyr::arrange(dplyr::desc(created)) |>
    dplyr::filter(duplicated(observed)) |>
    dplyr::pull(file_path)
}
