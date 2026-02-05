get_local_duplicates <- function(local_files, local_file_dates) {
  local_file_dates |>
    handyr::for_each(
      .enumerate = TRUE,
      .show_progress = FALSE,
      \(file_dates, i) {
        if (is.null(file_dates)) {
          return(NULL)
        }
        data.frame(
          data_dates = names(local_file_dates[[i]]),
          creation_dates = file_dates,
          paths = local_files[[i]]
        ) |>
          dplyr::arrange(dplyr::desc(creation_dates)) |>
          dplyr::filter(duplicated(data_dates)) |>
          dplyr::pull(paths)
      }
    )
}
