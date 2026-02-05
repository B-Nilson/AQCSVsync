get_local_duplicates <- function(local_file_dates) {
  local_file_dates |>
    handyr::for_each(
      .enumerate = TRUE,
      \(file_date, i) {
        data.frame(
          data_dates = names(local_file_dates[[i]]),
          creation_dates = file_date,
          paths = local_files[[i]]
        ) |>
          arrange(desc(creation_dates)) |>
          filter(duplicated(data_dates)) |>
          pull(paths)
      }
    )
}
