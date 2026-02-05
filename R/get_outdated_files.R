get_outdated_files <- function(
  local_files,
  server_file_dates,
  local_file_dates
) {
  if (length(local_files) == 0) {
    return(NULL)
  }

  # Make a dataframe matching up file data dates and creation times
  dates <- data.frame(
    data = names(server_file_dates),
    server_creation = server_file_dates
  ) |>
    dplyr::full_join(
      data.frame(
        data = names(local_file_dates),
        local_creation = local_file_dates,
        path = local_files
      ),
      by = "data"
    )

  # Get differences in creation dates
  dates |>
    dplyr::mutate(
      creation_diff = local_creation |>
        difftime(server_creation, units = "hours")
    ) |>
    # Drop entries with no differences
    dplyr::filter(creation_diff != 0) |>
    # Drop older entries when duplicates on server exist
    dplyr::arrange(dplyr::desc(server_creation)) |>
    dplyr::filter_out(duplicated(data)) |>
    # Return local paths of outdated files
    dplyr::pull(path)
}
