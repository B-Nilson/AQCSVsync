#' Get paths to local files that are out of date relative to the server
#'
#' @param server_file_dates Data frame of server file details with columns for file path, data obervation date, and file creation date
#' @param local_file_dates Data frame of local file details with columns for file path, data obervation date, and file creation date
#' @return Character vector of file paths of outdated local files
#' @export
get_outdated_files <- function(server_file_dates, local_file_dates) {
  if (length(local_file_dates) == 0) {
    return(NULL)
  }

  server_file_dates |>
    dplyr::full_join(
      y = local_file_dates,
      by = "observed",
      suffix = c("_server", "_local")
    ) |>
    dplyr::mutate(
      created_diff = .data$created_local |>
        difftime(.data$created_server, units = "hours")
    ) |>
    dplyr::filter(.data$created_diff != 0) |>
    dplyr::arrange(dplyr::desc(.data$created_server)) |>
    dplyr::filter_out(duplicated(.data$observed)) |>
    dplyr::pull(.data$file_path_local)
}
