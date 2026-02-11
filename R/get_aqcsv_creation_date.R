#' Get observed and created dates from AQCSV file names
#'
#' @param aqcsv_paths Character vector of paths to AQCSV files.
#' @param file_suffix Character string to remove from ends file names such that the end of the file is the creation date. Defaults to "" (no file suffix to remove).
#' @param file_sep Character string used to seperate pieces of the AQCSV file name. Defaults to ":".
#' @param date_formats Named character vector of date formats to use for each date in the file name. Defaults to c(observed = "%Y%m%d%H%M", created = "%Y%m%d%H%M%S").
#' @return A data.frame with the first column being the file path and the remaining columns being the extracted dates.
#' @export
#' @examples
#' aqcsv_path <- "/some/path/FEM20240101_0000_aq.txt:AQ:PurpleAir:AIRNOW:AQCSV:20260106195903"
#' get_aqcsv_dates(aqcsv_path)
get_aqcsv_dates <- function(
  aqcsv_paths,
  file_suffix = "",
  file_sep = ":",
  date_formats = c(observed = "%Y%m%d%H%M", created = "%Y%m%d%H%M%S")
) {
  if (length(aqcsv_paths) == 0) {
    return(NULL)
  }
  date_names <- names(date_formats)

  file_parts <- basename(aqcsv_paths) |>
    stringr::str_remove(stringr::str_escape(file_suffix) |> paste0("$")) |>
    stringr::str_split(stringr::fixed(file_sep), simplify = TRUE)
  date_indexes <- c(1, ncol(file_parts))

  data.frame(unname(aqcsv_paths), file_parts) |>
    dplyr::select(!!c(1, date_indexes + 1)) |>
    stats::setNames(c("file_path", date_names)) |>
    dplyr::mutate(
      dplyr::across(-"file_path", \(date_strings) {
        date_format <- date_formats[[dplyr::cur_column()]]
        date_strings |>
          stringr::str_remove_all(pattern = "[^0-9.]") |>
          lubridate::parse_date_time(orders = date_format, tz = "UTC")
      })
    )
}
