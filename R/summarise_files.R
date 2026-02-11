#' Output information on provided files for logging
#'
#' @param file_paths Character vector (or list of character vectors) of file paths to summarise
#' @return invisible NULL
#' @export
#' @examples
#' file_paths <- c("file1.csv", "file2.csv")
#' summarise_files(file_paths)
summarise_files <- function(file_paths) {
  # Get count of files
  n_files <-  unlist(file_paths) |>
    length()

  # Output messaging with information on outdated files
  handyr::log_step("\t", n_files, "files identified", time = FALSE)
  if (n_files > 0) {
    handyr::log_step(
      "\t\t-",
      file_paths |> unlist() |> paste(collapse = "\n\t\t- "),
      time = FALSE
    )
  }
  invisible()
}
