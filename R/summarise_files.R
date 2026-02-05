summarise_files <- function(file_paths) {
  # Get count of files
  n_files <- file_paths |>
    unlist() |>
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
}
