get_local_aqcsv_file_paths <- function(local_path, local_dirs) {
  # Get names of local files
  local_files <- local_dirs |>
    lapply(\(local_dir) file.path(local_path, local_dir)) |>
    lapply(list.files)

  # Loop through lists of files, preppending the server path and dir to each file
  names(local_files) |>
    lapply(\(file_set) {
      local_path |>
        file.path(local_dirs[[file_set]], local_files[[file_set]]) |>
        setNames(local_files[[file_set]])
    })
}
