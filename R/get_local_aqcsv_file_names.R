get_local_aqcsv_file_names = function(local_path, local_dirs) {
  # Get names of local files
  local_files = local_dirs |>
    # Loop through each file set directory, preppending the local path
    lapply(function(x) file.path(local_path, x)) |>
    # Loop through these, getting a list of files in each directory
    lapply(list.files)
  local_files = names(local_files) |>
    # Loop through lists of files, preppending the server path and dir to each file
    lapply(function(file_set) {
      file.path(local_path, local_dirs[[file_set]], local_files[[file_set]]) |>
        setNames(local_files[[file_set]])
    })
}
