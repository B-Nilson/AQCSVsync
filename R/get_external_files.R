get_external_file_paths <- function(
  server_path = "https://aqmap.ca/aqmap/data/AQCSV",
  server_dirs = c(RAW = "raw", COR = "cor", FEM = "fem"),
  server_file_lists = c(
    RAW = "file_list_raw.txt",
    COR = "file_list_cor.txt",
    FEM = "file_list_fem.txt"
  )
) {
  # Read in lists of AQCSV files available
  server_files <- server_file_lists |>
    # Loop through each file list, preppending the server path
    lapply(\(file_list) file.path(server_path, file_list)) |>
    # Loop through these, reading each in
    lapply(\(url) read.table(url)[, 1])

  names(server_files) |>
    # Loop through lists of files, preppending the server path and dir to each file
    lapply(\(file_set) {
      file.path(
        server_path,
        server_dirs[[file_set]],
        server_files[[file_set]]
      ) |>
        setNames(server_files[[file_set]])
    })
}
