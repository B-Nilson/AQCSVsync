# Get file creation timestamp from AQCSV file name
get_aqcsv_creation_date = function(file_names) {
  data_dates = file_names |>
    # Use file name not full path (we set the names previously)
    names() |>
    # Extract datestamp at start of filename
    stringr::str_split("_aq\\.txt", simplify = TRUE) |>
    _[, 1] |>
    stringr::str_remove_all("COR|FEM|RAW") |>
    # Convert to datetime object
    lubridate::ymd_hm()
  creation_dates = file_names |>
    # Extract datestamp at end of filename
    stringr::str_split("AQCSV:", simplify = TRUE) |>
    _[, 2] |>
    # Convert to datetime object
    lubridate::ymd_hms()
  return(creation_dates |> setNames(data_dates))
}
