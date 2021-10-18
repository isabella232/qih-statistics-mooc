# Function to read in raw data in correct format and save

#' process_raw_data
#' Loads all .csv files in data-raw, with column "Month" parsed as a date in the format %b-%y i.e. Jan-20
#' Other columns use default read_csv parsers.
#'
#' @return list of files processed
#' @export
#'
process_raw_data <- function() {
  raw_data_files <- list.files(path = "data-raw", pattern = "\\.csv$")
  data_list <- lapply(raw_data_files, function(x) readr::read_csv(file.path("data-raw", x),
                                                                  col_types = readr::cols(
                                                                    Month = readr::col_date(format = "%b-%y")
                                                              )))
  names(data_list) <- raw_data_files
  
  lapply(raw_data_files, function(x) saveRDS(data_list[[x]], file.path("data", stringr::str_replace(x, "\\.csv", ".rds"))))
  raw_data_files
}
