#' reads all CSVs from a folder
#'
#' @param directory_name name of the directory to read
#'
#' @return lsit of dataframe
#' @export
#' @importFrom utils read.csv
#' @import assertthat
#'
#' @examples
#' \dontrun{
#' csv_reader("../../path/to/dir")
#' }
csv_reader <- function(directory_name) {
  assertthat::assert_that(is.dir(directory_name), msg = "not a directory")
  assertthat::assert_that(is.readable(directory_name), msg = "file not writable")

  file_list = list.files(directory_name, pattern = ".csv*", full.names = TRUE)
  csv_list = lapply(file_list, FUN = read.csv)
  return(csv_list)
}
