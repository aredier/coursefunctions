
#' Title : function that checks the authorizations of the file before writing a DataFrame into a csv
#'
#' @param data dataframe to be writen in a csv file
#' @param file_path path to folder
#' @param ... rest of the parameters of the write.csv2 functions
#'
#' @return file_path
#' @export
#' @importFrom utils write.csv2
#' @import assertthat
#'
#' @examples
#' \dontrun{
#' save_as_csv(MyDataFrame, "../path/to/folder")
#'
#' MyDataFrame %>% save_as_csv("../path/to/folder")
#' }
#'
save_as_csv <- function(data , file_path,  ...)
{
  assertthat::assert_that(grepl("*.csv$", file_path), msg = "please give a .csv file")
  assertthat::assert_that(is.dir(dirname(file_path)), msg = "not a directory")
  assertthat::assert_that(is.writeable(dirname(file_path)), msg = "file not writable")
  assertthat::assert_that(not_empty(data), msg = "no data to be writen")
  assertthat::assert_that(is.data.frame(data), msg = "can only write data frames to csv ")

  write.csv2(x = data, file = file_path,  ...)

  invisible(normalizePath(file_path))
}
