#' extracts DataFrame from xlsx sheets (one dataframe per sheet)
#'
#' @param xlsx_path path to xlsx file
#'
#' @return list of dataframe
#' @export
#' @import readxl
#' @import assertthat
#'
#' @examples
#' \dontrun{
#' xlsx_reader(../../path/to/file.xlsx)
#' }
#'
xlsx_reader <- function(xlsx_path){
  assertthat::assert_that(grepl("*.xlsx$", xlsx_path), msg = "please give an .xlsx file")
  assertthat::assert_that(is.dir(dirname(xlsx_path)), msg = "not a directory")
  assertthat::assert_that(is.readable(xlsx_path), msg = "file not writable")
  assertthat::assert_that(file.exists(xlsx_path), msg = "No such file or directory")


  sheet_name_lists <- readxl::excel_sheets(path = xlsx_path)
  sheet_list <- lapply(sheet_name_lists, function (sheet){return(readxl::read_excel(xlsx_path, sheet = sheet))})
  return(sheet_list)
}
