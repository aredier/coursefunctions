#' draws the number born in France with a specific name and and sex
#'
#' @param the_name name of the babies
#' @param the_sex sex of the babies
#'
#' @return ggplot graph
#' @export
#' @import dplyr
#' @import ggplot2
#' @import prenoms
#' @import assertthat
#'
#' @examples
#' \dontrun{
#' draw_name("Antoine", "M")
#' }
#'
draw_name <- function(the_name, the_sex) {

  assertthat::assert_that(class(the_name) == "character", msg = "the name must be a character")
  assertthat::assert_that(class(the_sex) == "character", msg = "the sex must be a character")
  assertthat::assert_that(length(the_sex) == 1 & length(the_name) == 1, msg = "arguments must be of length 1")

  name_data <- prenoms::prenoms %>%
    filter_(~name %in% c(the_name)) %>%
    filter_(~sex %in% c(the_sex)) %>%
    group_by_(~year) %>%
    summarise(count = sum(n))

    graph <- ggplot2::ggplot(name_data, ggplot2::aes( x = name_data$year , y = name_data$count)) +
    ggplot2::geom_line() +
    ggplot2::ggtitle(the_name)
    return(graph)
}
