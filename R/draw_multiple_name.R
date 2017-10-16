#' draws the curves of the number of babys born with the names in name_vector
#'
#' @param name_vector a vector of names to plot
#'
#' @return ggplot graph
#' @export
#' @import ggplot2
#' @import dplyr
#' @import prenoms
#' @import assertthat
#'
#' @examples
#' \dontrun{
#' draw_multiple_name( c("Joe", "Kalysie"))
#' }
#'
draw_multiple_name <- function( name_vector) {
  assertthat::assert_that(class(name_vector) == "character", msg = "names must be character or character vector")
  data <- prenoms::prenoms
 name_count <- data %>%
    filter_(~name %in% name_vector) %>%
    group_by_(~year, ~name) %>%
    summarise(count = sum(n))
   graph <-  ggplot2::ggplot(name_count, ggplot2::aes( x = name_count$year , y = name_count$count, color = name_count$name)) +
    ggplot2::geom_line() +
    ggplot2::ggtitle(paste(name_vector, collapse = " , "))

   return(graph)
}
