#' @title Define map colors
#' @author Manuel Teodoro
#' @description Creates an object of class \code{map_colors} that contains colors used for the maps.
#'
#' @param dots_orgs,target_country,empty_countries,border_countries,oceans,text_cities,text_legend,text_copyright
#' String as character defining the colors of the different aspects of the maps, in hexadecimal notation.
#'
#' @return An S3 object of class \code{map_colors} that inherits \code{map_aesthetics}.
#'
#' @export
#' @examples
#' my_map_colors <- define_map_colors(dots_orgs = "#574166",
#'                                    target_country = "#8caeb4",
#'                                    empty_countries = "#f3f3f3",
#'                                    border_countries = "#9c9c9c",
#'                                    oceans = "#4e91d2",
#'                                    text_cities = "#a0a0a0",
#'                                    text_legend = "#283151",
#'                                    text_copyright = "#f3f3f3")
define_map_colors <- function(dots_orgs,
                              target_country,
                              empty_countries,
                              border_countries,
                              oceans,
                              text_cities,
                              text_legend,
                              background_legend,
                              text_copyright) {
  require(stringr)
  all_arguments <- c(as.list(environment()))
  for (arggs in all_arguments) {
    stopifnot("All arguments must be character" = is.character(arggs))
    if (nchar(arggs) != 7) {
      stop("Colors should be in hex notation")
    }
    if (!str_detect(arggs, "^#")) {
      stop("Colors should be in hex notation")
    }
  }

  structure(
    list(
      dots_orgs = dots_orgs,
      target_country = target_country,
      empty_countries = empty_countries,
      border_countries = border_countries,
      oceans = oceans,
      text_cities = text_cities,
      text_legend = text_legend,
      background_legend = background_legend,
      text_copyright = text_copyright),
    class = c("map_colors"))
}

#' @title Test if the object is of class \code{map_colors}
#' @author Manuel Teodoro
#' @param x An object
#' @return 'TRUE' if the object inhertis from the \code{map_colors} class
#' @export
is.map_colors <- function(x) inherits(x, "map_colors")


#' @export
print.map_colors <- function(x, ...) {
  stopifnot(is.map_colors(x))
  print("The chosen colors")
  for (argg in names(x)) {
    message(paste(argg, x[argg], sep = " : "))
  }
}

#' @export
default_map_colors <- define_map_colors(dots_orgs = "#493252",
                                        target_country = "#8caeb4",
                                        empty_countries = "#f3f3f3",
                                        border_countries = "#9c9c9c",
                                        oceans = "#4e91d2",
                                        text_cities = "#a0a0a0",
                                        text_legend = "#493252",
                                        background_legend = "#ffffff",
                                        text_copyright = "#f3f3f3")

#' @title With default map colors
#' @author Manuel Teodoro
#' @description Modifies the default colors
#' 
#' @param list_colors A list containing the parameters of the colors to be changed as passed in the function \code{\link{define_map_colors}}.
#' 
#' @details The function modifies the global objects containing the default colors.
#'
#' @seealso \code{\link{define_map_colors}} for details about the colors.
#' @export
#' @examples
#' my_colors <- with_default_colors(list(dots_orgs = "#000000")
with_default_colors <- function(list_colors = NA) {
  require(purrr)
  modified_map_colors <- default_map_colors
  modified_map_colors <- list_modify(modified_map_colors, !!!list_colors)
  return(modified_map_colors)
}
