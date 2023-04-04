##### ------------------ C L A S S E S ------------------ #####
## --- CONSTRUCTORS ---

#' @title Define map aesthetics
#' @author Manuel Teodoro
#' @description Creates an object of class \code{map_master_aes} that contains the aesthetics details for colors,
#' sizes and legends text.
#' 
#' @param aes_name A string with a name for the object.
#' @param colors Object of class \code{map_colors} defining the colors for the maps.
#' @param sizes Object of class \code{map_sizes} defining the sizes for the maps.
#' @param legends Object of class \code{map_legends} defining the aesthetics for the legends for the maps.
#' 
#' @return An S3 object of class \code{map_master_aes}.
#' 
#' @details The object inherits both classes, \code{map_aesthetics} and \code{map_master_aes}.
#' It works as an object to collect all the aesthetics defined for color, sizes and legends.
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
#' my_sizes_in_map <- define_sizes_in_map(basic_dot = 2.5, text_city = 4)
#' my_legends_specs <- define_legends_specs("Year", "Organizations")
#' 
#' my_aesthetics <- define_map_aesthetics("My maps aesthetics",
#'                                         my_map_colors,
#'                                         my_sizes_in_map,
#'                                         my_legends_specs)
define_map_aesthetics <- function(aes_name, colors, sizes, legends) {
  stopifnot(is.character(aes_name))
  stopifnot(is.map_colors(colors))
  stopifnot(is.map_sizes(sizes))
  stopifnot(is.map_legends(legends))

  structure(
    list(
      Name = aes_name,
      colors = colors,
      sizes = sizes,
      legends = legends),
    class = c("map_aesthetics", "map_master_aes"))
}


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
      text_copyright = text_copyright),
    class = c("map_aesthetics", "map_colors"))
}


#' @title Define map sizes
#' @author Manuel Teodoro
#' @description Creates an object of class \code{map_sizes} that contains the sizes used for the maps.
#' 
#' @param basic_dot,text_city Numeric values defining the sizes of the different aspects to be used in the maps.
#' 
#' @return An S3 object of class \code{map_sizes} that inherits \code{map_aesthetics}.
#' 
#' @export
#' @examples
#' my_sizes_in_map <- define_sizes_in_map(basic_dot = 2.5, text_city = 4)
define_sizes_in_map <- function(basic_dot,
                                text_city) {
  all_arguments <- c(as.list(environment()))
  for (arggs in all_arguments) {
    stopifnot("All arguments must be numeric" = is.numeric(arggs))
  }

  structure(
    list(
      basic_dot = basic_dot,
      text_city = text_city),
    class = c("map_aesthetics", "map_sizes"))
}


#' @title Define map legends specifications
#' @author Manuel Teodoro
#' @description Creates an object of class \code{map_legends} that contains the specifications used for the legends.
#' 
#' @param legend_year,legend_orgs Exact text to be shown in the maps.
#' 
#' @return An S3 object of class \code{map_legends} that inherits \code{map_aesthetics}.
#' 
#' @export
#' @examples
#' my_legends_specs <- define_legends_specs("Year", "Organizations")
define_legends_specs <- function(legend_year,
                                 legend_orgs) {
  all_arguments <- c(as.list(environment()))
  for (arggs in all_arguments) {
    stopifnot("All arguments must be character" = is.character(arggs))
  }

  structure(
    list(
      legend_year = legend_year,
      legend_orgs = legend_orgs),
    class = c("map_aesthetics", "map_legends"))
}



## --- VALIDATORS ---

#' @title Test if the object is of class \code{map_aesthetics}
#' @author Manuel Teodoro
#' @description This function returns ‘TRUE’ for \code{map_aesthetics} objects and ‘FALSE’ for all other objects.
#' 
#' @param x An object
#' @return 'TRUE' if the object inhertis from the {map_aesthetics} class
#' @export
is.map_aesthetics <- function(x) inherits(x, "map_aesthetics")

#' @title Test if the object is of class \code{map_master_aes}
#' @author Manuel Teodoro
#' @description This function returns ‘TRUE’ for \code{map_master_aes} objects and ‘FALSE’ for all other objects.
#' 
#' @param x An object
#' @return 'TRUE' if the object inhertis from the \code{map_master_aes} class
#' @export
is.map_master_aes <- function(x) inherits(x, "map_master_aes")

#' @title Test if the object is of class \code{map_colors}
#' @author Manuel Teodoro
#' @description This function returns ‘TRUE’ for \code{map_colors} objects and ‘FALSE’ for all other objects.
#' 
#' @param x An object
#' @return 'TRUE' if the object inhertis from the \code{map_colors} class
#' @export 
is.map_colors <- function(x) inherits(x, "map_colors")

#' @title Test if the object is of class \code{map_sizes}
#' @author Manuel Teodoro
#' @description This function returns ‘TRUE’ for \code{map_sizes} objects and ‘FALSE’ for all other objects.
#' 
#' @param x An object
#' @return 'TRUE' if the object inhertis from the \code{map_sizes} class
#' @export
is.map_sizes <- function(x) inherits(x, "map_sizes")

#' @title Test if the object is of class \code{map_legends}
#' @author Manuel Teodoro
#' @description This function returns ‘TRUE’ for \code{map_legends} objects and ‘FALSE’ for all other objects.
#' 
#' @param x An object
#' @return 'TRUE' if the object inhertis from the \code{map_legends} class
#' @export
is.map_legends <- function(x) inherits(x, "map_legends")



## --- METHODS ---

#' @export
print.map_aesthetics <- function(x, ...) {
  require(glue)
  stopifnot(is.map_aesthetics(x))
  print(glue("{class(x)[2]}"))
  for (argg in names(x)) {
    print(glue("{argg} : {x[argg]}"))
  }
  invisible(x)
}


#' @title With default map aesthetics
#' @author Manuel Teodoro
#' @description Modifies the default aesthetics.
#' 
#' @param colors A list containing the parameters of the colors to be changed as passed in the function \code{\link{define_map_colors}}.
#' @param sizes A list containing the parameters of the sizes to be changed as passed in the function \code{\link{define_map_sizes}}.
#' @param legends A list containing the textfor the legends to be changed, as passed in the function \code{\link{define_map_legends}}.
#' 
#' @details The function modifies the global objects containing the default aesthetics.
#'
#' @seealso \code{\link{define_map_colors}} for details about the colors, \code{\link{define_map_sizes}} for details about the sizes and \code{\link{define_map_legends}} for details about the legends.
#' @export
#' @examples
#' my_aesthetics <- with_default_aesthetics(list(dots_orgs = "#000000"), list(text_city = 12))
with_default_aesthetics <- function(colors = NA, sizes = NA, legends = NA) {
  require(purrr)

  modified_map_colors <- default_map_colors
  modified_sizes_in_map <- default_sizes_in_map
  modified_legends_specs <- default_legends_specs
  
  if (!is.na(colors)) modified_map_colors <- list_modify(modified_map_colors, !!!colors)
  if (!is.na(sizes)) modified_sizes_in_map <- list_modify(modified_sizes_in_map, !!!sizes)
  if (!is.na(legends)) modified_legends_specs <- list_modify(modified_legends_specs, !!!legends)
  modified_aesthetics <- define_map_aesthetics("Modified maps aesthetics from defaults",
                                               modified_map_colors,
                                               modified_sizes_in_map,
                                               modified_legends_specs)
  return(modified_aesthetics)
}



## --- DEFAULTS ---
default_map_colors <- define_map_colors(dots_orgs = "#574166",
                                        target_country = "#8caeb4",
                                        empty_countries = "#f3f3f3",
                                        border_countries = "#9c9c9c",
                                        oceans = "#4e91d2",
                                        text_cities = "#a0a0a0",
                                        text_legend = "#283151",
                                        text_copyright = "#f3f3f3")

default_legends_specs <- define_legends_specs("Year", "Organizations")

default_sizes_in_map <- define_sizes_in_map(2.5, 4)

default_aesthetics <- define_map_aesthetics("Default maps aesthetics",
                                            default_map_colors,
                                            default_sizes_in_map,
                                            default_legends_specs)
