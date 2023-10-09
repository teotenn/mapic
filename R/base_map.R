#' @title Base map
#' @description Creates the base map of the country
#'
#' @param country A string. Name of the country, as defined in the package \code{maps}.
#' @param x_limits A vector of size 2 containing the horizontal limits of the map.
#' @param y_limits A vector of size 2 containing the vertical limits of the map.
#' @param show_coords If \code{TRUE}, the coordinates are shown in the map. It is particularly
#' useful to set the x and y limits.
#' @param return_mapic_obj If \code{TRUE} (default), it returns an object of class \code{mapicHolder}.
#' Otherwise it returns a \code{ggplot} object.
#' @param map_colors Object of class \code{map_colors} containing the values for the colors
#' to use for the creation of the map. If not provided, it will use the \code{default_map_colors}.
#'
#' @return The function returns the map, either as a \code{mapicHolder} object or as a \code{ggplot}
#' (depending on \code{return_mapic_obj} flag).
#' If the object is of class \code{ggplot}, it can be used with other \code{ggplot} family of functions.
#' If the object is of class \code{mapicHolder} it contains embeded information to support
#' further steps in the mapping process.
#' The \code{ggplot} object is included in the \code{$mapic} element of the object.
#'
#' @details Creates the base map of a country.
#'
#' @export
#' @examples
#' base_map("Brazil")
#'
#' base_map("Mexico",
#'          x_limits = c(-118, -86),
#'          y_limits = c(14, 34),
#'          return_mapic_obj = FALSE,
#'          show_coords = T) +
#'   scale_x_continuous(n.breaks = 20) +
#'   ggtitle("A map of Mexico")
base_map <- function(country,
                     x_limits = NULL,
                     y_limits = NULL,
                     show_coords = FALSE,
                     return_mapic_obj = TRUE,
                     map_colors = default_map_colors) {
  require(maps)
  require(ggplot2)

  ## Verifying the arguments passed to the function
  if (length(country) != 1) stop("Function supports only one country per map")
  stopifnot(is.logical(show_coords))
  stopifnot("Name of the country should be character" = is.character(country))

  if (!country %in% map_data("world")$region) {
    stop(paste("Country name not recognized",
               "To see a list of recognized countries run",
               "<unique(maps::map_data('world')$region)>", sep = "\n"))
  }

  ## Allow user observe coords for reference
  if (missing(x_limits) || missing(y_limits)) {
    warning("X and/or Y limits not provided.\nPrinting worldwide map.")
    map_country_theme <- theme(panel.background = element_rect(fill = map_colors$oceans))
  } else if (show_coords) {
    map_country_theme <- theme(panel.background = element_rect(fill = map_colors$oceans))
  } else {
    if (length(x_limits) != 2 || length(y_limits) != 2 ||
         !all(grepl("^-?[0-9.]+$", c(x_limits, y_limits)))) {
      stop("Limits for X and Y coords should be provided as vectors with two numeric values")
    } else {

      ## Custom theme for the final map
      map_country_theme <- theme_bw() +
        theme(panel.background = element_rect(fill = map_colors$oceans),
              legend.position = "none",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black"),
              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank())
    }
  }

  ## Get the target cpuntry data
  map_data_country <- map_data("world")[map_data("world")$region == country, ]

  ## The map
  mapic <- ggplot() +
    ## First layer: worldwide map
    geom_polygon(data = map_data("world"),
                 aes(x = long, y = lat, group = group),
                 color = map_colors$border_countries,
                 fill = map_colors$empty_countries) +
    ## Second layer: Country map
    geom_polygon(data = map_data_country,
                 aes(x = long, y = lat, group = group),
                 color = map_colors$border_countries,
                 fill = map_colors$target_country) +
    coord_map() +
    coord_fixed(1.3,
                xlim = x_limits,
                ylim = y_limits) +
    map_country_theme

  if (return_mapic_obj) {
    map_pointer <- structure(
      list(
        mapic = mapic,
        base_map = mapic,
        x_limits = x_limits,
        y_limits = y_limits,
        colors = map_colors
      ),
      class = "mapicHolder")
    return(map_pointer)
  } else {
    return(mapic)
  }
}
