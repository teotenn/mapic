#' @title Mapic year
#' @author Manuel Teodoro
#' @description Generates the label of the year to be map, either as internal or external,
#' depending which function is being called.
#'
#' @param .mapic_holder Object of class \code{mapicHolder} as returned from the function
#' \code{base_map} and \code{mapic_city_dots} (optional).
#' @param year The year to be plot, as \code{numeric}.
#' Not necessary if an object of class \code{mapicHolder} is passed.
#' @param x_limits A vector of size 2 containing the horizontal limits of the map.
#' Not necessary for external or if an object of class \code{mapicHolder} is passed.
#' @param y_limits A vector of size 2 containing the vertical limits of the map.
#' Not necessary for external or if an object of class \code{mapicHolder} is passed.
#' @param year_label A string to be used for the label "Year".
#' @param map_colors An object of class \code{map_colors} containing the details of the colors for the maps.
#' Not necessary if an object of class \code{mapicHolder} is passed.
#'
#' @return If the map is creating using an object of class \code{mapicHolder}, it returns the same object with added
#' information and map elements. Otherwise, it returns a \code{ggplot} object.
#'
#' @details It generates the labels to show which years is being mapped.
#' The internal version currently plots only on the lower-left corner of the map.
#' The external version generates a sepparated plot that has to be called sepparately.
#'
#' @export
#' @examples
mapic_year_internal <- function(x, ...) UseMethod("mapic_year_internal")

mapic_year_internal.default <- function(year,
                                        x_limits,
                                        y_limits,
                                        year_label = "Year",
                                        map_colors = default_map_colors) {
  require(ggplot2)

  ## POSITION FOR THE LABELS
  ## Starting points
  x_units <- abs(x_limits[1] - x_limits[2]) / 10
  y_units <- abs(y_limits[1] - y_limits[2]) / 10
  start_x <- min(x_limits)
  start_y <- min(y_limits)
  ## Frame
  rectangle_start_x <- start_x
  rectangle_wide <- rectangle_start_x + x_units
  rectangle_start_y <- start_y
  rectangle_high <- rectangle_start_y + y_units
  ## Text
  num_size <- 4
  text_size <- 3
  num_position_x <- start_x + (x_units * 0.5)
  text_position_x <- start_x + (x_units * 0.5)
  num_position_y <- start_y + (y_units * 0.25)
  text_position_y <- start_y + (y_units * 0.65)

  pyears <- list(
    geom_rect(
      aes(xmin = rectangle_start_x, xmax = rectangle_wide,
          ymin = rectangle_start_y, ymax = rectangle_high),
      color = map_colors$text_legend,
      fill = map_colors$text_legend,
      alpha = 9 / 10),
    geom_text(
      aes(x = num_position_x,
          y = num_position_y,
          label = year),
      size = num_size,
      fontface = "bold",
      color = map_colors$background_legend),
    geom_text(
      aes(x = text_position_x,
          y = text_position_y,
          label = year_label),
      size = text_size,
      fontface = "bold",
      alpha = 9 / 10,
      color = map_colors$background_legend)
  )
  return(pyears)
}

mapic_year_internal.mapicHolder <- function(.mapic_holder,
                                            year_label = "Year") {
  mapic_year <- mapic_year_internal(year = .mapic_holder$year,
                                    x_limits = .mapic_holder$x_limits,
                                    y_limits = .mapic_holder$y_limits,
                                    year_label = year_label,
                                    map_colors = .mapic_holder$colors)

  .mapic_holder[["mapic_year"]] <- mapic_year
  .mapic_holder[["mapic"]] <- .mapic_holder[["mapic"]] + mapic_year
  return(.mapic_holder)
}
