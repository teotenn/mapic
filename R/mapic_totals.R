#' @title Mapic totals generic
#' @author Manuel Teodoro
#' @description Generates the label of the total elements included in the map,
#' either as internal or external label, depending which function is being called.
#'
#' @param .mapic_holder Object of class \code{mapicHolder} as returned from the function
#' \code{base_map} and \code{mapic_city_dots} (optional).
#' @param totals The value to be included, as \code{numeric}.
#' Not necessary if an object of class \code{mapicHolder} is passed.
#' @param x_limits A vector of size 2 containing the horizontal limits of the map.
#' Not necessary for external or if an object of class \code{mapicHolder} is passed.
#' @param y_limits A vector of size 2 containing the vertical limits of the map.
#' Not necessary for external or if an object of class \code{mapicHolder} is passed.
#' @param totals_label A string to be used for the label "Totals".
#' @param map_colors An object of class \code{map_colors} containing the details of the colors for the maps.
#' Not necessary if an object of class \code{mapicHolder} is passed.
#'
#' @return If the map is created using an object of class \code{mapicHolder}, it returns the same object with added
#' information and map elements. Otherwise, it returns a \code{ggplot} object.
#'
#' @details It generates the labels to show the total values mapped.
#' The internal version currently plots only on the lower-left corner of the map.
#' The external version generates a sepparated plot that has to be called sepparately.
#'
#' @export
#'
mapic_totals_internal <- function(x, ...) UseMethod("mapic_totals_internal")

#' @method mapic_totals_internal default
#' @describeIn mapic_totals_internal Default
#' @export
mapic_totals_internal.default <- function(totals,
                                          x_limits,
                                          y_limits,
                                          totals_label = "Totals",
                                          map_colors = default_map_colors) {
  require(ggplot2)

  ## POSITION FOR THE LABELS
  ## Starting points
  x_units <- abs(x_limits[1] - x_limits[2]) / 10
  y_units <- abs(y_limits[1] - y_limits[2]) / 10
  start_x <- min(x_limits) + x_units
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

    ptotals <- list(
        geom_rect(aes(xmin = rectangle_start_x, xmax = rectangle_wide,
                      ymin = rectangle_start_y, ymax = rectangle_high),
                  color = "#283151",
                  fill = map_colors$background_legend,
                  alpha = 9 / 10),
        geom_text(
            aes(x = num_position_x, y = num_position_y,
                label = totals),
            size = num_size,
            fontface = "bold",
            alpha = 9 / 10,
            color = map_colors$text_legend),
        geom_text(
            aes(x = text_position_x, y = text_position_y,
                label = totals_label),
            size = text_size,
            fontface = "bold",
            alpha = 9 / 10,
            color = map_colors$text_legend)
    )
    return(ptotals)
}

#' @method mapic_totals_internal mapicHolder
#' @describeIn mapic_totals_internal mapicHolder
#' @export
mapic_totals_internal.mapicHolder <- function(.mapic_holder,
                                              totals_label = "Totals") {
  data_totals <- sum(.mapic_holder$data$map$n)
  mapic_totals <- mapic_totals_internal(totals = data_totals,
                                        x_limits = .mapic_holder$x_limits,
                                        y_limits = .mapic_holder$y_limits,
                                        totals_label = totals_label,
                                        map_colors = .mapic_holder$colors)

  .mapic_holder[["mapic_totals"]] <- mapic_totals
  .mapic_holder[["totals"]] <- data_totals
  .mapic_holder[["mapic"]] <- .mapic_holder[["mapic"]] + mapic_totals
  return(.mapic_holder)
}


#' @rdname mapic_totals_internal
#' @export
mapic_totals_external <- function(x, ...) UseMethod("mapic_totals_external")

#' @method mapic_totals_external default
#' @rdname mapic_totals_internal
#' @export
mapic_totals_external.default <- function(totals,
                                          totals_label = "Totals",
                                          map_colors = default_map_colors) {
  require(ggplot2)

  ## Constant values for a good visualization
  rectangle_wide <- 4
  rectangle_high <- 9.5
  num_position_x <- 2
  num_position_y <- 3.6
  num_size <- 20
  text_position_x <- 2
  text_position_y <- 7.6
  text_size <- 7.5

  ptotals <- ggplot() +
    geom_rect(
      aes(xmin = 0, xmax = rectangle_wide,
          ymin = 0, ymax = rectangle_high),
      color = "#283151",
      fill = map_colors$background_legend,
      alpha = 9 / 10) +
    geom_text(
      aes(
        x = num_position_x,
        y = num_position_y,
        label = totals),
      size = num_size,
      family = "Montserrat",
      fontface = "bold",
      alpha = 9 / 10,
      color = map_colors$text_legend) +
    geom_text(
      aes(
        x = text_position_x,
        y = text_position_y,
        label = "Organizations"),
      size = text_size,
      family = "Montserrat",
      fontface = "bold",
      alpha = 9 / 10,
      color = map_colors$text_legend) +
    theme_bw() 

  return(ptotals)
}

#' @method mapic_totals_external mapicHolder
#' @rdname mapic_totals_internal
#' @export
mapic_totals_external.mapicHolder <- function(.mapic_holder,
                                              totals_label = "Totals") {
  data_totals <- sum(.mapic_holder$data$map$n)
  mapic_totals <- mapic_totals_external(totals = data_totals,
                                        totals_label = totals_label,
                                        map_colors = .mapic_holder$colors)

  .mapic_holder[["mapic_totals"]] <- mapic_totals + .mapic_holder[["theme_labels"]]
  .mapic_holder[["totals"]] <- data_totals

  return(.mapic_holder)
}
