#' @title Mapic city Names
#' @description Generates labels to show city names.
#' 
#' @param .df Object of class \code{data.frame} containing the data to be mapped
#' (including coordinates). Preferably as prepared by \code{api_to_db}.
#' See \code{column_names} below for a list of required fields.
#' @param list_cities A vector of characters with the names of the cities to be labelled.
#' @param lat Name of the column containing the lattitude. Not necessary if an object of
#' class \code{mapicHolder} is used.
#' @param lon Name of the column containing the longitude. Not necessary if an object of
#' class \code{mapicHolder} is used.
#' @param cities Name of the column containing the city names. Not necessary if an object
#' of class \code{mapicHolder} is used.
#' @param font_family Font to be used.
#' @param text_size Size of the text in the plot.
#' @param map_colors An object of class \code{map_colors} containing the details of the
#' colors for the maps.
#' Not necessary if an object of class \code{mapicHolder} is used.
#' 
#' @return If the map is created using an object of class \code{mapicHolder}, it returns
#' the same object with added information and map elements. Otherwise, it returns a
#' \code{ggplot} object.
#' 
#' @export
mapic_city_names <- function(x, ...) UseMethod("mapic_city_names")


#' @method mapic_city_names default
#' @describeIn mapic_city_names Default
#' @export
mapic_city_names.default <- function(.df,
                                     list_cities,
                                     lat = "lat",
                                     lon = "lon",
                                     cities = "city",
                                     font_family = "Arial",
                                     text_size = 4,
                                     map_colors = default_map_colors) {
  list_cities <- purrr::map_chr(list_cities, str_to_title)
  coord_cities <-.df %>%
    mutate(city_name = str_to_title(!!ensym(cities))) %>%
    group_by(city_name) %>%
    summarise(x = median(!!ensym(lon), na.rm = T),
              y = median(!!ensym(lat), na.rm = T),
              n = n()) %>%
    filter(city_name %in% list_cities)

  list_labels <- list(geom_text(
    data = coord_cities,
    aes(x, y, label = city_name),
    family = font_family,
    fontface = 'bold',
    size = text_size,
    color = map_colors$text_cities))

  return(list_labels)
}


#' @method mapic_city_names mapicHolder
#' @describeIn mapic_city_names mapicHolder
#' @export
mapic_city_names.mapicHolder <- function(.mapic_holder,
                                         list_cities,
                                         font_family = "Arial",
                                         text_size = 4) {
  data <- .mapic_holder$data$base

  city_labels <- mapic_city_names(.df = data,
                                  list_cities = list_cities,
                                  lat = "lat",
                                  lon = "lon",
                                  cities = "city",
                                  font_family = font_family,
                                  text_size = text_size,
                                  map_colors = .mapic_holder$colors)

  .mapic_holder[["mapic"]] <- .mapic_holder[["mapic"]] + city_labels
  .mapic_holder[["mapic_city_labels"]] <- city_labels
  return(.mapic_holder)
}
