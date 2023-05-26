#' @title Mapic City Dots
#' @author Manuel Teodoro
#' @description Generates the plot of dots per city with its size based on amount of organizations.
#'
#' @param .mapic_holder Object of class \code{mapicHolder} as returned from the function \code{base_map} (optional).
#' @param .df Object of class \code{data.frame} containing the data to be included in the map. Preferably as return from the function \code{combine_csv_sql}. See \code{column_names} for a list of required fields.
#' @param year The year to be plot, as \code{numeric}.
#' @param column_names A \code{list} with the names of the columns containing the values to be used for the map.
#' \enumerate{
#' \item lat : Column containing the latitude.
#' \item lon : Column containing the longitude.
#' \item cities : Column containing the names of the cities.
#' \item start_year : Column containing the starting year.
#' \item end_year : Optional. Column containing the ending year.
#' }
#' @param dot_size Default 1. Proportional sizes of the dots.
#' @param map_colors An object of class \code{map_colors} containing the details of the colors for the maps.
#' Not necessary if an object of class \code{mapicHolder} is passed.
#'
#' @return If the map is creating using an object of class \code{mapicHolder}, it returns the same object with added
#' information and map elements. Otherwise, it returns a \code{ggplot} object.
#'
#' @details It generates the dots to be added to the base map, showing the ammount of elements per city in a given year.
#'
#' @export
#'
mapic_city_dots <- function(x, ...) UseMethod("mapic_city_dots")

mapic_city_dots.default <- function(.df,
                                     year,
                                     column_names = list(
                                       lat = "lat",
                                       lon = "lon",
                                       cities = "city",
                                       start_year = "year",
                                       end_year = NULL),
                                     dot_size = 1,
                                     map_colors = default_map_colors) {
  require(dplyr)
  require(tidyr)
  require(stringr)
  require(ggplot2)

  mandatory_cols <- c("lat", "lon", "cities", "start_year")
  if (!all(mandatory_cols %in% names(column_names))) {
    stop("Column names missing!")
  } else {
    if (!"end_year" %in% names(column_names)) {
      .df$final_year <- NA_real_
      column_names[["end_year"]] <- "final_year"
    }
  }

  ## Dots base size
  base_size <- 5
  dot_sizes <- c(0.5 * (base_size * dot_size),
                 1 * (base_size * dot_size),
                 2 * (base_size * dot_size),
                 3 * (base_size * dot_size),
                 4 * (base_size * dot_size),
                 5 * (base_size * dot_size),
                 7 * (base_size * dot_size),
                 8 * (base_size * dot_size),
                 9 * (base_size * dot_size))

  ## Main data to plot
  filt <- .df  %>%
    mutate(year_final = replace_na(!!sym(column_names$end_year), year + 1),
           city_name = str_to_sentence(!!sym(column_names$cities))) %>%
    filter(year_final > year & !!sym(column_names$start_year) <= year) %>%
    group_by(city_name) %>%
    summarise(x = median(!!sym(column_names$lon), na.rm = TRUE),
              y = median(!!sym(column_names$lat), na.rm = TRUE),
              n = n()) %>%
    mutate(dot_size = case_when(n == 1 ~ dot_sizes[1],
                                n >= 2 & n <= 5 ~ dot_sizes[2],
                                n >= 6 & n <= 10 ~ dot_sizes[3],
                                n >= 11 & n <= 30 ~ dot_sizes[4],
                                n >= 31 & n <= 50 ~ dot_sizes[5],
                                n >= 51 & n <= 100 ~ dot_sizes[6],
                                n >= 101 & n <= 200 ~ dot_sizes[7],
                                n >= 201 & n <= 300 ~ dot_sizes[8],
                                n >= 301 ~ dot_sizes[9],
                                TRUE ~ NA))

  ##################### MAIN MAP #######################
  mapic_points <- list(
    geom_point(data = filt,
               aes(x, y, size = dot_size),
               color = map_colors$dots_orgs,
               alpha = 7 / 10,
               shape = 19) ,
    scale_size_identity("",
                        breaks = dot_sizes,
                        labels = c("1", "2-5", "6-10", "11-30", "31-50",
                                   "51-100", "101-200", "201-300", ">300"),
                        guide = guide_legend(label.position = "bottom",
                                             label.vjust = 0,
                                             nrow = 1)),
    geom_point(data = filter(filt, n == 1),
               aes(x, y),
               color = map_colors$dots_orgs,
               shape = 19,
               size = 2.5) ,
    geom_point(data = filter(filt, n >= 2 & n <= 5),
               aes(x, y),
               color = map_colors$dots_orgs,
               shape = 19,
               size = 5),
    theme(legend.position = "bottom")
  )

  return(mapic_points)
}

mapic_city_dots.mapicHolder <- function(.mapic_holder,
                                        .df,
                                        year,
                                        column_names = list(
                                          lat = "lat",
                                          lon = "lon",
                                          cities = "city",
                                          start_year = "year",
                                          end_year = NULL),
                                        dot_size = 1) {
  require(dplyr)
  require(tidyr)
  require(stringr)

  mandatory_cols <- c("lat", "lon", "cities", "start_year")
  if (!all(mandatory_cols %in% names(column_names))) {
    stop("Column names missing!")
  } else {
    if (!"end_year" %in% names(column_names)) {
      .df$final_year <- NA_real_
      column_names[["end_year"]] <- "final_year"
    }
  }

  mapic_dots <- mapic_city_dots(.df = .df,
                                year = year,
                                column_names = column_names,
                                dot_size = dot_size,
                                map_colors = .mapic_holder$colors)

  data_for_map <- .df  %>%
    mutate(year_final = replace_na(!!sym(column_names$end_year), year + 1),
           city_name = str_to_sentence(!!sym(column_names$cities))) %>%
    filter(year_final > year & !!sym(column_names$start_year) <= year) %>%
    group_by(city_name) %>%
    summarise(x = median(!!sym(column_names$lon), na.rm = TRUE),
              y = median(!!sym(column_names$lat), na.rm = TRUE),
              n = n())

  .mapic_holder[["mapic_dots"]] <- mapic_dots
  .mapic_holder[["year"]] <- year
  .mapic_holder[["data"]] <- list(base = .df, map = data_for_map)
  .mapic_holder[["mapic"]] <- .mapic_holder[["mapic"]] + mapic_dots
  return(.mapic_holder)
}
