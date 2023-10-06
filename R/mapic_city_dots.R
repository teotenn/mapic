#' @title Mapic City Dots
#'
#' @description Generates the plot of dots per city with its size based on amount of organizations.
#'
#' @param .mapic_holder Object of class \code{mapicHolder} as returned from the function \code{base_map} (optional).
#' @param .df Object of class \code{data.frame} containing the data to be included in the map.
#' Preferably as return from the function \code{combine_csv_sql}. See \code{column_names} for a
#' list of required fields.
#' @param year The year to be plot, as \code{numeric}.
#' @param column_names A \code{list} with the names of the columns containing the values to be used for the map.
#' \enumerate{
#' \item lat : Column containing the latitude.
#' \item lon : Column containing the longitude.
#' \item cities : Column containing the names of the cities.
#' \item start_year : Column containing the starting year.
#' \item end_year : Optional. Column containing the ending year.
#' }
#' @param legend_position Overwrites ggplot's \code{theme(legend.position)}. See theme's help for details.
#' @param legend_external Boolean. If an object of class \code{mapicHolder} is used, the legends can be
#' extracted from the map and passed as an element of the object, as \code{object$legend}.
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
mapic_city_dots <- function(x, ...) UseMethod("mapic_city_dots")

#' @method mapic_city_dots default
#' @describeIn mapic_city_dots Default
#' @export
mapic_city_dots.default <- function(.df,
                                    year,
                                    column_names = list(
                                      lat = "lat",
                                      lon = "lon",
                                      cities = "city",
                                      start_year = "year",
                                      end_year = NULL),
                                    legend_position = "bottom",
                                    dot_size = 1,
                                    map_colors = default_map_colors) {
  require(dplyr)
  require(tidyr)
  require(stringr)
  require(ggplot2)

  column_names <- column_names[lengths(column_names) != 0]
  year__ <- year

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
    mutate_at(vars(column_names$end_year), ~replace_na(., year__ + 1)) %>%
    mutate(year_final = !!sym(column_names$end_year),
           city_name = str_to_sentence(!!sym(column_names$cities))) %>%
    filter(year_final > year__ & !!sym(column_names$start_year) <= year__) %>%
    group_by(city_name) %>%
    summarise(x = mean(!!sym(column_names$lon), na.rm = TRUE),
              y = mean(!!sym(column_names$lat), na.rm = TRUE),
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

  ## Main map
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
    theme(legend.position = legend_position)
  )

  return(mapic_points)
}

#' @method mapic_city_dots mapicHolder
#' @describeIn mapic_city_dots Default
#' @export
mapic_city_dots.mapicHolder <- function(.mapic_holder,
                                        .df,
                                        year,
                                        column_names = list(
                                          lat = "lat",
                                          lon = "lon",
                                          cities = "City",
                                          start_year = "Year_start",
                                          end_year = "Year_end"),
                                        legend_external = TRUE,
                                        legend_position = "bottom",
                                        dot_size = 1) {
  require(dplyr)
  require(tidyr)
  require(stringr)

  column_names <- column_names[lengths(column_names) != 0]
  year__ <- year

  ## Check required fields
  mandatory_cols <- c("lat", "lon", "cities", "start_year")
  if (!all(mandatory_cols %in% names(column_names))) {
    stop("Column names missing!")
  } else {
    if (!"end_year" %in% names(column_names)) {
      .df$final_year <- NA_real_
      column_names[["end_year"]] <- "final_year"
    }
  }

  ## Make map using default method
  if (legend_external) {
    mapic_dots <- mapic_city_dots(.df = .df,
                                year = year__,
                                column_names = column_names,
                                legend_position = "none",
                                dot_size = dot_size,
                                map_colors = .mapic_holder$colors)
    } else {
      mapic_dots <- mapic_city_dots(.df = .df,
                                    year = year__,
                                    column_names = column_names,
                                    legend_position = legend_position,
                                    dot_size = dot_size,
                                    map_colors = .mapic_holder$colors)
    }

  ## Papere the data
  data_for_map <- .df  %>%
    mutate_at(vars(column_names$end_year), ~replace_na(., year__ + 1)) %>%
    mutate(year_final = !!sym(column_names$end_year),
           city_name = str_to_sentence(!!sym(column_names$cities))) %>%
    filter(year_final > year__ & !!sym(column_names$start_year) <= year__) %>%
    group_by(city_name) %>%
    summarise(x = median(!!sym(column_names$lon), na.rm = TRUE),
              y = median(!!sym(column_names$lat), na.rm = TRUE),
              n = n())

  ## Empty theme for labels
  empty_theme <- theme_bw() +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "white"),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin = unit(c(-0, -0, -0, -0), "cm"))

  ## legend_external
  if (legend_external) {
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

    external_legends <- data.frame(
      n = c(1, 2, 6, 11, 31, 51, 101, 201, 301),
      x = c(1, 1.08, 1.18, 1.32, 1.48, 1.68, 1.92, 2.22, 2.55),
      labs = c("1", "2-5", "6-10", "11-30",
               "31-50", "51-100", "101-200",
               "201-300", ">300"),
      y = 1) %>%
      mutate(dot_size = case_when(n == 1 ~ dot_sizes[1],
                                  n == 2 ~ dot_sizes[2],
                                  n == 6 ~ dot_sizes[3],
                                  n == 11 ~ dot_sizes[4],
                                  n == 31 ~ dot_sizes[5],
                                  n == 51 ~ dot_sizes[6],
                                  n == 101 ~ dot_sizes[7],
                                  n == 201 ~ dot_sizes[8],
                                  n == 301 ~ dot_sizes[9])) %>%
    ggplot(aes(x, y)) +
      geom_point(
        aes(size = dot_size),
        color = .mapic_holder$colors$dots_orgs,
        alpha = 7 / 10,
        shape = 19) +
      scale_size_identity(
        "",
        breaks = c(2.5, 5, 10, 15, 20, 25, 35, 40, 45),
        labels = c("1", "2-5", "6-10", "11-30",
                   "31-50", "51-100", "101-200",
                   "201-300", ">300"),
        guide = guide_legend(label.position = "bottom",
                             label.vjust = 0,
                             nrow = 1)) +
      geom_text(
        aes(label = labs),
        hjust = 0.5,
        vjust = 7.5,
        family = "Montserrat") +
    xlim(1, 2.65) +
      empty_theme

    .mapic_holder[["legend"]] <- external_legends
  }

  .mapic_holder[["theme_labels"]] <- empty_theme
  .mapic_holder[["mapic_dots"]] <- mapic_dots
  .mapic_holder[["year"]] <- year__
  .mapic_holder[["data"]] <- list(base = .df, map = data_for_map)
  .mapic_holder[["mapic"]] <- .mapic_holder[["mapic"]] + mapic_dots
  return(.mapic_holder)
}
