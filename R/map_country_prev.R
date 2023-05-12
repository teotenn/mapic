#' @title Base map
#' @author Manuel Teodoro
#' @description Creates the base map of the country
#'
#' @param country A string. Name of the country, as defined in the package \code{maps}.
#' @param x_limits A vector of size 2 containing the horizontal limits of the map.
#' @param y_limits A vector of size 2 containing the vertical limits of the map.
#' @param show_coords If \code{TRUE}, the coordinates are shown in the map. It is particularly
#' useful to set the x and y limits.
#' @param map_colors Object of class \code{map_colors} containing the values for the colors
#' to use for the creation of the map.
#'
#' @return The map.
#'
#' @details Plots the map.
#'
#' @export
#' @examples
#' map_country_prev("Brazil")
base_map <- function(country,
                     x_limits = NULL,
                     y_limits = NULL,
                     show_coords = FALSE,
                     map_colors = default_map_colors) {
  require(maps)
  require(ggplot2)

  ## Verifying the arguments passed to the function
  if (length(country) != 1) stop("Function supports only one country per map")
  stopifnot(is.logical(show_coords))
  stopifnot("Name of the country should be character" = is.character(country))

  if (!country %in% map_data('world')$region) {
    stop(paste("Country name not recognized",
               "To see a list of recognized countries run",
               "<unique(maps::map_data('world')$region)>", sep = "\n"))
  }

  ## If coords limits missing, print worldwide map with coordinates system to allow
  ## User observe coords for reference
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

      ## All the received inputs are correct.
      ## Let's define our custom theme for the final map
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

  ## make a df with only the country to overlap
  map_data_country <- map_data('world')[map_data('world')$region == country, ]
  ## The map (maps + ggplot2 )
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

  map_pointer <- structure(
    list(
      mapic = mapic,
      base_map = mapic,
      x_limits = x_limits,
      y_limits = y_limits,
      colors = map_colors
    ),
    class = "mPointer")
  return(map_pointer)
}


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
    summarise(x = median(!!sym(column_names$lon), na.rm = T),
              y = median(!!sym(column_names$lat), na.rm = T),
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
               alpha = 7/10,
               shape = 19) ,
    scale_size_identity('',
                        breaks = dot_sizes, 
                        labels = c('1', '2-5', '6-10', '11-30', '31-50',
                                   '51-100', '101-200', '201-300', '>300'),
                        guide = guide_legend(label.position = 'bottom',
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
    theme(legend.position = 'bottom')
  )

  return(mapic_points)
}

mapic_city_dots.mPointer <- function(.mapic_pointer,
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
                                map_colors = .mapic_pointer$colors)

  data_for_map <- .df  %>%
    mutate(year_final = replace_na(!!sym(column_names$end_year), year + 1),
           city_name = str_to_sentence(!!sym(column_names$cities))) %>%
    filter(year_final > year & !!sym(column_names$start_year) <= year) %>%
    group_by(city_name) %>%
    summarise(x = median(!!sym(column_names$lon), na.rm = T),
              y = median(!!sym(column_names$lat), na.rm = T),
              n = n())

  .mapic_pointer[["mapic_dots"]] <- mapic_dots
  .mapic_pointer[["year"]] <- year
  .mapic_pointer[["data"]] <- list(base = .df, map = data_for_map) 
  .mapic_pointer[["mapic"]] <- .mapic_pointer[["mapic"]] + mapic_dots
  return(.mapic_pointer)
}

## Make the years
mapic_year_internal <- function(x, ...) UseMethod("mapic_year_internal")

mapic_year_internal.default <- function(year,
                                        x_limits,
                                        y_limits,
                                        year_label = "Year",
                                        map_colors = default_map_colors) {

  ## POSITION FOR THE LABELS
  ## Starting points
  x_units <- abs(x_limits[1] - x_limits[2])/10
  y_units <- abs(y_limits[1] - y_limits[2])/10
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
  num_position_x <- start_x + (x_units*0.5)
  text_position_x <- start_x + (x_units*0.5)
  num_position_y <- start_y + (y_units*0.25)
  text_position_y <- start_y + (y_units*0.65)
   
  pyears <- list(
    geom_rect(
      aes(xmin = rectangle_start_x, xmax = rectangle_wide,
          ymin = rectangle_start_y, ymax = rectangle_high),
      color = map_colors$text_legend,
      fill = map_colors$text_legend,
      alpha = 9/10),
    geom_text(
      aes(x = num_position_x,
          y = num_position_y,
          label = year),
      size = num_size,
      fontface = 'bold',
      color = map_colors$background_legend),
    geom_text(
      aes(x = text_position_x,
          y = text_position_y,
          label = year_label),
      size = text_size,
      fontface = 'bold',
      alpha = 9/10,
      color = map_colors$background_legend)
  )
  return(pyears)
}

mapic_year_internal.mPointer <- function(.mapic_pointer,
                                         year_label = "Year") {
  mapic_year <- mapic_year_internal(year = .mapic_pointer$year,
                                    x_limits = .mapic_pointer$x_limits,
                                    y_limits = .mapic_pointer$y_limits,
                                    year_label = year_label,
                                    map_colors = .mapic_pointer$colors)

  .mapic_pointer[["mapic_year"]] <- mapic_year
  .mapic_pointer[["mapic"]] <- .mapic_pointer[["mapic"]] + mapic_year
  return(.mapic_pointer)
}


## TOTALS
mapic_totals_internal <- function(x, ...) UseMethod("mapic_totals_internal")

mapic_totals_internal.default <- function(totals,
                                          x_limits,
                                          y_limits,
                                          totals_label = "Totals",
                                          map_colors = default_map_colors) {
  ## POSITION FOR THE LABELS
  ## Starting points
  x_units <- abs(x_limits[1] - x_limits[2])/10
  y_units <- abs(y_limits[1] - y_limits[2])/10
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
  num_position_x <- start_x + (x_units*0.5)
  text_position_x <- start_x + (x_units*0.5)
  num_position_y <- start_y + (y_units*0.25)
  text_position_y <- start_y + (y_units*0.65)
  
    ptotals <- list(
        geom_rect(aes(xmin = rectangle_start_x, xmax = rectangle_wide,
                      ymin = rectangle_start_y, ymax = rectangle_high),
                  color = '#283151',
                  fill = map_colors$background_legend,
                  alpha = 9/10),
        geom_text(
            aes(x = num_position_x, y = num_position_y,
                label = totals),
            size = num_size,
            fontface = 'bold',
            alpha = 9/10,
            color = map_colors$text_legend),
        geom_text(
            aes(x = text_position_x, y = text_position_y,
                label = totals_label),
            size = text_size,
            fontface = 'bold',
            alpha = 9/10,
            color = map_colors$text_legend)
    )
    return(ptotals)
}

mapic_totals_internal.mPointer <- function(.mapic_pointer,
                                           totals,
                                           x_limits,
                                           y_limits,
                                           totals_label = "Totals",
                                           map_colors = default_map_colors) {
  data_totals <- sum(mx$data$map$n)
  mapic_totals <- mapic_totals_internal(totals = data_totals,
                                        x_limits = .mapic_pointer$x_limits,
                                        y_limits = .mapic_pointer$y_limits,
                                        totals_label = totals_label,
                                        map_colors = .mapic_pointer$colors)

  .mapic_pointer[["mapic_totals"]] <- mapic_totals
  .mapic_pointer[["totals"]] <- data_totals
  .mapic_pointer[["mapic"]] <- .mapic_pointer[["mapic"]] + mapic_totals
  return(.mapic_pointer)
}

## ------------------------------------------------------------
## ---------------------- TESTS -------------------------------
## mx <- base_map("Mexico",
##                x_limits = c(-118, -86),
##                y_limits = c(14, 34),
##                show_coords = T)

## ## We can add elements as in ggplot
## mx$mapic + scale_x_continuous(n.breaks = 20) +
##   ggtitle("A map of Mexico")

## ## webscrap_to_db(db_name = "test-mex.sqlite",
## ##                dat = mapic::mexico,
## ##                city = "City",
## ##                country = "Country",
## ##                ##state = "Region",
## ##                db_backup_after = 5)

## ## remove_na_from_db("test-mex.sqlite")

## datmx <- combine_csv_sql(db_file = "test-mex.sqlite",
##                          csv_file = mapic::mexico)


## col_names = list(lat = "lat",
##                  lon = "lon",
##                  cities = "City",
##                  start_year = "Registration_year")
##                  #end_year = "End_year")

## myaes <- with_default_colors(list(dots_orgs = "#D30000",
##                                   text_legend = "#ffffff",
##                                   background_legend = "#000000"))

## ## Using the generic
## mx$mapic + mapic_city_dots(datmx, year = 2022, col_names, map_colors = myaes)

## ## Using the object specific
## mx <- mx |> mapic_city_dots(datmx, year = 2022, col_names)

## base_map("Mexico",
##          x_limits = c(-118, -86),
##          y_limits = c(14, 34),
##          show_coords = T,
##          map_colors = myaes) |>
##   mapic_city_dots(datmx, year = 2022, col_names)

## ## Map the year

## x_lim <- c(-118, -86)
## y_lim <- c(14, 34)
## mx$mapic + mapic_year_internal(2022, x_lim, y_lim, "Año", myaes)

## mx <- mx |> mapic_year_internal()

## ## Map the totals
## mx$mapic + mapic_totals_internal(20, x_lim, y_lim, "Total", myaes)

## mx <- mx |> mapic_totals_internal()

## ## Add ggplot elements
## mx$mapic + scale_x_continuous(n.breaks = 20) +
##   ggtitle("A map of Mexico")

## ## All as a pipeline
## base_map("Mexico",
##          x_lim,
##          y_lim,
##          show_coords = T) |>
##   mapic_city_dots(rbind(datmx, datmx),
##                   year = 2020,
##                   col_names) |>
##   mapic_year_internal(year_label = "Año") |>
##   mapic_totals_internal(totals_label = "Totales") 


## x_lim <- c(4, 18)
## y_lim <- c(47, 56)
## base_map("Germany",
##          x_lim, y_lim,
##          show_coords = T)$mapic + 
##   mapic_year_internal(2022, x_lim, y_lim) +
##   mapic_totals_internal(12, x_lim, y_lim)



## base_map("Russia",
##          show_coords = T)$mapic + 
##                           scale_x_continuous(n.breaks = 20) +
##                           scale_y_continuous(n.breaks = 20)

## x_lim <- c(28, 185)
## y_lim <- c(10, 100)
## base_map("Russia",
##          x_lim, y_lim,
##          show_coords = T)$mapic + 
##   mapic_year_internal(2022, x_lim, y_lim, map_colors = myaes) +
##   mapic_totals_internal(12, x_lim, y_lim, map_colors = myaes)


## ## NOTES:
## ## [TODO] Clean up this and rename the funcs properly
## ## [DONE] Now we want to pass args between functions
## ## [TODO] Save the tests workflow somewhere
