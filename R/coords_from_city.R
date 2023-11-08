#' @title Coordinates from city
#'
#' @description Get coordinates of a city per country.
#'     
#' @param city Name of the city
#' @param country_code the 2-letter code of the country
#' @param region Optional, region name. This option is NOT RECOMENDED,
#' it's functioning is not wel docummented.
#' @param state Optional, state name.
#' @param county Optional, county name.
#' @param choose_when_multiple Not implemented yet!.
#' @param silent If TRUE, silences the messages from the function.
#'
#' @return Returns a 1-row data frame, containing latitude, longitude and osm name.
#'
#' @details Get the coordinates of a city, given the Country 2-letter code.
#' The search can be supported by region, state and/or county (optional).
#' The function uses open street maps nominatim api.
#'
#' @details All the variables used by the function must be strings
#' @export
#'
coords_from_city <- function(city = NULL,
                             country_code,
                             region = NULL,
                             state = NULL,
                             county = NULL,
                             choose_when_multiple = FALSE,
                             silent = FALSE) {
  CityCoded <- gsub(" ", "%20", city) #remove space for URLs
  CountryCoded <- paste("&countrycodes=", country_code, sep = "")
  extras <- c(city = city, state = state, region = region, county = county)
  extrasCoded <- ""
  if (!is.null(extras)) {
    for (i in 1:length(extras)) {
      if (extras[i] != "" && !is.na(extras[i]) && !grepl("^\\s*$", extras[i])) {
        valCoded <- gsub(" ", "%20", extras[i])
        extrasCoded <- paste0(extrasCoded, "&", names(extras)[i], "=", valCoded)
      }
    }
  }
  
  ## get data
  response <- paste(
    "http://nominatim.openstreetmap.org/search?city=",
    extrasCoded,
    CountryCoded,
    "&format=json",
    sep = ""
  )
  x <- RJSONIO::fromJSON(response)
  
  ## retrieve coords
  if (is.vector(x)) {
    message(paste("Found", x[[1]]$display_name))
    lon <- x[[1]]$lon
    lat <- x[[1]]$lat
    osm_name <- x[[1]]$display_name
    coords <- data.frame("lon" = lon, "lat" = lat, "osm_name" = osm_name)
  } else {
    message(paste("No results found for", extrasCoded, country_code))
    coords <- data.frame("lon" = NA, "lat" = NA, "osm_name" = as.character(NA))
  }
  
  ## return a df
  return(coords)
}

