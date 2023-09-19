#' @title Coordinates from city
#' @author Manuel Teodoro
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
#' @details Get the coordinates of a City, given the Country 2-letter code.
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
  require(RJSONIO)
  require(httr)

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
  link <- paste(
    "https://nominatim.openstreetmap.org/search?city="
  , extrasCoded
  , CountryCoded
  , "&format=json"
  , sep = ""
  )

  response <- try({fromJSON(link)},
                  silent = TRUE)

  if (class(response) == "try-error") {
    stop(response[1])
  } else if (class(response) == "response") {
    response_status <- http_status(response)
    if (response_status$category != "Success") {
      stop(response_status$message)
    }
  } else if (is.list(response)) {
    
    if (length(response) == 0) {
      if (!silent) message(paste("No results found for", extrasCoded))
      coords <- data.frame("lon" = NA, "lat" = NA, "osm_name" = as.character(NA))
      
    } else if (length(response) == 1) {
      if (!silent) message(paste("Found", response[[1]]$display_name))
      coords <- data.frame(
        lon = response[[1]]$lon,
        lat = response[[1]]$lat,
        osm_name = response[[1]]$display_name
      )
      
    } else {
      if (!silent) message(paste("Several entries found for", city, country_code))
      coords <- data.frame(
        lon = response[[1]]$lon,
        lat = response[[1]]$lat,
        osm_name = response[[1]]$display_name
      )
    }

    ## return a df
    return(coords)
  }
}


#' @title api to database
#' @author Manuel Teodoro
#'
#' @description Based on a list of countries, retrieve coordinates from OSM api and
#' send the results to a database or database alternative.
#'
#' @param mdb Mapic database configuration object (See
#' \link{database_configuration} for a reference).
#' @param dat The data frame containing the data. It MUST contain a collumn called 'ID' with UNIQUE identification
#' numbers, and at least names of the cities and country.
#' @param city String with the name of the column with the city names
#' @param country String with the name of the column with the country codes (2-letter)
#' @param state Optional. String with the name of the column with the state names
#' @param county Optional. String with the name of the column with the county names
#' @param region String with the name of the column with the region names. NOT RECOMENDED.
#' @param db_backup_after Number of iteration after which the data is sent to the database.
#' @param silent If TRUE, silences the messages from the function
#'
#' @details Given a list of cities and country(ies) in a data frame (as tibble),
#' the function iterates over the values to find the coordinates, using
#' the function \code{coords_from_city} and send the results to a database
#'
#' @details The data must be provided as a data frame, the unique identification
#' numbers must be provided in a column called "ID". Other values that MUST be
#' provided are name of the cities and country in a 2-letter code.
#'
#' The function uses \code{coords_from_city} for retreiving the data from the API,
#' therefore the values \code{state} and \code{county} are optional.
#' The results of the query are added to the database or database alternative.
#' When more than one result is found, only the first entry is added.
#'
#' @export
api_to_db <- function(mdb,
                      dat,
                      city = "City",
                      country = "Country",
                      region = NULL,
                      state = NULL,
                      county = NULL,
                      db_backup_after = 10,
                      silent = FALSE) {
  ## require(RSQLite)
  require(dplyr)

  ## load db
  db <- db_load(mdb)
  ## Filtering the data
  new_coords <- data.frame()
  dat_local <- db_compare_data(mdb, dat)
  df_len <- nrow(dat_local)

  ## As long as DB and DF have different sizes repeat:
  if (df_len != 0) {
    ## Resize as specified
    dat_local <- dat_local[c(1:db_backup_after), ]
    dat_local <- filter(dat_local, rowSums(is.na(dat_local)) != ncol(dat_local))

    ## ---- Iteration to web-scrap data ---- ##
    ## For loop to api connection
    for (i in 1:nrow(dat_local)) {
      if (!silent) print(paste0("Searching entry ", dat_local[["ID"]][i]))
      ## Abstracting info
      rg <- ifelse(is.null(region), "", dat_local[[region]][i])
      st <- ifelse(is.null(state), "", dat_local[[state]][i])
      ct <- ifelse(is.null(county), "", dat_local[[county]][i])
      rcity <- dat_local[[city]][i]
      rcountry <- dat_local[[country]][i]

      ##-- Get the coords -- ##
      ## First search in DB
      search_query <- filter(db, City == rcity, Country == rcountry,
                             Region == rg, State == st, County == ct)
      if (nrow(search_query) != 0) {
        coords <- search_query[1, ]
        coords$ID <- dat_local[["ID"]][i]
        if (!silent) print("Found from memory")
      } else {
        ## If not not yet exists, go to OSM API
        coords <- coords_from_city(rcity, rcountry,
                                   region = rg, state = st, county = ct,
                                   silent = silent)
        ## DF exact replica of DB
        coords <- cbind(ID = dat_local[["ID"]][i],
                        City = rcity,
                        Country = rcountry,
                        Region = rg,
                        State = st,
                        County = ct,
                        coords)
      }
      new_coords <- rbind(new_coords, coords)
    }

    ## Send only the new results to DB and close connection
    db_append(mdb, new_coords)

    ## repeat
    api_to_db(mdb = mdb,
                   dat = dat,
                   city = city,
                   country = country,
                   region = region,
                   state = state,
                   county = county,
                   db_backup_after = db_backup_after,
                   silent = silent)
  } else { ## Exit info
    db_final <- db_load(mdb)
    size <- nrow(db_final)
    not_found <- nrow(db_final[is.na(db_final$lat), ])

    if (!silent) {
      message(paste("Search finished.\n",
                    size, "entries searched.\n",
                    not_found, "ENTRIES NOT FOUND"))
    }
  }
}


#' @title Add coordinates manually
#' @author Manuel Teodoro
#'
#' @description Add missing values manually, from as csv file or data frame to
#' the database or database alternative.
#'
#' @param complementary_data file containing the missing values. The dataset
#' providing the values must contain the exact same fields as the ones in the
#' database. Fields can be empty except for "ID", "lat" and "lon".
#' @param mdb Mapic database configuration object (See
#' \link{database_configuration} for a reference).
#'
#' @return It appends the provided values to the database.
#'
#' @details Note that if one missing city appears several times, you can provide
#' the lat and long via this function only once, making sure that all the fields
#' for this city match, and then run again
#' \code{link{api_to_sqlite}} which will find it in the database and set it for
#' all entries with the same city.
#'
#' @export
#'
add_coords_manually <- function(complementary_data, mdb) {
  require(dplyr)
  require(RSQLite)

  if (is.character(complementary_data)) {
    if (grepl(".csv", complementary_data, fixed = TRUE)) {
      local_df <- read.csv(complementary_data)
    } else {
      stop("Incorrect file format for complementary_data.")
    }
  } else if (is.data.frame(complementary_data)) {
    local_df <- complementary_data
  } else {
    stop("Incorrect data format for complementary_data.")
  }

  db_df <- db_load(mdb)
  if (any(!names(db_df) %in% names(local_df))) {
    stop(cat("The complementary data is missing columns or the names are not correct.\nCorrect names of the fields/columns are:\n",
             names(db_df), "\n"))
  } else if (any(local_df$ID %in% db_df$ID)) {
    stop(cat("The following ID fields are already present in the database:\n",
             local_df$ID[local_df$ID %in% db_df$ID], "\n"))
  } else if (any(is.na(local_df$lon) | is.na(local_df$lat) | is.na(local_df$ID))) {
    stop("Data missing for either ID, lat or long")
  } else {
    local_df <- local_df %>% mutate_all(~replace(., is.na(.), ""))
    ## Send the values to DB and
    db_append(mdb, local_df)
  }
}


#' @title api no city
#' @author Manuel Teodoro
#'
#' @description Retrieves data from the API ignoring city names.
#'
#' @param mdb Mapic database configuration object (See
#' \link{database_configuration} for a reference).
#' @param dat The data frame containing the data. It MUST contain a collumn
#' named 'ID' with UNIQUE identification numbers, the country
#' and at least one of the following fields: state, county and/or region.
#' @param country String with the name of the column specifying the country
#' (2-letter code)
#' @param state Optional. String with the name of the column with the state names
#' @param county Optional. String with the name of the column with the county names
#' @param region NOT RECOMENDED. String with the name of the column with the region names.
#' @param silent If TRUE, silences the messages from the function
#'
#' @return The function fulfills the same task as \code{\link{api_to_db}}.
#'
#' @details The function provides an alternative system to search for region or
#' state coordinates, for example, if the map has to be done per region instead of
#' per city.
#'
#' @seealso \link{api_to_db}.
#' @export
api_no_city <- function(mdb,
                        dat,
                        country,
                        region = NULL,
                        state = NULL,
                        county = NULL,
                        silent = FALSE) {
  parameters <- c(region, state, county)
  if (length(parameters) == 0) {
    stop("Provide at least one of the following parameters: region, state, county")
  }
  dat$City <- as.character(NA)

  api_to_db(mdb = mdb,
            dat = dat,
            region = region,
            state = state,
            county = county,
            silent = silent)
}
