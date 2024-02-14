#' @title api to database
#'
#' @description Based on a list of countries, retrieve coordinates from OSM api and
#' send the results to a database or database alternative.
#'
#' @param mdb Mapic database configuration object (See
#' \link{database_configuration} for a reference).
#' @param dat The data frame containing the data. It MUST contain a collumn called 'id' with UNIQUE identification
#' numbers, and at least names of the cities and country.
#' @param city String with the name of the column with the city names
#' @param country String with the name of the column with the country codes (2-letter)
#' @param state Optional. String with the name of the column with the state names
#' @param county Optional. String with the name of the column with the county names
#' @param region String with the name of the column with the region names. NOT RECOMENDED.
#' @param start_year String with the name of the column with the year of start.
#' @param end_year String with the name of the column with the end.
#' @param db_backup_after Number of iterations after which the data is sent to the database.
#' @param silent If TRUE, silences the messages from the function.
#'
#' @details Given a list of cities and country(ies) in a data frame (as tibble),
#' the function iterates over the values to find the coordinates, using
#' the function \code{coords_from_city} and send the results to a database
#'
#' @details The data must be provided as a data frame, the unique identification
#' numbers must be provided in a column called "id". Other values that MUST be
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
                      city = "city",
                      country = "country",
                      region = NULL,
                      state = NULL,
                      county = NULL,
                      start_year = NULL,
                      end_year = NULL,
                      db_backup_after = 10,
                      silent = FALSE) {
  ## require(RSQLite)
  require(dplyr)

  ## id as character
  if ("id" %in% names(dat)) {
    dat$id <- as.character(dat$id)
  } else {
    stop("Column <id> was not found and it is mandatory.")
  }

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
      if (!silent) print(paste0("Searching entry ", dat_local[["id"]][i]))
      ## Abstracting info
      rg <- ifelse(is.null(region), "", dat_local[[region]][i])
      st <- ifelse(is.null(state), "", dat_local[[state]][i])
      ct <- ifelse(is.null(county), "", dat_local[[county]][i])
      rcity <- dat_local[[city]][i]
      rcountry <- dat_local[[country]][i]

      ##-- Get the coords -- ##
      ## First search in DB
      search_query <- filter(db, city == rcity, country == rcountry,
                             region == rg, state == st, county == ct)
      if (nrow(search_query) != 0) {
        coords <- search_query[1, c("lon", "lat", "osm_name")]
        ## coords$id <- dat_local[["id"]][i]
        if (!silent) print("Found from memory")
      } else {
        ## If not not yet exists, go to OSM API
        coords <- coords_from_city(rcity, rcountry,
                                   region = rg, state = st, county = ct,
                                   silent = silent)
      }
      ## DF exact replica of DB
      coords <- cbind(id = dat_local[["id"]][i],
                      year_start = ifelse(is.null(start_year), NA, as.numeric(dat_local[[start_year]][i])),
                      year_end = ifelse(is.null(end_year), NA, as.numeric(dat_local[[end_year]][i])),
                      city = rcity,
                      country = rcountry,
                      region = rg,
                      state = st,
                      county = ct,
                      coords)

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
              start_year = start_year,
              end_year = end_year,
              db_backup_after = db_backup_after,
              silent = silent)
  } else { ## Exit info
    db_final <- db_load(mdb)
    size <- nrow(db_final)
    not_found <- nrow(db_final[is.na(db_final$lat), ])

    ## Remove not found from DB
    db_remove_empty(mdb)

    if (!silent) {
      message(paste("Search finished.\n",
                    size, "entries searched.\n",
                    not_found, "ENTRIES NOT FOUND"))
    }
  }
}
