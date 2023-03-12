#' @title Coordinates from city
#' @author Manuel Teodoro
#'
#' @description Get coordinates of a city per country
#'
#' @param city Name of the city
#' @param country_code the 2-letter code of the country
#' @param region Optional, region name. This option is NOT RECOMENDED, it's functioning is not wel docummented.
#' @param state Optional, state name
#' @param county Optional, county name
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
                             county = NULL) {
  require("RJSONIO")

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
    "http://nominatim.openstreetmap.org/search?city="
  , extrasCoded
  , CountryCoded
  , "&format=json"
  , sep = ""
  )
  x <- fromJSON(response)

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



#' @title webscrap to database
#' @author Manuel Teodoro
#'
#' @description Retrieve coordinates from a list of countries and send it to a sql DB
#'
#' @param db_name name of the SQLite database. If not exists, it will be created
#' @param dat The tibble containing the data. It MUST contain a collumn called 'ID' with UNIQUE identification
#' numbers, and at least names of the cities, country and date of registration of the Org.
#' @param city String with the name of the column with the city names
#' @param country String with the name of the column with the country codes (2-letter)
#' @param state Optional. String with the name of the column with the state names
#' @param county Optional. String with the name of the column with the county names
#' @param region String with the name of the column with the region names. NOT RECOMENDED.
#'
#' @return Creates a table (if not yet exists) called "orgs" in the given SQLite data base. The table contains
#' the ID, city name and country code obtained for the data inside the tibble \code{dat}, together with the
#' latitude and longitude of the city, per organization.
#'
#' @details Given a list of cities and country(ies) in a data frame (as tibble),
#' the function iterates over the values to find the coordinates, using
#' the function \code{coords_from_city} and send the results to a SQLite DB
#'
#' @details The data must be provided as a tibble, the unique identification numbers must be provided in a
#' column called "ID". Other values that MUST be provided in the tibble are name of the cities and country in
#' a 2-letter code.
#'
#' The function uses \code{coords_from_city} for the web scrapping, therefore the values \code{state} and
#' \code{county} are optional.
#' The results of the query will be added to a table called "orgs" into the SQLite database ONLY when a single
#' result is found. If more than one result is found, the first entry will be added, if none are found, the
#' details will be printed on screen and the entry will not be added to the SQLite table.
#'
#' @details NOTE: As of version 2.3.1, the function passes to the DB empty strings \code{""} from the empty values of
#' state, region and county. This behaviour doesn't seem to conflict with the creation of the maps.
#' If necessary, it is easy to convert all such values to \code{NA} in R.
#' If bugs are found regarding this behaviour conflicting with the database, please report it immediately.
#' @export
webscrap_to_db <- function(db_name,
                           dat,
                           city = "City",
                           country = "Country",
                           region = NULL,
                           state = NULL,
                           county = NULL,
                           db_backup_after = 10) {
    require(RSQLite)
    require(dplyr)

    ## Connect to db and table
    con <- dbConnect(drv = SQLite(), dbname = db_name)
    dbExecute(conn = con,
              "CREATE TABLE IF NOT EXISTS orgs
                    (ID INTEGER UNIQUE,
                     City TEXT,
                     Country TEXT, 
                     Region TEXT,
                     State TEXT,
                     County TEXT,
                     osm_name TEXT,
                     lon REAL,
                     lat REAL)")
    ## And load it
    db <- as_tibble(dbReadTable(con, "orgs"))
    ## Filtering the data
    new_coords <- data.frame()
    dat_local <- compare_db_data(db_name, dat)
    df_len <- nrow(dat_local)

    ## As long as DB and DF have different sizes repeat:
    if (df_len != 0) {
        ## Resize as specified
        dat_local <- dat_local[c(1:db_backup_after), ]
        dat_local <- filter(dat_local, rowSums(is.na(dat_local)) != ncol(dat_local))

        ## ---- Iteration to web-scrap data ---- ##
        ## For loop to webscrapping
        for (i in 1:nrow(dat_local)) {
            print(paste0("Searching entry ", dat_local[["ID"]][i]))
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
                print("Found from memory")
            } else {
                ## If not not yet exists, go to OSM API
                coords <- coords_from_city(rcity, rcountry,
                                           region = rg, state = st, county = ct)
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
        dbWriteTable(con, "orgs", new_coords, append = TRUE)
        dbDisconnect(con)

        ## repeat
        webscrap_to_db(db_name = db_name,
                           dat = dat,
                           city = city,
                           country = country,
                           region = region,
                           state = state,
                           county = county,
                           db_backup_after = db_backup_after)
    } else { ## Exit info
        db_final <- import_db_as_df(db_name)
        size <- nrow(db_final)
        not_found <- nrow(db_final[is.na(db_final$lat), ])
        message(paste("Search finished.\n",
                      size, "entries searched.\n",
                      not_found, "ENTRIES NOT FOUND"))
    }
}



#' @title add coordinates manually
#' @author Manuel Teodoro
#'
#' @description Add missing values manually, from as csv file, tibble or data frame to SQLite table "orgs"
#'
#' @param csv_file file containing the missing values. The dataset providing the values must contain the exact
#' same fields as the ones in the database table. Fields can be empty except for "ID", "lat" and "lon".
#' @param db_name name of the SQLite database containing the table "orgs"
#'
#' @return It appends the provided values to the database table
#'
#' @details Note that if one missing city appears several times, you can provide the lat and long via this
#' function only once, making sure that all the fields for this city match, and then run again
#' \code{webscrap_to_sqlite} which will find it in the database and set it for all other values to be searched.
#'
#' @export
#'
add_coords_manually <- function(csv_file, db_name) {
  require(dplyr)
  require(RSQLite)
  require(readr)

  if (is.character(csv_file)) {
    if (grepl(".csv", csv_file, fixed = TRUE)) {
      tib <- read_csv(csv_file, show_col_types = FALSE)
    } else {
      stop("Incorrect file format for data")
    }
  } else if (is.data.frame(csv_file)) {
    tib <- csv_file
  } else {
    stop("Incorrect data format")
  }

  db_df <- import_db_as_df(db_name)
  if (any(!names(db_df) %in% names(tib))) {
    stop(cat("The dataset with the missing info is missing columns or the names are not correct.\nCorrect names of the fields/columns are:\n",
             names(db_df), "\n"))
  } else if (any(tib$ID %in% db_df$ID)) {
    stop(cat("The following ID fields are already present in the database:\n",
             tib$ID[tib$ID %in% db_df$ID], "\n"))
  } else if (any(is.na(tib$lon) | is.na(tib$lat) | is.na(tib$ID))) {
    stop("Data missing for either ID, lat or long")
  } else {
    tib <- tib %>% mutate_all(~replace(., is.na(.), ""))
    ## Send the values to DB and
    con <- dbConnect(drv = SQLite(), dbname = db_name)
    dbWriteTable(con, "orgs", tib, append = TRUE)
    dbDisconnect(con)
  }
}



#' @title webscrap no city
#' @author Manuel Teodoro
#'
#' @description Performs the webs crapping ignoring city names
#'
#' @param db_name name of the SQLite database. If not exists, it will be created.
#' @param dat The tibble containing the data. It MUST contain a collumn called 'ID' with UNIQUE identification numbers
#' and at least names of the cities, country and date of registration of the Org.
#' @param country String with the name of the column with the country codes (2-letter)
#' @param state Optional. String with the name of the column with the state names
#' @param county Optional. String with the name of the column with the county names
#' @param region NOT RECOMENDED. String with the name of the column with the region names.
#'
#' @return Creates a table (if not yet exists) called "orgs" in the given SQLite data base. The table contains the ID,
#' country code and the extra field(s) provided, together with the latitude and longitude of the region/state/county,
#' per organization. The column City also exists but is filled with empty values.
#'
#' @details The function provides an alternative system to search for region or state coordinates, for example,
#' if the map has to be done per region instead of per city.
#'
#' @export
#'
#'
webscrap_no_city <- function(db_name,
                             dat,
                             country,
                             region = NULL,
                             state = NULL,
                             county = NULL,
                             city = NULL) {
  if (!missing(city)) {
    warning("As from v2.3.1 the parameter <city> is not used anymore. Please remove it to avoid this warning.")
  }

  parameters <- c(region, state, county)
  if (length(parameters) == 0) {
    stop("Provide at least one of the following parameters: region, state, county")
  }
  dat$City <- as.character(NA)

  webscrap_to_db(db_name = db_name,
                 dat = dat,
                 region = region,
                 state = state,
                 county = county)
}
