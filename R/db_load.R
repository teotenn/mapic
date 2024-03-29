#' @title Loads the database table
#'
#' @description Loads the database table to R as a data frame.
#'
#' @param mdb Mapic database configuration object (See
#' \link{database_configuration} for a reference).
#'
#' @return An object of class \code{data.frame} containing the values stored
#' in the database specified in \code{mdb}.
#'
#' @details The main task of the function is to load the table specified in the
#' \code{mdb} object to R as a \code{data.frame}. The function also has the tasks of
#' creating the table when it does not exist yet. This is the equivalent to the
#' command \code{CREATE TABLE IF NOT EXISTS} in SQL. For \code{csv} it creates
#' the file and for \code{data.frame} it creates the data frame in the global
#' environment, both with the structure required by mapic to store the coordinates.
#'
#' @seealso \link{database_configuration}
#' @export
db_load  <- function(mdb, ...) UseMethod("db_load")

#' @method db_load default
#' @describeIn db_load Default
#' @export
db_load.default <- function(mdb) {
  stop(paste("Object of class",
             class(mdb),
             "not recognized as a mapic db configuration object.",
             sep = " "))
}

#' @method db_load mdb_df
#' @describeIn db_load mdb_df
#' @export
db_load.mdb_df <- function(mdb) {
  df_name <- mdb$table

  if (!df_name %in% ls(envir = .GlobalEnv)) {
    initial_df <- data.frame(id = character(0),
                             year_start = numeric(0),
                             year_end = numeric(0),
                             city = character(0),
                             country = character(0),
                             region = character(0),
                             state = character(0),
                             county = character(0),
                             lon = numeric(0),
                             lat = numeric(0),
                             osm_name = character(0))
    assign(df_name, initial_df, envir = .GlobalEnv)
  }
  return(get(df_name, envir = .GlobalEnv))
}

#' @method db_load mdb_csv
#' @describeIn db_load mdb_csv
#' @export
db_load.mdb_csv <- function(mdb) {
  path_csv <- mdb$table
  initial_df <- data.frame(id = character(0),
                           year_start = numeric(0),
                           year_end = numeric(0),
                           city = character(0),
                           country = character(0),
                           region = character(0),
                           state = character(0),
                           county = character(0),
                           lon = numeric(0),
                           lat = numeric(0),
                           osm_name = character(0))

  if (!file.exists(path_csv)) {
    write.csv(initial_df, path_csv, row.names = FALSE)
    local_df <- initial_df
  } else {
    local_df <- read.csv(path_csv)
  }

  local_df <- dplyr::mutate(
    local_df,
    id = as.character(id),
    year_start = as.numeric(year_start),
    year_end = as.numeric(year_end),
    city = as.character(city),
    country = as.character(country),
    region = as.character(region),
    state = as.character(state),
    county = as.character(county),
    lon = as.numeric(lon),
    lat = as.numeric(lat),
    osm_name = as.character(osm_name))
  return(local_df)
}

#' @method db_load mdb_SQLite
#' @describeIn db_load mdb_SQLite
#' @export
db_load.mdb_SQLite <- function(mdb) {
  require(RSQLite)
  path_to_db <- mdb$database
  table <- mdb$table

  con <- dbConnect(drv = RSQLite::SQLite(), dbname = path_to_db)
  query_create_table <- paste0(
    "CREATE TABLE IF NOT EXISTS ", table,
    "(id TEXT UNIQUE,
       year_start INTEGER,
       year_end INTEGER,
       city TEXT,
       country TEXT,
       region TEXT,
       state TEXT,
       county TEXT,
       lon REAL,
       lat REAL,
       osm_name TEXT)"
  )
  dbExecute(conn = con, query_create_table)
  db <- dbReadTable(con, table)
  dbDisconnect(con)
  return(db)
}


#' @method db_load mdb_PostgreSQL
#' @describeIn db_load mdb_PostgreSQL
#' @export
db_load.mdb_PostgreSQL <- function(mdb) {
  require(RPostgreSQL)
  table <- mdb$table
  schema <- mdb$schema

  driv <- DBI::dbDriver("PostgreSQL")
  con <- DBI::dbConnect(driv,
                        dbname =  mdb$database,
                        host = mdb$host,
                        port = mdb$port,
                        user = mdb$user,
                        password = mdb$password)
  query_create_table <- paste0(
    "CREATE TABLE IF NOT EXISTS ",
    schema, ".", table,
    "(id INTEGER UNIQUE,
       year_start INTEGER,
       year_end INTEGER,
       city TEXT,
       country TEXT,
       region TEXT,
       state TEXT,
       county TEXT,
       lon REAL,
       lat REAL,
       osm_name TEXT)"
  )
  DBI::dbExecute(conn = con, query_create_table)
  db <- DBI::dbReadTable(con, name = c(schema, table))
  DBI::dbDisconnect(con)
  return(db)
}
