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
  df_name <- mdb$location

  if (!df_name %in% ls(envir = .GlobalEnv)) {
    initial_df <- data.frame(ID = character(0),
                             Year_start = numeric(0),
                             Year_end = numeric(0),
                             City = character(0),
                             Country = character(0),
                             Region = character(0),
                             State = character(0),
                             County = character(0),
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
  path_csv <- mdb$location
  initial_df <- data.frame(ID = character(0),
                           Year_start = numeric(0),
                           Year_end = numeric(0),
                           City = character(0),
                           Country = character(0),
                           Region = character(0),
                           State = character(0),
                           County = character(0),
                           lon = numeric(0),
                           lat = numeric(0),
                           osm_name = character(0))

  if (!file.exists(path_csv)) {
    write.csv(initial_df, path_csv, row.names = FALSE)
    local_df <- initial_df
  } else {
    local_df <- read.csv(path_csv)
  }
  return(local_df)
}

#' @method db_load mdb_SQLite
#' @describeIn db_load mdb_SQLite
#' @export
db_load.mdb_SQLite <- function(mdb) {
  require(RSQLite)
  path_to_db <- mdb$location
  table_name <- mdb$table

  con <- dbConnect(drv = RSQLite::SQLite(), dbname = path_to_db)
  query_create_table <- paste0(
    "CREATE TABLE IF NOT EXISTS ", table_name,
    "(ID INTEGER UNIQUE,
       Year_start INTEGER,
       Year_end INTEGER,
       City TEXT,
       Country TEXT,
       Region TEXT,
       State TEXT,
       County TEXT,
       lon REAL,
       lat REAL,
       osm_name TEXT)"
  )
  dbExecute(conn = con, query_create_table)
  db <- dbReadTable(con, table_name)
  dbDisconnect(con)
  return(db)
}
