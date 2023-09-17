#' @title Loads the database table
#' @author Manuel Teodoro
#'
#' @description Loads the table database to R as a data frame
#'
#' @param db_file Path to the SQLite file containing the table 'orgs'
#'
#' @return An object of class tibble (which inherits data.frame) containing the values stored
#' in the database with name \code{db_file}
#'
#' @export
db_load  <- function(x, ...) UseMethod("db_load")

#' @method db_load default
#' @describeIn db_load Default
#' @export
db_load.default <- function(x, ...) {
  stop(paste("Object of class",
             class(x),
             "not recognized.",
             sep = " "))
}

#' @method db_load mdb_df
#' @describeIn db_load mdb_df
#' @export
db_load.mdb_df <- function(mdb, ...) {
  df_name <- mdb$location

  if (!df_name %in% ls(envir = .GlobalEnv)) {
    initial_df <- data.frame(ID = character(0),
                             City = character(0),
                             Country = character(0),
                             Region = character(0),
                             State = character(0),
                             County = character(0),
                             osm_name = character(0),
                             lon = numeric(0),
                             lat = numeric(0))
    assign(df_name, initial_df, envir = .GlobalEnv)
  }
  return(get(df_name, envir = .GlobalEnv))
}

#' @method db_load mdb_csv
#' @describeIn db_load mdb_csv
#' @export
db_load.mdb_csv <- function(mdb, ...) {
  
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
                     City TEXT,
                     Country TEXT, 
                     Region TEXT,
                     State TEXT,
                     County TEXT,
                     osm_name TEXT,
                     lon REAL,
                     lat REAL)"
  )
  dbExecute(conn = con, query_create_table)
  db <- dbReadTable(con, table_name)
  dbDisconnect(con)
  return(db)
}

