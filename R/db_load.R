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

#' @method db_load data.frame
#' @describeIn db_load data.frame
#' @export
db_load.data.frame <- function(x, ...) {

}

#' @method db_load mdb_csv
#' @describeIn db_load mdb_csv
#' @export
db_load.mdb_csv <- function(x, ...) {
  
}

#' @method db_load mdb_SQLite
#' @describeIn db_load mdb_SQLite
#' @export
db_load.mdb_SQLite <- function(mdb) {
  require(RSQLite)
  path_to_db <- mdb$path
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
