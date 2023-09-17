#' @title Removes empty entries from tabase table
#' @author Manuel Teodoro
#'
#' @description Removes from the database table the entries where the \code{lat} and \code{lot} are empty.
#'
#' @param db_file Path to the SQLite file containing the table 'orgs'
#'
#' @return An object of class tibble (which inherits data.frame) containing the values stored
#' in the database with name \code{db_file}
#'
#' @export
db_remove_empty  <- function(x, ...) UseMethod("db_remove_empty")

#' @method db_remove_empty default
#' @describeIn db_remove_empty Default
#' @export
db_remove_empty.default <- function(x, ...) {
  stop(paste("Object of class",
             class(x),
             "not recognized.",
             sep = " "))
}

#' @method db_remove_empty mdb_df
#' @describeIn db_remove_empty mdb_df
#' @export
db_remove_empty.mdb_df <- function(mdb) {
  df_name <- mdb$location
  df <- get(df_name, envir = .GlobalEnv)
  output <- df[!is.na(df$lon),]
  assign(df_name, output, envir = .GlobalEnv)
}

#' @method db_remove_empty mdb_csv
#' @describeIn db_remove_empty mdb_csv
#' @export
db_remove_empty.mdb_csv <- function(x, ...) {

}

#' @method db_remove_empty mdb_SQLite
#' @describeIn db_remove_empty mdb_SQLite
#' @export
db_remove_empty.mdb_SQLite <- function(mdb) {
  require(RSQLite)
  path_to_db <- mdb$location
  table_name <- mdb$table

  con <- dbConnect(drv = RSQLite::SQLite(), dbname = path_to_db)
  dbExecute(conn = con,
            paste0("DELETE FROM ", table_name,  " WHERE lon IS NULL OR trim(lon) = '';"))
  dbDisconnect(con)
}
