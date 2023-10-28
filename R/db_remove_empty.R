#' @title Removes empty entries from database table
#'
#' @description Removes from the database table the entries where the \code{lat} and \code{lot} are empty.
#'
#' @param mdb Mapic database configuration object (See
#' \link{database_configuration} for a reference).
#'
#' @details When the function link{api_to_db} does not find an entry in the query,
#' it adds it to the database with the fields \code{lat} and \code{lon} empty. The
#' present function removes those entries from the database completely.
#'
#' @export
db_remove_empty  <- function(mdb, ...) UseMethod("db_remove_empty")

#' @method db_remove_empty default
#' @describeIn db_remove_empty Default
#' @export
db_remove_empty.default <- function(mdb) {
  stop(paste("Object of class",
             class(mdb),
             "not recognized as a mapic db configuration object.",
             sep = " "))
}

#' @method db_remove_empty mdb_df
#' @describeIn db_remove_empty mdb_df
#' @export
db_remove_empty.mdb_df <- function(mdb) {
  df_name <- mdb$table
  df <- get(df_name, envir = .GlobalEnv)
  output <- dplyr::filter(df, !is.na(lat), !is.na(lon))
  assign(df_name, output, envir = .GlobalEnv)
}

#' @method db_remove_empty mdb_csv
#' @describeIn db_remove_empty mdb_csv
#' @export
db_remove_empty.mdb_csv <- function(mdb) {
  df <- db_load(mdb)
  output <- dplyr::filter(df, !is.na(lat), !is.na(lon))
  write.csv(output, mdb$table, row.names = FALSE)
}

#' @method db_remove_empty mdb_SQLite
#' @describeIn db_remove_empty mdb_SQLite
#' @export
db_remove_empty.mdb_SQLite <- function(mdb) {
  require(RSQLite)
  path_to_db <- mdb$database
  table <- mdb$table

  con <- dbConnect(drv = RSQLite::SQLite(), dbname = path_to_db)
  dbExecute(conn = con,
            paste0("DELETE FROM ", table,  " WHERE lon IS NULL OR lat IS NULL"))
  dbDisconnect(con)
}
