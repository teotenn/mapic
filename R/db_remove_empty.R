#' @title Removes empty entries from database table
#'
#' @description Removes from the database table the entries where the \code{lat} and \code{lot} are empty.
#'
#' @noRd
#' 
db_remove_empty  <- function(mdb, ...) UseMethod("db_remove_empty")

#' @method db_remove_empty default
#' @describeIn db_remove_empty Default
#' 
db_remove_empty.default <- function(mdb) {
  stop(paste("Object of class",
             class(mdb),
             "not recognized as a mapic db configuration object.",
             sep = " "))
}

#' @method db_remove_empty mdb_df
#' @describeIn db_remove_empty mdb_df
#' 
db_remove_empty.mdb_df <- function(mdb) {
  df_name <- mdb$table_name
  df <- get(df_name, envir = .GlobalEnv)
  output <- dplyr::filter(df, !is.na(lat), !is.na(lon))
  assign(df_name, output, envir = .GlobalEnv)
}

#' @method db_remove_empty mdb_csv
#' @describeIn db_remove_empty mdb_csv
#' 
db_remove_empty.mdb_csv <- function(mdb) {
  df <- db_load(mdb)
  output <- dplyr::filter(df, !is.na(lat), !is.na(lon))
  write.csv(output, mdb$table_name, row.names = FALSE)
}

#' @method db_remove_empty mdb_sql
#' @describeIn db_remove_empty mdb_sql
#' 
db_remove_empty.mdb_sql <- function(mdb) {
  table_name <- mdb$table_name
  con <- mdb$connection
  DBI::dbExecute(conn = con,
            paste0("DELETE FROM ", table_name,  " WHERE lon IS NULL OR lat IS NULL"))
  ## dbDisconnect(con)
}
