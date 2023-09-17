#' @title Compares the database to the original data
#' @author Manuel Teodoro
#'
#' @description Creates a data frame with the missing data
#'
#' @param db_file Path to the SQLite file containing the table 'orgs'
#'
#' @return An object of class tibble (which inherits data.frame) containing the values stored
#' in the database with name \code{db_file}
#'
#' @export
db_compare_data  <- function(x, ...) UseMethod("db_compare_data")

#' @method db_compare_data default
#' @describeIn db_compare_data Default
#' @export
db_compare_data.default <- function(x, ...) {
  stop(paste("Object of class",
             class(x),
             "not recognized.",
             sep = " "))
}

#' @method db_compare_data mdb_df
#' @describeIn db_compare_data mdb_df
#' @export
db_compare_data.mdb_df <- function(mdb, df) {
  local_df <- db_load(mdb)
  filtered <- filter(df, !(as.character(ID) %in% as.character(local_df$ID)))
  return(filtered)
}

#' @method db_compare_data mdb_csv
#' @describeIn db_compare_data mdb_csv
#' @export
db_compare_data.mdb_csv <- function(x, ...) {

}

#' @method db_compare_data mdb_SQLite
#' @describeIn db_compare_data mdb_SQLite
#' @export
db_compare_data.mdb_SQLite <- function(mdb, df) {
  local_df <- db_load(mdb)
  filtered <- filter(df, !(as.character(ID) %in% as.character(local_df$ID)))
  return(filtered)
}
