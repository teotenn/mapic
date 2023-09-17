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
compare_db_data  <- function(x, ...) UseMethod("compare_db_data")

#' @method compare_db_data default
#' @describeIn compare_db_data Default
#' @export
compare_db_data.default <- function(x, ...) {
  stop(paste("Object of class",
             class(x),
             "not recognized.",
             sep = " "))
}

#' @method compare_db_data mdb_df
#' @describeIn compare_db_data mdb_df
#' @export
compare_db_data.mdb_df <- function(mdb, df) {
  local_df <- db_load(mdb)
  filtered <- filter(df, !(as.character(ID) %in% as.character(local_df$ID)))
  return(filtered)
}

#' @method compare_db_data mdb_csv
#' @describeIn compare_db_data mdb_csv
#' @export
compare_db_data.mdb_csv <- function(x, ...) {

}

#' @method compare_db_data mdb_SQLite
#' @describeIn compare_db_data mdb_SQLite
#' @export
compare_db_data.mdb_SQLite <- function(mdb, df) {
  local_df <- db_load(mdb)
  filtered <- filter(df, !(as.character(ID) %in% as.character(local_df$ID)))
  return(filtered)
}
