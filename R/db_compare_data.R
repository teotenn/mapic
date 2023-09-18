#' @title Compares the database to the original data
#' @author Manuel Teodoro
#'
#' @description Creates a data frame with the missing data.
#'
#' @param mdb Mapic database configuration object (See
#' \link{database_configuration} for a reference).
#' @param df data frame with the original data.
#'
#' @return An object of class \code{data.frame} containing the missing values
#' in the database (specified in \code{mdb}) as compared with the original \code{df}
#'
#' @seealso \link{database_configuration}
#' @export
db_compare_data  <- function(mdb, ...) UseMethod("db_compare_data")

#' @method db_compare_data default
#' @describeIn db_compare_data Default
#' @export
db_compare_data.default <- function(mdb, ...) {
  stop(paste("Object of class",
             class(mdb),
             "not recognized as a mapic db configuration object.",
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
