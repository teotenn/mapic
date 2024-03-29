#' @title Compares the database to the original data
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
db_compare_data  <- function(mdb, df) {
  mdb_df <- db_load(mdb)
  filtered <- dplyr::filter(df, !(as.character(id) %in% as.character(mdb_df$id)))
  return(filtered)
}
