#' @title join the database to the original data
#' @author Manuel Teodoro
#'
#' @description Joins the data from the database and the original data.
#'
#' @param mdb Mapic database configuration object (See
#' \link{database_configuration} for a reference).
#' @param df data frame with the original data. It MUST contain a
#' column with Unique Identification Numbers called "ID".
#' @param city String with the name of the column with the city names.
#'
#' @return The function returns a \code{data.frame} containing the joined data.
#'
#' @details Creates an inner join between the original data and the data
#' contained in the database table. The joint is performed by "ID" and "City"
#' fields. The fields "Region", "State" and
#' "County" are taken from the database.
#'
#' @export
db_join_original_data <- function(mdb, original_data, city = "City") {
  require(dplyr)

  stopifnot(is.data.frame(original_data))
  local_df <- original_data

  cols_to_exclude <- names(local_df)[names(local_df) %in% c("Country", "Region", "State", "County")]
  if (length(cols_to_exclude) != 0) {
    local_df <- select(local_df, -all_of(cols_to_exclude))
  }
  if (!any(names(local_df) == city)) {
    stop(paste("Field", city, "not found in data frame", sep = " "))
  }

  names(local_df)[names(local_df) == city] <- "City"
  db <- db_load(mdb)
  result <- inner_join(local_df, db, by = c("ID", "City"))
  return(result)
}
