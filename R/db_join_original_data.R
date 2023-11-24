#' @title join the database to the original data
#'
#' @description Joins the data from the database and the original data.
#'
#' @param mdb Mapic database configuration object (See
#' \link{database_configuration} for a reference).
#' @param df data frame with the original data. It MUST contain a
#' column with Unique Identification Numbers called "id".
#' @param city String with the name of the column with the city names.
#'
#' @return The function returns a \code{data.frame} containing the joined data.
#'
#' @details Creates an inner join between the original data and the data
#' contained in the database table. The joint is performed by "id" and "city"
#' fields. The fields "region", "state" and
#' "county" are taken from the database.
#'
#' @export
db_join_original_data <- function(mdb, original_data, city = "city") {
  require(dplyr)

  ## id as character
  if ("id" %in% names(original_data)) {
    original_data$id <- as.character(original_data$id)
  } else {
    stop("Column <id> was not found and it is mandatory.")
  }
  
  stopifnot(is.data.frame(original_data))
  local_df <- original_data

  cols_to_exclude <- names(local_df)[names(local_df) %in% c("country", "region", "state", "county", "year_start", "year_end")]
  if (length(cols_to_exclude) != 0) {
    local_df <- select(local_df, -all_of(cols_to_exclude))
  }
  if (!any(names(local_df) == city)) {
    stop(paste("Field", city, "not found in data frame", sep = " "))
  }

  names(local_df)[names(local_df) == city] <- "city"
  db <- db_load(mdb)
  result <- inner_join(local_df, db, by = c("id", "city"))
  return(result)
}
