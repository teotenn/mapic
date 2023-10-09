#' @title Add coordinates manually
#'
#' @description Add missing values manually, from as csv file or data frame to
#' the database or database alternative.
#'
#' @param complementary_data file containing the missing values. The dataset
#' providing the values must contain the exact same fields as the ones in the
#' database. Fields can be empty except for "ID", "lat" and "lon".
#' @param mdb Mapic database configuration object (See
#' \link{database_configuration} for a reference).
#'
#' @return It appends the provided values to the database.
#'
#' @details Note that if one missing city appears several times, you can provide
#' the lat and long via this function only once, making sure that all the fields
#' for this city match, and then run again
#' \code{link{api_to_sqlite}} which will find it in the database and set it for
#' all entries with the same city.
#'
#' @export
#'
add_coords_manually <- function(complementary_data, mdb) {
  require(dplyr)
  require(RSQLite)

  if (is.character(complementary_data)) {
    if (grepl(".csv", complementary_data, fixed = TRUE)) {
      local_df <- read.csv(complementary_data)
    } else {
      stop("Incorrect file format for complementary_data.")
    }
  } else if (is.data.frame(complementary_data)) {
    local_df <- complementary_data
  } else {
    stop("Incorrect data format for complementary_data.")
  }

  db_df <- db_load(mdb)
  if (any(!names(db_df) %in% names(local_df))) {
    stop(cat("The complementary data is missing columns or the names are not correct.\nCorrect names of the fields/columns are:\n",
             names(db_df), "\n"))
  } else if (any(local_df$ID %in% db_df$ID)) {
    stop(cat("The following ID fields are already present in the database:\n",
             local_df$ID[local_df$ID %in% db_df$ID], "\n"))
  } else if (any(is.na(local_df$lon) | is.na(local_df$lat) | is.na(local_df$ID))) {
    stop("Data missing for either ID, lat or long")
  } else {
    local_df <- local_df %>%
      mutate_at(c("City", "Country", "Region", "State", "County", "osm_name"), ~replace(., is.na(.), ""))
    ## Send the values to DB and
    db_append(mdb, local_df)
  }
}
