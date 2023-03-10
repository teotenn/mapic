#' @title Import database as data frame
#' @author Manuel Teodoro
#'
#' @description Imports the database as a tibble data frame
#'
#' @param db_file Path to the SQLite file containing the table 'orgs'
#'
#' @return An object of class tibble (which inherits data.frame) containing the values stored
#' in the database with name \code{db_file}
#'
#' @export
#' @examples
#' x <- import_db_as_df("my_data.sqlite")
import_db_as_df <- function(db_file) {
    require(dplyr)
    require(RSQLite)
    con <- dbConnect(drv = RSQLite::SQLite(), dbname = db_file)
    db <- as_tibble(dbReadTable(con, "orgs"))
    dbDisconnect(con)
    return(db)
}



#' @title remove na from database
#' @author Manuel Teodoro
#'
#' @description Removes values not found from the database
#'
#' @param db_file Path to the SQLite file containing the table "orgs"
#'
#' @details Removes all the values that were not found in the OSM query and thus contain empty values of
#' \code{lat} and \code{lon}. It is IMPORANT to consider that the function modifes the database directly from
#' an SQL query.
#'
#' @export
#' @examples
#' remove_na_from_db("my_data.sqlite")
remove_na_from_db <- function(db_file) {
  require(RSQLite)
  con <- dbConnect(drv = RSQLite::SQLite(), dbname = db_file)
  dbExecute(conn = con,
            "DELETE FROM orgs WHERE lon IS NULL OR trim(lon) = '';")
  dbDisconnect(con)
}



#' @title combine csv to sql table
#' @author Manuel Teodoro
#'
#' @description Joins the data from the SQLite and CSV tables
#'
#' @param db_file Path to the SQLite file containing the table "orgs" generated by the function
#' \code{webscrap_to_sqlite}, or with the same parameters
#' @param csv_file Path to the csv file, or data.frame containing the source information. It MUST contain a
#' column with Unique Identification Numbers called "ID".
#' @param city String with the name of the column with the city names
#'
#' @return The function returns a tibble containing the joined information
#'
#' @details Creates an inner join between the original data (either as csv or data frame tibble) and the data
#' contained in the database table. The joint is performed by ID and City fields. The fields Region, State and
#' County are taken from the database.
#'
#' @export
combine_csv_sql <- function(db_file, csv_file, city = "City") {
  require(dplyr)
  require(readr)
  require(RSQLite)

  if (is.character(csv_file)) {
    if (grepl(".csv", csv_file, fixed = TRUE)) {
      tib <- read_csv(csv_file)
    } else {
      stop("Incorrect file format for data")
    }
  } else if (is.data.frame(csv_file)) {
    tib <- csv_file
  } else {
    stop("Incorrect data format")
  }

  cols_to_exclude <- names(tib)[names(tib) %in% c("Country", "Region", "State", "County")]
  if (length(cols_to_exclude) != 0) {
    tib <- select(tib, -all_of(cols_to_exclude))
  }
  if (!any(names(tib) == city)) {
    stop(paste("Field", city, "not found in data frame", sep = " "))
  }

  names(tib)[names(tib) == city] <- "City"
  db <- import_db_as_df(db_file)
  result <- inner_join(tib, db, by = c("ID", "City"))
  return(result)
}



#' @title compare database to data
#' @description Makes a tibble with the missing data
#'
#' @param db_file Path to the SQLite file containing the table "orgs" generated by the function
#' \code{webscrap_to_sqlite}, or with the same parameters
#' @param dat Path to the csv file or data.frame containing the source information. It MUST contain a column
#' with Unique Identification Numbers called "ID"
#'
#' @details Compares the \code{csv} file with the \code{orgs} table contained in a sqlite DB, returning a tibble
#' that has ONLY the missing data, meaning the ID rows that are present in the tibble/csv-file but are missing
#' in the database.
#' @details If wish to use this function to obtain the values that were not found after the OSM query, run
#' first \code{remove_na_from_db}.
#' @details If you do not wish to modify the database but obtain only the values that were not found during the
#' OSM query, call the data with \code{import_db_as_df} and filter values where \code{lat} and/or \code{lon}
#' fields are \code{NA}.
#'
#' @return The function returns a tibble containing the same data as the csv file, but with only missing values
#' @export
compare_db_data <- function(db_file, dat) {
  require(dplyr)
  require(RSQLite)
  if (is.character(dat)) {
    if (grepl(".csv", dat, fixed = TRUE)) {
      tib <- read_csv(dat)
    } else {
      stop("Incorrect file format for data")
    }
  } else if (is.data.frame(dat)) {
    tib <- dat
  } else {
    stop("Incorrect data format")
  }
  db <- import_db_as_df(db_file)
  filtered <- filter(tib, !(as.character(ID) %in% as.character(db$ID)))
  return(filtered)
}
