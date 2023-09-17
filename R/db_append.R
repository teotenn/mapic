#' @title Appends rows to the database table
#' @author Manuel Teodoro
#'
#' @description Appends the rows of a data frame into the database table
#'
#' @param db_file Path to the SQLite file containing the table 'orgs'
#'
#' @return An object of class tibble (which inherits data.frame) containing the values stored
#' in the database with name \code{db_file}
#'
#' @export
db_append  <- function(x, ...) UseMethod("db_append")

#' @method db_append default
#' @describeIn db_append Default
#' @export
db_append.default <- function(x, ...) {
  stop(paste("Object of class",
             class(x),
             "not recognized.",
             sep = " "))
}

#' @method db_append mdb_df
#' @describeIn db_append mdb_df
#' @export
db_append.mdb_df <- function(mdb, df) {
  df_name <- mdb$location
  local_df <- get(df_name, envir = .GlobalEnv)
  local_df <- rbind(local_df, df)
  assign(df_name, local_df, envir = .GlobalEnv)
}

#' @method db_append mdb_csv
#' @describeIn db_append mdb_csv
#' @export
db_append.mdb_csv <- function(x, ...) {
  
}

#' @method db_append mdb_SQLite
#' @describeIn db_append mdb_SQLite
#' @export
db_append.mdb_SQLite <- function(mdb, df) {
  require(RSQLite)
  path_to_db <- mdb$location
  table_name <- mdb$table

  con <- dbConnect(drv = RSQLite::SQLite(), dbname = path_to_db)
  dbWriteTable(con, table_name, df, append = TRUE)
  dbDisconnect(con)
}
