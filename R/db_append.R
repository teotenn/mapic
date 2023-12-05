#' @title Appends rows to the database table
#'
#' @description Appends the rows of a data frame into the database table
#'
#' @param mdb Mapic database configuration object (See
#' \link{database_configuration} for a reference).
#' @param df data frame with the rows to be appended.
#'
#' @details The function appends all the rows in \code{df} to the database and
#' table specified in \code{mdb}.
#'
#' @seealso \link{database_configuration}
#' @export
db_append  <- function(mdb, ...) UseMethod("db_append")

#' @method db_append default
#' @describeIn db_append Default
#' @export
db_append.default <- function(mdb, ...) {
  stop(paste("Object of class",
             class(mdb),
             "not recognized as a mapic db configuration object.",
             sep = " "))
}

#' @method db_append mdb_df
#' @describeIn db_append mdb_df
#' @export
db_append.mdb_df <- function(mdb, df) {
  df_name <- mdb$table
  local_df <- get(df_name, envir = .GlobalEnv)
  local_df <- rbind(local_df, df)
  assign(df_name, local_df, envir = .GlobalEnv)
}

#' @method db_append mdb_csv
#' @describeIn db_append mdb_csv
#' @export
db_append.mdb_csv <- function(mdb, df) {
  path_csv <- mdb$table
  write.table(df, path_csv, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE, quote = TRUE)
}

#' @method db_append mdb_SQLite
#' @describeIn db_append mdb_SQLite
#' @export
db_append.mdb_SQLite <- function(mdb, df) {
  require(RSQLite)
  path_to_db <- mdb$database
  table <- mdb$table

  con <- dbConnect(drv = RSQLite::SQLite(), dbname = path_to_db)
  dbWriteTable(con, table, df, append = TRUE)
  dbDisconnect(con)
}

#' @method db_append mdb_PostgreSQL
#' @describeIn db_append mdb_PostgreSQL
#' @export
db_append.mdb_PostgreSQL <- function(mdb, df) {
  require(RPostgreSQL)
  path_to_db <- mdb$database
  table <- mdb$table

  driv <- DBI::dbDriver("PostgreSQL")
  con <- DBI::dbConnect(driv,
                        dbname =  mdb$database,
                        host = mdb$host,
                        port = mdb$port,
                        user = mdb$user,
                        password = mdb$password)
  
  dbWriteTable(con,
               name = c(mdb$schema, mdb$table),
               value = df,
               row.names = FALSE,
               append = TRUE)
  dbDisconnect(con)
}
