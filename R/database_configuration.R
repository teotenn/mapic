#' @title database configuration
#' @description Creates the corresponding mapic db configuration object containing the database configuration details.
#'
#' @param connection Either a database connection or a string describing the
#' type of storage to use. Options are "memory" and "file".
#' @param table_name A string with the table name. If \code{connection} equals
#' 'file' then the path to the file, preferably as csv.
#'
#' @return Returns an S3 object of different class (mapic db configuration object),
#' depending on the type of database selected.
#'
#' @details The function returns an object of the corresponding class, depending of the database specified:
#' \itemize{
#'  \item "memory" returns object of class "mdb_df". Here,
#' all the data obtained from the API
#' connection is stored internally in R only, which means that by clossing
#' the session the information will disappear. When the object \code{mdb_df} is
#' passed to \code{\link{db_load}}, it creates a data frame in the global environment
#' with the name specified in \code{table_name}. Therefore, it is important that an
#' object with the same name does not exist yet, or it will create a conflict.
#'  \item "file" returns object of class "mdb_csv". It uses a text-based csv file
#' to store the data obtained from the API connection. When the object
#' \code{mdb_csv} is passed to \code{\link{db_load}}, it will create a csv file in the
#' path specified in \code{table_name}. It is recommended that such file is not
#' created manually or it can generate conflicts.
#'  \item connection returns object of class "mdb_sql". It uses the
#' database called in the connection object and the table specified in
#' \code{table_name}.
#'}
#'
#' @export
#'
database_configuration <- function(connection,
                                   table_name) {
  stopifnot("The table name must be provided as character." = is.character(table_name))
  
  if (is.character(connection)) {
    if (tolower(connection) == "memory") {
      mdb_obj <- structure(
        list(table_name = table_name),
        class = c("mdb_df"))
    } else if (tolower(connection) == "file") {
      mdb_obj <- structure(
        list(table_name = table_name),
        class = c("mdb_csv"))
    } else {
      stop("Connection type not recognized. Use 'memory' to storage data in a for a data.frame.")
    }
  } else {
    mdb_obj <- structure(
      list(
        connection = connection,
        table_name = table_name),
      class = c("mdb_sql"))
  }
  return(mdb_obj)
}
