#' @title api no city
#'
#' @description Retrieves data from the API ignoring city names.
#'
#' @inheritDotParams api_to_db mdb dat country:silent
#'
#' @return The function fulfills the same task as \code{\link{api_to_db}} but for states or counties.
#'
#' @details The function provides an alternative system to search for region or
#' state coordinates, for example, if the map has to be done per region instead of
#' per city.
#'
#' @seealso \link{api_to_db}.
#' @export
api_no_city <- function(mdb,
                        dat,
                        country = "Country",
                        region = NULL,
                        state = NULL,
                        county = NULL,
                        start_year = NULL,
                        end_year = NULL,
                        db_backup_after = 10,
                        silent = FALSE) {
  parameters <- c(region, state, county)
  if (length(parameters) == 0) {
    stop("Provide at least one of the following parameters: region, state, county")
  }
  dat$City <- as.character(NA)

  api_to_db(mdb = mdb,
            dat = dat,
            region = region,
            state = state,
            county = county,
            start_year = start_year,
            end_year = end_year,
            db_backup_after = db_backup_after,
            silent = silent)
}
