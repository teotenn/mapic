% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_no_city.R
\name{api_no_city}
\alias{api_no_city}
\title{api no city}
\usage{
api_no_city(
  mdb,
  dat,
  country = "country",
  region = NULL,
  state = NULL,
  county = NULL,
  start_year = NULL,
  end_year = NULL,
  db_backup_after = 10,
  silent = FALSE
)
}
\arguments{
\item{...}{
  Arguments passed on to \code{\link[=api_to_db]{api_to_db}}
  \describe{
    \item{\code{mdb}}{Mapic database configuration object (See
\link{database_configuration} for a reference).}
    \item{\code{dat}}{The data frame containing the data. It MUST contain a collumn called 'id' with UNIQUE identification
numbers, and at least names of the cities and country.}
    \item{\code{country}}{String with the name of the column with the country codes (2-letter)}
    \item{\code{state}}{Optional. String with the name of the column with the state names}
    \item{\code{county}}{Optional. String with the name of the column with the county names}
    \item{\code{region}}{String with the name of the column with the region names. NOT RECOMENDED.}
    \item{\code{start_year}}{String with the name of the column with the year of start.}
    \item{\code{end_year}}{String with the name of the column with the end.}
    \item{\code{db_backup_after}}{Number of iterations after which the data is sent to the database.}
    \item{\code{silent}}{If TRUE, silences the messages from the function.}
  }}
}
\value{
The function fulfills the same task as \code{\link{api_to_db}} but for states or counties.
}
\description{
Retrieves data from the API ignoring city names.
}
\details{
The function provides an alternative system to search for region or
state coordinates, for example, if the map has to be done per region instead of
per city.
}
\seealso{
\link{api_to_db}.
}
