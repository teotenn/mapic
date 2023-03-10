% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database_connections.R
\name{compare_db_data}
\alias{compare_db_data}
\title{compare database to data AAA}
\usage{
compare_db_data(db_file, dat)
}
\arguments{
\item{db_file}{Path to the SQLite file containing the table "orgs" generated by the function
\code{webscrap_to_sqlite}, or with the same parameters}

\item{dat}{Path to the csv file or data.frame containing the source information. It MUST contain a column
with Unique Identification Numbers called "ID"}
}
\value{
The function returns a tibble containing the same data as the csv file, but with only missing values
}
\description{
Makes a tibble with the missing data
}
\details{
Compares the \code{csv} file with the \code{orgs} table contained in a sqlite DB, returning a tibble
that has ONLY the missing data, meaning the ID rows that are present in the tibble/csv-file but are missing
in the database.

If wish to use this function to obtain the values that were not found after the OSM query, run
first \code{remove_na_from_db}.

If you do not wish to modify the database but obtain only the values that were not found during the
OSM query, call the data with \code{import_db_as_df} and filter values where \code{lat} and/or \code{lon}
fields are \code{NA}.
}
