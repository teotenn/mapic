% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_load.R
\name{db_load}
\alias{db_load}
\alias{db_load.default}
\alias{db_load.data.frame}
\alias{db_load.mdb_csv}
\alias{db_load.mdb_SQLite}
\title{Loads the database table}
\usage{
db_load(x, ...)

\method{db_load}{default}(x, ...)

\method{db_load}{data.frame}(x, ...)

\method{db_load}{mdb_csv}(x, ...)

\method{db_load}{mdb_SQLite}(mdb)
}
\arguments{
\item{db_file}{Path to the SQLite file containing the table 'orgs'}
}
\value{
An object of class tibble (which inherits data.frame) containing the values stored
in the database with name \code{db_file}
}
\description{
Loads the table database to R as a data frame
}
\section{Methods (by class)}{
\itemize{
\item \code{db_load(default)}: Default

\item \code{db_load(data.frame)}: data.frame

\item \code{db_load(mdb_csv)}: mdb_csv

\item \code{db_load(mdb_SQLite)}: mdb_SQLite

}}
\author{
Manuel Teodoro
}