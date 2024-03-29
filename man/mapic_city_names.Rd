% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapic_city_names.R
\name{mapic_city_names}
\alias{mapic_city_names}
\alias{mapic_city_names.default}
\alias{mapic_city_names.mapicHolder}
\title{Mapic city Names}
\usage{
mapic_city_names(x, ...)

\method{mapic_city_names}{default}(
  .df,
  list_cities,
  lat = "lat",
  lon = "lon",
  city = "city",
  font_family = "Arial",
  text_size = 4,
  map_colors = default_map_colors
)

\method{mapic_city_names}{mapicHolder}(
  .mapic_holder,
  list_cities,
  font_family = "Arial",
  text_size = 4
)
}
\arguments{
\item{.df}{Object of class \code{data.frame} containing the data to be mapped
(including coordinates). Preferably as prepared by \code{api_to_db}.
See \code{column_names} below for a list of required fields.}

\item{list_cities}{A vector of characters with the names of the cities to be labelled.}

\item{lat}{Name of the column containing the lattitude. Not necessary if an object of
class \code{mapicHolder} is used.}

\item{lon}{Name of the column containing the longitude. Not necessary if an object of
class \code{mapicHolder} is used.}

\item{city}{Name of the column containing the city names. Not necessary if an object
of class \code{mapicHolder} is used.}

\item{font_family}{Font to be used.}

\item{text_size}{Size of the text in the plot.}

\item{map_colors}{An object of class \code{map_colors} containing the details of the
colors for the maps.
Not necessary if an object of class \code{mapicHolder} is used.}
}
\value{
If the map is created using an object of class \code{mapicHolder}, it returns
the same object with added information and map elements. Otherwise, it returns a
\code{ggplot} object.
}
\description{
Generates labels to show city names.
}
\section{Methods (by class)}{
\itemize{
\item \code{mapic_city_names(default)}: Default

\item \code{mapic_city_names(mapicHolder)}: mapicHolder

}}
