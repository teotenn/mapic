#' @export
is.mapicHolder <- function(x) inherits(x, "mapicHolder")

#' @export
print.mapicHolder <- function(mapic_holder, ...) {
  stopifnot(is.mapicHolder(mapic_holder))
  stopifnot("mapic" %in% names(mapic_holder))
  plot(mapic_holder$mapic)
}

#' @export
plot.mapicHolder <- function(p) plot(p$mapic)
