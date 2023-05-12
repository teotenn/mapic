## new_mapic_holder <- function(...) {
##   mapic_holder <- structure(list(...), class = "mapic_holder")
##   return(mapic_holder)
## }

## add_mapic_holder <- function(mapic_holder, input_value, name_value) {
##   if (name_value %in% names(mapic_holder)) {
##     warning(paste(name_value, "already present in the mapic_holder object. Overwriting!"))
##   }
##   mapic_holder[[name_value]] <- input_value
##   return(mapic_holder)
## }

is.mPointer <- function(x) inherits(x, "mPointer")

print.mPointer <- function(mapic_pointer, ...) {
  stopifnot(is.mPointer(mapic_pointer))
  stopifnot("mapic" %in% names(mapic_pointer))
  plot(mapic_pointer$mapic)
}

plot.mPointer <- function(p) plot(p$mapic)
