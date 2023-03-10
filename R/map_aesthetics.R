## ------------------------------------------------------------
##### ------------------ C L A S S E S ------------------ #####
## CONSTRUCTORS
define_map_aesthetics <- function(aes_name, colors, sizes, legends) {
  stopifnot(is.character(aes_name))
  stopifnot(is.map_colors(colors))
  stopifnot(is.map_sizes(sizes))
  stopifnot(is.map_legends(legends))

  structure(
    list(
      Name = aes_name,
      colors = colors,
      sizes = sizes,
      legends = legends),
    class = c("map_aesthetics", "map_master_aes"))
}

define_map_colors <- function(dots_orgs,
                               target_country,
                               empty_countries,
                               border_countries,
                               oceans,
                               text_cities,
                               text_legend,
                               text_copyright) {
  require(stringr)
  all_arguments <- c(as.list(environment()))
  for (arggs in all_arguments) {
    stopifnot("All arguments must be character" = is.character(arggs))
    if (nchar(arggs) != 7) {
      stop("Colors should be in hex notation")
    }
    if (!str_detect(arggs, "^#")) {
      stop("Colors should be in hex notation")
    }
  }

  structure(
    list(
      dots_orgs = dots_orgs,
      target_country = target_country,
      empty_countries = empty_countries,
      border_countries = border_countries,
      oceans = oceans,
      text_cities = text_cities,
      text_legend = text_legend,
      text_copyright = text_copyright),
    class = c("map_aesthetics", "map_colors"))
}

define_sizes_in_map <- function(basic_dot,
                                 text_city) {
  all_arguments <- c(as.list(environment()))
  for (arggs in all_arguments) {
    stopifnot("All arguments must be numeric" = is.numeric(arggs))
  }

  structure(
    list(
      basic_dot = basic_dot,
      text_city = text_city),
    class = c("map_aesthetics", "map_sizes"))
}

define_legends_specs <- function(legend_year,
                                  legend_orgs) {
  all_arguments <- c(as.list(environment()))
  for (arggs in all_arguments) {
    stopifnot("All arguments must be character" = is.character(arggs))
  }

  structure(
    list(
      legend_year = legend_year,
      legend_orgs = legend_orgs),
    class = c("map_aesthetics", "map_legends"))
}

## VALIDATORS
is.map_aesthetics <- function(x) inherits(x, "map_aesthetics")
is.map_master_aes <- function(x) inherits(x, "map_master_aes")
is.map_colors <- function(x) inherits(x, "map_colors")
is.map_sizes <- function(x) inherits(x, "map_sizes")
is.map_legends <- function(x) inherits(x, "map_legends")

## DEFAULTS
default_map_colors <- define_map_colors(dots_orgs = "#574166",
                                        target_country = "#8caeb4",
                                        empty_countries = "#f3f3f3",
                                        border_countries = "#9c9c9c",
                                        oceans = "#4e91d2",
                                        text_cities = "#a0a0a0",
                                        text_legend = "#283151",
                                        text_copyright = "#f3f3f3")

default_legends_specs <- define_legends_specs("Year", "Organizations")

default_sizes_in_map <- define_sizes_in_map(2.5, 4)

default_aesthetics <- define_map_aesthetics("Default maps aesthetics",
                                            default_map_colors,
                                            default_sizes_in_map,
                                            default_legends_specs)

## ------------------------------------------------------------
##### ------------------ M E T H O D S ------------------ #####

print.map_aesthetics <- function(x, ...) {
  require(glue)
  stopifnot(is.map_aesthetics(x))
  print(glue("{class(x)[2]}"))
  for (argg in names(x)) {
    print(glue("{argg} : {x[argg]}"))
  }
  invisible(x)
}




## create_map_aes <- function(x) {
##   UseMethod("create_map_aes")
## }

## create_map_aes.list <- function(aesthetics_list) {
##   ## Get arguments using constructors with defaults
##   args <- aesthetics_list
##   map_aes <- list()
##   if ("color_oceans" %in% names(args)) {
##     map_aes[["colors"]] <- list(oceans = args$color_oceans)
##   } else {
##     map_aes[["colors"]] <- list(oceans = "blue")
##   }
##   return(map_aes)
## }

## create_map_aes.map_master_aes <- function(map_aes) {
##   return(map_aes)
## }




with_default_aesthetics <- function(colors = NA, sizes = NA, legends = NA) {
  require(purrr)
  if (!is.na(colors)) default_map_colors <<- list_modify(default_map_colors, !!!colors)
  if (!is.na(sizes)) default_sizes_in_map <<- list_modify(default_sizes_in_map, !!!sizes)
  if (!is.na(legends)) default_legends_specs <<- list_modify(default_legends_specs, !!!legends)
  default_aesthetics <<- define_map_aesthetics("Default maps aesthetics",
                                               default_map_colors,
                                               default_sizes_in_map,
                                               default_legends_specs)
}

## TEST
with_default_map_aes(list(dots_orgs = "orange"), list(text_city = 12))



## TO_DO
## X) Create a father class
## *) Check missing sizes
## *) Check missing legends specs
## X) Create func to modify defaults in father object
## *) Modify func create_map_aes.list to use it
