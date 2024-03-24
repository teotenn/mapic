library(testthat)
data(mexico)
t_dat <- mexico

### ---------- T E S T S ---------- ###
test_that("database_configuration", {
  mock_mdb <<- database_configuration("SQLite", "organizations", "test.sqlite")
  expect_s3_class(mock_mdb, "mdb_SQLite")
  expect_equal(mock_mdb$database, "test.sqlite")
  expect_equal(mock_mdb$table, "organizations")
})

test_that("coords_from_city: Found results", {
  ## Case:
  ## obtaining coords from open street maps for CITY ONLY
  found <- coords_from_city(city = t_dat$city[1],
                            country_code = t_dat$country[1],
                            silent = TRUE)
  ## Case:
  ## obtaining coords from open street maps Using region
  found_region <- coords_from_city(city = t_dat$city[2],
                                   country_code = t_dat$country[2],
                                   region = t_dat$region[2],
                                   silent = TRUE)
  ## Case:
  ## obtaining coords from open street maps Using state
  found_state <- coords_from_city(city = t_dat$city[2],
                                  country_code = t_dat$country[2],
                                  state = t_dat$region[2],
                                  silent = TRUE)
  ## TESTS
  expect_s3_class(found, "data.frame")
  expect_true(complete.cases(found))
  expect_s3_class(found_state, "data.frame")
  expect_true(complete.cases(found_state))
  expect_s3_class(found_region, "data.frame")
  expect_true(complete.cases(found_region))
})


test_that("coords_from_city: Not found results", {
  ## Case:
  ## Not found coords due to wrong city
  not_found <- coords_from_city(city = "Mex. city",
                                country_code = t_dat$country[1],
                                state = t_dat$region[1],
                                silent = TRUE)
  ## Case:
  ## Not found coords due to wrong state or region
  not_found_state <- coords_from_city(city = t_dat$city[1],
                                      country_code = t_dat$country[1],
                                      state = "Tamaulipas",
                                      silent = TRUE)
  ## TESTS
  expect_s3_class(not_found, "data.frame")
  expect_false(complete.cases(not_found))
  expect_s3_class(not_found_state, "data.frame")
  expect_false(complete.cases(not_found_state))
})


## TEST FUNCTION: <api_to_sqlite> -------------------------------|

## Find a suitable test, for now test results below
mock_data <- dplyr::mutate(
  t_dat,
  city = ifelse(city == "Ciudad de Mexico", "CD Mex", city))

api_to_db(mock_mdb,
          dat = mock_data,
          city = "city",
          country = "country",
          state = "region",
          year_start = "registration_year",
          year_end = "year_end",
          db_backup_after = 5,
          silent = TRUE)
## RESULTS:
## 1) Not found Cd de Mexico
## 2) Found everything else
## 3) Tijuana already in DB
## ------------------------------------------------------------------|

test_that("db_load: returns a data frame", {
    db_as_df <- db_load(mock_mdb)
    ## TESTS
    expect_s3_class(db_as_df, "data.frame")
    ## expect_equal(nrow(db_as_df), 5)
    ## expect_equal(nrow(filter(db_as_df, is.na(lat))), 4)
    expect_equal(ncol(db_as_df), 11)
    expect_vector(db_as_df$city, ptype = character())
    expect_vector(db_as_df$lat, ptype = double())
    expect_vector(db_as_df$year_start, ptype = integer())
    expect_setequal(unique(db_as_df$country), "MX")
    ## expect_equal(length(complete.cases(db_as_df)[complete.cases(db_as_df) == TRUE]), 6)
})


test_that("db_remove_empty", {
    db_remove_empty(mock_mdb)
    db_as_df <- db_load(mock_mdb)
    ## TESTS
    ## expect_equal(nrow(db_as_df), 6)
    expect_equal(nrow(filter(db_as_df, is.na(lat))), 0)
})


test_that("db_compare_data: error", {
    expect_error(db_compare_data(mock_mdb, c(1, 2, 3)),
                 "no applicable method")
    expect_error(db_compare_data(1, data.frame()),
                 "Object of class numeric not recognized")
})


test_that("db_compare_data: returns a data frame", {
    missing <- db_compare_data(mock_mdb, mock_data)
    ## TESTS
    expect_s3_class(missing, "data.frame")
    ## expect_equal(nrow(missing), 4)
    expect_equal(ncol(missing), 8)
    expect_vector(missing$city, ptype = character())
    ## expect_setequal(missing$country, rep("MX", 4))
})


test_that("db_join_original_data: from data.frame", {
    combined <- db_join_original_data(mdb = mock_mdb,
                                      original_data = mock_data)
    ## TESTS
    expect_s3_class(combined, "data.frame")
    ## expect_equal(nrow(combined), 6)
    expect_equal(ncol(combined), 15)
    expect_vector(combined$city, ptype = character())
    expect_vector(combined$lon, ptype = double())
    expect_setequal(combined$country, rep("MX", 6))
})


test_that("add_coords_manually", {
  to_add <- data.frame(id = 1,
                       year_start = 1900, year_end = NA,
                       city = "CD Mex",
                       country = "MX", region = "",
                       state = "Mexico", county = "",
                       lon = 12, lat = 13, osm_name = "")
  add_coords_manually(to_add, mock_mdb)
  df_added <- db_load(mock_mdb)
  api_to_db(mock_mdb, mock_data, state = "region", silent = TRUE)
  df_complete <- db_load(mock_mdb)
  expect_true(1 %in% df_complete$id)
  expect_true(!is.na(df_complete[df_complete$id == 1, ]$lat))
  expect_equal(df_complete$lon[df_complete$id == 1], 12)
  ## expect_equal(nrow(df_added), 7)
  ## expect_equal(nrow(df_complete), 9)
})


test_that("api_no_city", {
  new_state <- data.frame(id = 11,
                          Name = "org11",
                          Type = "none",
                          registration_year = 2005,
                          year_end = NA,
                          country = "MX",
                          region = "Tlaxcala",
                          city = NA,
                          Source = "none")
  api_no_city(mock_mdb, new_state, "country", state = "region", silent = T)
  df_added <- db_load(mock_mdb)
  expect_true(11 %in% df_added$id)
  expect_true(!is.na(df_added[df_added$id == 11, ]$lat))
  ## expect_equal(nrow(df_added), 10)
})


file.remove("test.sqlite")
