library(testthat)
data(mexico)
t_dat <- mexico

### ---------- T E S T S ---------- ###
test_that("database_configuration", {
  mock_mdb <<- database_configuration("csv", "test.csv")
  expect_s3_class(mock_mdb, "mdb_csv")
  expect_equal(mock_mdb$location, "test.csv")
})

test_that("coords_from_city: Found results", {
  ## Case:
  ## obtaining coords from open street maps for CITY ONLY
  found <- coords_from_city(city = t_dat$City[1],
                            country_code = t_dat$Country[1],
                            silent = TRUE)
  ## Case:
  ## obtaining coords from open street maps Using region
  found_region <- coords_from_city(city = t_dat$City[2],
                                   country_code = t_dat$Country[2],
                                   region = t_dat$Region[2],
                                   silent = TRUE)
  ## Case:
  ## obtaining coords from open street maps Using state
  found_state <- coords_from_city(city = t_dat$City[2],
                                  country_code = t_dat$Country[2],
                                  state = t_dat$Region[2],
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
  not_found <- coords_from_city(city = "Mex. City",
                                country_code = t_dat$Country[1],
                                state = t_dat$Region[1],
                                silent = TRUE)
  ## Case:
  ## Not found coords due to wrong state or region
  not_found_state <- coords_from_city(city = t_dat$City[1],
                                      country_code = t_dat$Country[1],
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
  City = ifelse(City == "Ciudad de Mexico", "CD Mex", City))

api_to_db(mock_mdb,
          dat = mock_data,
          city = "City",
          country = "Country",
          state = "Region",
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
    expect_equal(nrow(db_as_df), 10)
    expect_equal(nrow(filter(db_as_df, is.na(lat))), 4)
    expect_equal(ncol(db_as_df), 9)
    expect_vector(db_as_df$City, ptype = character())
    expect_vector(db_as_df$lon, ptype = double())
    expect_vector(db_as_df$lat, ptype = double())
    expect_setequal(unique(db_as_df$Country), "MX")
})


test_that("db_remove_empty", {
    db_remove_empty(mock_mdb)
    db_as_df <- db_load(mock_mdb)
    ## TESTS
    expect_equal(nrow(db_as_df), 6)
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
    expect_equal(nrow(missing), 4)
    expect_equal(ncol(missing), 9)
    expect_vector(missing$City, ptype = character())
    expect_setequal(missing$Country, rep("MX", 4))
})


test_that("db_join_original_data: from data.frame", {
    combined <- db_join_original_data(mdb = mock_mdb,
                                      original_data = mock_data)
    ## TESTS
    expect_s3_class(combined, "data.frame")
    expect_equal(nrow(combined), 6)
    expect_equal(ncol(combined), 14)
    expect_vector(combined$City, ptype = character())
    expect_vector(combined$lon, ptype = double())
    expect_vector(combined$lat, ptype = double())
    expect_setequal(combined$Country, rep("MX", 6))
})


test_that("add_coords_manually", {
    to_add <- data.frame(ID = 1, City = "CD Mex",
                         Country = "MX", Region = "",
                         State = "Mexico", County = "",
                         osm_name = "", lon = 12, lat = 13)
    add_coords_manually(to_add, mock_mdb)
    df_added <- db_load(mock_mdb)
    api_to_db(mock_mdb, mock_data, state = "Region", silent = TRUE)
    df_complete <- db_load(mock_mdb)
    expect_equal(nrow(df_added), 7)
    expect_equal(nrow(df_complete), 10)
})


test_that("api_no_city", {
  new_state <- data.frame(ID = 11,
                          Name = "org11",
                          Type = "none",
                          Registration_year = 2005,
                          End_year = NA,
                          Country = "MX",
                          Region = "Tlaxcala",
                          City = NA,
                          Source = "none")
  api_no_city(mock_mdb, new_state, "Country", state = "Region", silent = T)
  df_added <- db_load(mock_mdb)
  expect_equal(nrow(df_added), 11)
})

file.remove("test.csv")