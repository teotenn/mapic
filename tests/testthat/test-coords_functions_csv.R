library(testthat)

the_file <- "test_file.csv"
t_dat <- read.csv(the_file)


api_to_db(db_name = "test.sqlite",
               dat = t_dat,
               city = "City",
               country = "Country",
               state = "Region",
               db_backup_after = 5,
               silent = TRUE)

test_that("remove_na_from_db", {
    remove_na_from_db("test.sqlite")
    db_as_df <- import_db_as_df("test.sqlite")
    ## TESTS
    expect_equal(nrow(db_as_df), 6)
    expect_equal(nrow(filter(db_as_df, is.na(lat))), 0)
})


test_that("compare_db_data: returns a data frame", {
    missing <- compare_db_data("test.sqlite", the_file)
    ## TESTS
    expect_s3_class(missing, "data.frame")
    expect_equal(nrow(missing), 4)
    expect_equal(ncol(missing), 9)
    expect_vector(missing$City, ptype = character())
    expect_setequal(missing$Country, rep("MX", 4))
})


test_that("combine_csv_sql: from data.frame", {
    combined <- combine_csv_sql(db_file = "test.sqlite",
                                csv_file = the_file)
    ## TESTS
    expect_s3_class(combined, "data.frame")
    expect_equal(nrow(combined), 6)
    expect_equal(ncol(combined), 14)
    expect_vector(combined$City, ptype = character())
    expect_vector(combined$lon, ptype = double())
    expect_setequal(combined$Country, rep("MX", 6))
})


file.remove("test.sqlite")
