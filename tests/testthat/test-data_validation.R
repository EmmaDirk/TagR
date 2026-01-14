# tests/testthat/test-validate_teamtv_shots.R
# this test file checks validate_teamtv_shots()

testthat::test_that("validate_teamtv_shots errors on non-TeamTV data", {
  # cars is a built in dataset and will not have the expected teamtv core columns
  testthat::expect_error(
    validate_teamtv_shots(cars),
    "Missing required columns"
  )
})

testthat::test_that("validate_teamtv_shots accepts the packaged shots dataset", {
  # happy path for the packaged example dataset
  testthat::expect_silent(validate_teamtv_shots(shots))
})

testthat::test_that("validate_teamtv_shots warns when optional columns are missing", {
  # simulate a teamtv export that omits opponent columns
  df <- shots
  drop_cols <- c(
    "opponent_person_id", "opponent_first_name", "opponent_last_name",
    "opponent_number", "opponent_full_name"
  )
  df <- df[, setdiff(names(df), drop_cols), drop = FALSE]

  testthat::expect_warning(
    validate_teamtv_shots(df),
    "Missing important columns"
  )
})

testthat::test_that("validate_teamtv_shots errors when a required core column is missing", {
  # x and y are required core columns for core TagR plotting
  df <- shots
  df <- df[, setdiff(names(df), c("x")), drop = FALSE]

  testthat::expect_error(
    validate_teamtv_shots(df),
    "Missing required columns"
  )
})

testthat::test_that("validate_teamtv_shots errors on invalid coded values when the column exists", {
  # pressure exists in shots so an invalid value should be rejected
  bad <- shots
  bad$pressure[1] <- "SUPERHIGH"

  testthat::expect_error(
    validate_teamtv_shots(bad),
    "Unknown pressure value"
  )
})

testthat::test_that("validate_teamtv_shots errors on wrong types", {
  # distance is expected to be numeric and this test forces it to character
  bad <- shots
  bad$distance <- as.character(bad$distance)

  testthat::expect_error(
    validate_teamtv_shots(bad),
    "Type mismatch"
  )
})
