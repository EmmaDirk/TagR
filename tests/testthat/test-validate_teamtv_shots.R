# this test checks that validate_teamtv_shots rejects data that is not from teamtv
# cars is a built in dataset and will not have the expected teamtv schema
# the validator should fail at the column name check with a column name mismatch message
test_that("validate_teamtv_shots errors on non-TeamTV data", {
  expect_error(
    validate_teamtv_shots(cars),
    "Column-name mismatch"
  )
})

# this test checks the happy path for the packaged example dataset
# it should not error and should be silent meaning validation passed
# if this fails it usually means the expected schema in the validator is out of sync
test_that("validate_teamtv_shots accepts the packaged shots dataset", {
  # assuming your package dataset is called shots
  expect_silent(validate_teamtv_shots(shots))
})

# this test checks the allowed value validation for coded columns
# pressure has a strict allowed set and this test injects an invalid code
# the validator should catch it and error with an unknown pressure value message
test_that("validate_teamtv_shots errors on invalid coded values", {
  bad <- shots
  # overwrite one value with a code that is not part of the allowed set
  bad$pressure[1] <- "SUPERHIGH"

  expect_error(
    validate_teamtv_shots(bad),
    "Unknown pressure value"
  )
})

# this test checks the type validation logic
# distance is expected to be numeric and this test forces it to character
# the validator should error and include a type mismatch message
test_that("validate_teamtv_shots errors on wrong types", {
  bad <- shots
  # convert numeric column into character to simulate a wrong read from csv
  bad$distance <- as.character(bad$distance)

  expect_error(
    validate_teamtv_shots(bad),
    "Type mismatch"
  )
})
