test_that("validate_teamtv_shots errors on non-TeamTV data", {
  expect_error(
    validate_teamtv_shots(cars),
    "Column-name mismatch"
  )
})

test_that("validate_teamtv_shots accepts the packaged shots dataset", {
  # assuming your package dataset is called `shots`
  expect_silent(validate_teamtv_shots(shots))
})

test_that("validate_teamtv_shots errors on invalid coded values", {
  bad <- shots
  bad$pressure[1] <- "SUPERHIGH"  # not allowed

  expect_error(
    validate_teamtv_shots(bad),
    "Unknown pressure value"
  )
})

test_that("validate_teamtv_shots errors on wrong types", {
  bad <- shots
  bad$distance <- as.character(bad$distance)  # should be numeric

  expect_error(
    validate_teamtv_shots(bad),
    "Type mismatch"
  )
})

