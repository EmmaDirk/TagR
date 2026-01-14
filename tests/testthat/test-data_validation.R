# tests/testthat/test-validate_teamtv_shots.R
# unit tests for validate_teamtv_shots()

testthat::test_that("validate_teamtv_shots errors on non-TeamTV data", {
  # cars is a built in dataset and will not have the expected teamtv schema
  # the validator should fail at the column name check with a column name mismatch message
  testthat::expect_error(
    validate_teamtv_shots(cars),
    "Column-name mismatch"
  )
})

testthat::test_that("validate_teamtv_shots accepts the packaged shots dataset", {
  # this is the happy path for the packaged example dataset
  # it should not error and should be silent meaning validation passed
  # if this fails it usually means the expected schema in the validator is out of sync
  testthat::expect_silent(validate_teamtv_shots(shots))
})

testthat::test_that("validate_teamtv_shots accepts reordered columns", {
  # teamtv sometimes changes export order without changing column names
  # validator should accept this by reordering internally

  df <- shots

  # mimic the newer teamtv order where opponent columns appear earlier
  new_order <- c(
    "X",
    "sporting_event_id",
    "sporting_event_name",
    "sporting_event_scheduled_at",
    "observation_id",
    "clock_id",
    "start_time",
    "end_time",
    "code",
    "description",
    "possession_id",
    "team_id",
    "team_name",
    "team_ground",
    "position",
    "team_name_full",
    "team_key",
    "person_id",
    "first_name",
    "last_name",
    "number",
    "full_name",
    "opponent_person_id",
    "opponent_first_name",
    "opponent_last_name",
    "opponent_number",
    "opponent_full_name",
    "leg",
    "type",
    "angle",
    "result",
    "distance",
    "pressure",
    "x",
    "y",
    "participantsPersonIds",
    "shot_count"
  )

  df <- df[, new_order, drop = FALSE]

  testthat::expect_silent(validate_teamtv_shots(df))
})

testthat::test_that("validate_teamtv_shots errors on invalid coded values", {
  # this test checks the allowed value validation for coded columns
  # pressure has a strict allowed set and this test injects an invalid code
  # the validator should catch it and error with an unknown pressure value message

  bad <- shots
  bad$pressure[1] <- "SUPERHIGH"

  testthat::expect_error(
    validate_teamtv_shots(bad),
    "Unknown pressure value"
  )
})

testthat::test_that("validate_teamtv_shots errors on wrong types", {
  # this test checks the type validation logic
  # distance is expected to be numeric and this test forces it to character
  # the validator should error and include a type mismatch message

  bad <- shots
  bad$distance <- as.character(bad$distance)

  testthat::expect_error(
    validate_teamtv_shots(bad),
    "Type mismatch"
  )
})
