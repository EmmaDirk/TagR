test_that("result plot rejects non-TeamTV data", {
  testthat::expect_error(
    tagr_plot_shots_result(cars),
    "Column-name mismatch"
  )
})

test_that("type plot rejects non-TeamTV data", {
  testthat::expect_error(
    tagr_plot_shots_type(cars),
    "Column-name mismatch"
  )
})

test_that("pressure plot rejects non-TeamTV data", {
  testthat::expect_error(
    tagr_plot_shots_pressure(cars),
    "Column-name mismatch"
  )
})

test_that("leg plot rejects non-TeamTV data", {
  testthat::expect_error(
    tagr_plot_shots_leg(cars),
    "Column-name mismatch"
  )
})

test_that("result plot returns a ggplot", {
  p <- tagr_plot_shots_result(shots)
  expect_s3_class(p, "ggplot")
})

test_that("type plot returns a ggplot", {
  p <- tagr_plot_shots_type(shots)
  expect_s3_class(p, "ggplot")
})

test_that("pressure plot returns a ggplot", {
  p <- tagr_plot_shots_pressure(shots)
  expect_s3_class(p, "ggplot")
})

test_that("leg plot returns a ggplot", {
  p <- tagr_plot_shots_leg(shots)
  expect_s3_class(p, "ggplot")
})

test_that("player filter by number works", {
  nm <- unique(shots$number)
  nm <- nm[!is.na(nm)]
  testthat::skip_if(length(nm) == 0)

  p <- tagr_plot_shots_result(shots, player = nm[1])
  expect_s3_class(p, "ggplot")
})

test_that("player filter by fuzzy name works", {
  nm <- unique(shots$full_name)
  nm <- nm[!is.na(nm)]
  testthat::skip_if(length(nm) == 0)

  first <- nm[1]
  frag <- substr(first, 1, max(1, nchar(first) - 2))

  p <- tagr_plot_shots_result(shots, player = frag)
  expect_s3_class(p, "ggplot")
})

test_that("unknown player name errors", {
  testthat::expect_error(
    tagr_plot_shots_result(shots, player = "Definitely Not A Player"),
    "No close match"
  )
})

test_that("filters reduce data and can error to empty", {
  testthat::skip_if(!"pressure" %in% names(shots))
  testthat::skip_if(all(is.na(shots$pressure)))

  bad_pressure <- "THIS-IS-NOT-A-VALID-PRESSURE"
  testthat::expect_error(
    tagr_plot_shots_result(shots, pressure = bad_pressure),
    "No rows left after filtering"
  )
})

test_that("result is a filter option for non-result plots", {
  testthat::skip_if(!"result" %in% names(shots))
  testthat::skip_if(all(is.na(shots$result)))

  p <- tagr_plot_shots_type(shots, result = "GOAL")
  expect_s3_class(p, "ggplot")
})

test_that("type is a filter option for non-type plots", {
  testthat::skip_if(!"type" %in% names(shots))
  testthat::skip_if(all(is.na(shots$type)))

  p <- tagr_plot_shots_pressure(shots, type = "SHORT")
  expect_s3_class(p, "ggplot")
})
