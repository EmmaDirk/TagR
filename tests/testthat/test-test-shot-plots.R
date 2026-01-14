test_that("heatmap rejects non-TeamTV data", {
  testthat::expect_error(
    tagr_heatmap(cars),
    "Column-name mismatch"
  )
})

test_that("heatmap returns a ggplot (result)", {
  p <- tagr_heatmap(shots, colour_by = "result")
  expect_s3_class(p, "ggplot")
})

test_that("heatmap returns a ggplot (type)", {
  p <- tagr_heatmap(shots, colour_by = "type")
  expect_s3_class(p, "ggplot")
})

test_that("heatmap returns a ggplot (pressure)", {
  p <- tagr_heatmap(shots, colour_by = "pressure")
  expect_s3_class(p, "ggplot")
})

test_that("heatmap returns a ggplot (leg)", {
  p <- tagr_heatmap(shots, colour_by = "leg")
  expect_s3_class(p, "ggplot")
})

test_that("player filter by number works", {
  nm <- unique(shots$number)
  nm <- nm[!is.na(nm)]
  testthat::skip_if(length(nm) == 0)

  p <- tagr_heatmap(shots, colour_by = "result", player = nm[1])
  expect_s3_class(p, "ggplot")
})

test_that("player filter by fuzzy name works", {
  nm <- unique(shots$full_name)
  nm <- nm[!is.na(nm)]
  testthat::skip_if(length(nm) == 0)

  first <- nm[1]
  frag <- substr(first, 1, max(1, nchar(first) - 2))

  p <- tagr_heatmap(shots, colour_by = "result", player = frag)
  expect_s3_class(p, "ggplot")
})

test_that("player filter by multiple players works", {
  nm <- unique(shots$number)
  nm <- nm[!is.na(nm)]
  testthat::skip_if(length(nm) < 2)

  p <- tagr_heatmap(shots, colour_by = "result", player = c(nm[1], nm[2]))
  expect_s3_class(p, "ggplot")
})

test_that("unknown player name errors", {
  testthat::expect_error(
    tagr_heatmap(shots, colour_by = "result", player = "Definitely Not A Player"),
    "No close match"
  )
})

test_that("filters can error to empty via filter=list(...)", {
  testthat::skip_if(!"pressure" %in% names(shots))
  testthat::skip_if(all(is.na(shots$pressure)))

  bad_pressure <- "THIS-IS-NOT-A-VALID-PRESSURE"
  testthat::expect_error(
    tagr_heatmap(shots, colour_by = "result", filter = list(pressure = bad_pressure)),
    "No rows left after filtering"
  )
})

test_that("result is a filter option for non-result plots", {
  testthat::skip_if(!"result" %in% names(shots))
  testthat::skip_if(all(is.na(shots$result)))

  p <- tagr_heatmap(shots, colour_by = "type", filter = list(result = "GOAL"))
  expect_s3_class(p, "ggplot")
})

test_that("type is a filter option for non-type plots", {
  testthat::skip_if(!"type" %in% names(shots))
  testthat::skip_if(all(is.na(shots$type)))

  p <- tagr_heatmap(shots, colour_by = "pressure", filter = list(type = "SHORT"))
  expect_s3_class(p, "ggplot")
})

test_that("bad filter name errors", {
  testthat::expect_error(
    tagr_heatmap(shots, colour_by = "result", filter = list(not_a_real_filter = "x")),
    "Unknown filter name"
  )
})

test_that("filter must be a list", {
  testthat::expect_error(
    tagr_heatmap(shots, colour_by = "result", filter = "goal"),
    "`filter` must be a named list"
  )
})
