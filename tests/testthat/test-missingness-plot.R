test_that("missingness plot rejects non-TeamTV data", {
  testthat::expect_error(
    tagr_plot_missingness(cars),
    "Column-name mismatch"
  )
})

test_that("missingness plot rejects TeamTV-like data with unknown codes", {
  df <- shots
  df$pressure[1] <- "SUPERHIGH"

  testthat::expect_error(
    tagr_plot_missingness(df),
    "Unknown pressure value"
  )
})

test_that("missingness plot returns a ggplot", {
  p <- tagr_plot_missingness(shots)
  expect_s3_class(p, "ggplot")
})

test_that("missingness plot works for a single player", {
  nm <- unique(shots$full_name)
  nm <- nm[!is.na(nm)]
  testthat::skip_if(length(nm) == 0)

  p <- tagr_plot_missingness(shots, person = nm[1])
  expect_s3_class(p, "ggplot")
})

test_that("unknown player throws an error", {
  testthat::expect_error(
    tagr_plot_missingness(shots, person = "Definitely Not A Player"),
    "No close match"
  )
})

test_that("onbekend values are treated as missing", {
  df <- shots
  df$opponent_full_name <- "onbekend"

  p <- tagr_plot_missingness(df)
  expect_s3_class(p, "ggplot")
})