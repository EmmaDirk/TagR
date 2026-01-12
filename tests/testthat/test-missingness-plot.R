test_that("missingness plot returns a ggplot", {
  p <- tagr_plot_missingness(anon)
  expect_s3_class(p, "ggplot")
})

test_that("missingness plot works for a single player", {
  nm <- unique(anon$full_name)
  nm <- nm[!is.na(nm)]
  testthat::skip_if(length(nm) == 0)

  p <- tagr_plot_missingness(anon, person = nm[1])
  expect_s3_class(p, "ggplot")
})

test_that("unknown player throws an error", {
  testthat::expect_error(
    tagr_plot_missingness(anon, person = "Definitely Not A Player"),
    "No close match"
  )
})

test_that("onbekend values are treated as missing", {
  df <- anon
  df$opponent_full_name <- "onbekend"

  p <- tagr_plot_missingness(df)
  expect_s3_class(p, "ggplot")
})
