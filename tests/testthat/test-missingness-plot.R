# this test checks that the missingness plot refuses non teamtv data
# cars does not match the expected schema so validate_teamtv_shots should fail
# tagr_plot_missingness calls validate_teamtv_shots at the start so we expect that error
test_that("missingness plot rejects non-TeamTV data", {
  testthat::expect_error(
    tagr_plot_missingness(cars),
    "Column-name mismatch"
  )
})

# this test checks that even if the data has the right columns
# invalid coded values still cause an error
# tagr_plot_missingness validates coded values through validate_teamtv_shots
# pressure is a coded column so an unknown value should be rejected
test_that("missingness plot rejects TeamTV-like data with unknown codes", {
  df <- shots
  # inject an invalid pressure code that is not in the allowed set
  df$pressure[1] <- "SUPERHIGH"

  testthat::expect_error(
    tagr_plot_missingness(df),
    "Unknown pressure value"
  )
})

# this test checks the basic success case
# the function should return a ggplot object when given valid teamtv shots data
test_that("missingness plot returns a ggplot", {
  p <- tagr_plot_missingness(shots)
  expect_s3_class(p, "ggplot")
})

# this test checks the person filter path
# it uses an existing full_name value so fuzzy matching should succeed
# if the dataset has no names the test is skipped to avoid failing on missing fixtures
test_that("missingness plot works for a single player", {
  nm <- unique(shots$full_name)
  nm <- nm[!is.na(nm)]
  testthat::skip_if(length(nm) == 0)

  p <- tagr_plot_missingness(shots, person = nm[1])
  expect_s3_class(p, "ggplot")
})

# this test checks that an unknown person value produces an error
# the function uses edit distance and should stop when no close match exists
test_that("unknown player throws an error", {
  testthat::expect_error(
    tagr_plot_missingness(shots, person = "Definitely Not A Player"),
    "No close match"
  )
})

# this test checks the cleaning rule that treats onbekend as missing
# setting opponent_full_name to onbekend should be converted to na internally
# the function should still succeed and return a ggplot
test_that("onbekend values are treated as missing", {
  df <- shots
  # force a known unknown token in a character column used in the plot
  df$opponent_full_name <- "onbekend"

  p <- tagr_plot_missingness(df)
  expect_s3_class(p, "ggplot")
})
