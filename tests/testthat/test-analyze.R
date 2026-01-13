testthat::test_that("analyze plots reject bad data via validate_teamtv_shots()", {
  bad <- mtcars

  testthat::expect_error(
    TagR::tagr_plot_prob_by_distance(bad),
    "Column-name mismatch\\.",
    ignore.case = FALSE
  )

  testthat::expect_error(
    TagR::tagr_plot_prob_by_pressure(bad),
    "Column-name mismatch\\.",
    ignore.case = FALSE
  )

  testthat::expect_error(
    TagR::tagr_plot_prob_by_shot_count(bad),
    "Column-name mismatch\\.",
    ignore.case = FALSE
  )
})

testthat::test_that("analyze plots error on invalid categorical values (validate_teamtv_shots)", {
  df <- TagR::shots

  df$pressure[1] <- "SUPERHIGH"
  testthat::expect_error(
    TagR::tagr_plot_prob_by_pressure(df),
    "Unknown pressure value\\(s\\)",
    ignore.case = TRUE
  )

  df <- TagR::shots
  df$type[1] <- "MEGASHOT"
  testthat::expect_error(
    TagR::tagr_plot_prob_by_distance(df, long_short_only = TRUE),
    "Unknown shot type value\\(s\\)",
    ignore.case = TRUE
  )
})
