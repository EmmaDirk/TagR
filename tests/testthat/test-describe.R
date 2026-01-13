test_that("team overview tables rejects non-TeamTV data", {
  testthat::expect_error(
    tagr_team_overview_tables(cars),
    "Column-name mismatch"
  )
})

test_that("team overview plot rejects non-TeamTV data", {
  testthat::expect_error(
    tagr_team_overview_plot(cars, "distance"),
    "Column-name mismatch"
  )
})

test_that("team overview tables returns expected structure", {
  tabs <- tagr_team_overview_tables(shots)

  expect_type(tabs, "list")
  expect_true(all(c("pressure", "distance", "type", "shot_count", "leg") %in% names(tabs)))

  for (nm in c("pressure", "distance", "type", "shot_count", "leg")) {
    expect_s3_class(tabs[[nm]], "data.frame")
    expect_true(all(c("level", "shots", "goals", "pct_goal") %in% names(tabs[[nm]])))
  }
})

test_that("team overview tables returns numeric pct_goal within bounds", {
  tabs <- tagr_team_overview_tables(shots)

  for (nm in names(tabs)) {
    pct <- tabs[[nm]]$pct_goal
    expect_true(is.numeric(pct))
    expect_true(all(pct >= 0 & pct <= 100, na.rm = TRUE))
  }
})

test_that("team overview plot returns a ggplot", {
  p <- tagr_team_overview_plot(shots, "distance")
  expect_s3_class(p, "ggplot")
})

test_that("team overview plot split_by_player returns a ggplot", {
  testthat::skip_if(all(is.na(shots$full_name)))

  p <- tagr_team_overview_plot(shots, "type", split_by_player = TRUE, max_players = 5)
  expect_s3_class(p, "ggplot")
})

test_that("team overview tables can filter to one player by number", {
  nm <- unique(shots$number)
  nm <- nm[!is.na(nm)]
  testthat::skip_if(length(nm) == 0)

  tabs <- tagr_team_overview_tables(shots, player = nm[1])
  expect_type(tabs, "list")
  expect_true(all(c("pressure", "distance", "type", "shot_count", "leg") %in% names(tabs)))
})

test_that("team overview tables can filter to one player by fuzzy name", {
  nm <- unique(shots$full_name)
  nm <- nm[!is.na(nm)]
  testthat::skip_if(length(nm) == 0)

  frag <- substr(nm[1], 1, max(1, nchar(nm[1]) - 2))
  tabs <- tagr_team_overview_tables(shots, player = frag)
  expect_type(tabs, "list")
  expect_true(all(c("pressure", "distance", "type", "shot_count", "leg") %in% names(tabs)))
})

test_that("team overview tables split_by_player returns list of players", {
  testthat::skip_if(all(is.na(shots$full_name)))

  tabs_split <- tagr_team_overview_tables(shots, split_by_player = TRUE, max_players = 3)
  expect_type(tabs_split, "list")
  expect_true(length(tabs_split) > 0)

  first <- tabs_split[[1]]
  expect_type(first, "list")
  expect_true(all(c("pressure", "distance", "type", "shot_count", "leg") %in% names(first)))
})

test_that("team overview plot errors on bad attribute", {
  testthat::expect_error(
    tagr_team_overview_plot(shots, attribute = "not_an_attribute"),
    "'arg' should be one of"
  )
})
