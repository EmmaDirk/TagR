# tests for the TeamTV dashboard functions
# these are unit tests using testthat and the packaged example dataset `shots`

test_that("team dashboard rejects non-TeamTV data", {

  # cars is a base R dataset and should fail schema validation
  testthat::expect_error(
    tagr_team_dashboad(cars),
    "Column-name mismatch"
  )
})

test_that("player dashboard rejects non-TeamTV data", {

  # cars is a base R dataset and should fail schema validation
  testthat::expect_error(
    tagr_player_dashboard(cars, player = "1"),
    "Column-name mismatch"
  )
})

test_that("team dashboard errors on invalid max_players", {

  # max_players must be a single number >= 1
  testthat::expect_error(
    tagr_team_dashboad(shots, max_players = 0),
    "max_players"
  )

  testthat::expect_error(
    tagr_team_dashboad(shots, max_players = NA),
    "max_players"
  )
})

test_that("player dashboard errors when player is missing", {

  # player is required
  testthat::expect_error(
    tagr_player_dashboard(shots),
    "player"
  )

  testthat::expect_error(
    tagr_player_dashboard(shots, player = ""),
    "player"
  )
})

test_that("team dashboard returns patchwork or list of ggplots", {

  out <- tagr_team_dashboad(shots, max_players = 5)

  # if patchwork is installed it returns a patchwork object, otherwise it returns a list of ggplots
  if (requireNamespace("patchwork", quietly = TRUE)) {
    expect_true(inherits(out, "patchwork") || inherits(out, "ggplot"))
  } else {
    expect_type(out, "list")
    expect_true(all(vapply(out, inherits, logical(1), "ggplot")))
  }
})

test_that("player dashboard returns patchwork or list of ggplots", {

  nm <- unique(shots$full_name)
  nm <- nm[!is.na(nm)]
  testthat::skip_if(length(nm) == 0)

  out <- tagr_player_dashboard(shots, player = nm[1])

  # if patchwork is installed it returns a patchwork object, otherwise it returns a list of ggplots
  if (requireNamespace("patchwork", quietly = TRUE)) {
    expect_true(inherits(out, "patchwork") || inherits(out, "ggplot"))
  } else {
    expect_type(out, "list")
    expect_true(all(vapply(out, inherits, logical(1), "ggplot")))
  }
})

test_that("player dashboard can filter to one player by number", {

  # pick a number that exists and ensure the function runs
  nm <- unique(shots$number)
  nm <- nm[!is.na(nm)]
  testthat::skip_if(length(nm) == 0)

  out <- tagr_player_dashboard(shots, player = nm[1])

  if (requireNamespace("patchwork", quietly = TRUE)) {
    expect_true(inherits(out, "patchwork") || inherits(out, "ggplot"))
  } else {
    expect_type(out, "list")
    expect_true(all(vapply(out, inherits, logical(1), "ggplot")))
  }
})

test_that("player dashboard can filter to one player by fuzzy name", {

  # pick a name fragment and ensure the function runs
  nm <- unique(shots$full_name)
  nm <- nm[!is.na(nm)]
  testthat::skip_if(length(nm) == 0)

  frag <- substr(nm[1], 1, max(1, nchar(nm[1]) - 2))
  out <- tagr_player_dashboard(shots, player = frag)

  if (requireNamespace("patchwork", quietly = TRUE)) {
    expect_true(inherits(out, "patchwork") || inherits(out, "ggplot"))
  } else {
    expect_type(out, "list")
    expect_true(all(vapply(out, inherits, logical(1), "ggplot")))
  }
})

test_that("player dashboard errors on bad fuzzy name", {

  # a very unlikely name should fail the fuzzy matching threshold
  testthat::expect_error(
    tagr_player_dashboard(shots, player = "this name should never match"),
    "No close match"
  )
})
