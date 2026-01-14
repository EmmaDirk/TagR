# tests/testthat/test-analyze.R
# unit tests for tagr_shot_analysis

testthat::test_that("distance returns ggplot and caps distance at 10", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("dplyr")

  # create a local copy of tagr_shot_analysis with its own function environment
  # this prevents mocked validate_teamtv_shots from leaking into other tests
  local_shot_analysis <- function(validator) {
    # grab the real function from the package
    f <- TagR::tagr_shot_analysis

    # copy it so we can safely change its environment
    f2 <- f

    # make a new environment that inherits from the original function environment
    e <- new.env(parent = environment(f))

    # inject a test validator into that new environment
    e$validate_teamtv_shots <- validator

    # attach the new environment to the copied function
    environment(f2) <- e

    # return the isolated function copy
    f2
  }

  # counter to confirm validate_teamtv_shots is called once
  called <- 0L

  # build an isolated analysis function that increments the counter
  sa <- local_shot_analysis(function(df) { called <<- called + 1L; TRUE })

  # create a minimal dataset for the distance branch including values above 10 and missing values
  df <- data.frame(
    result     = c("goal","miss","goal","miss","onbekend","miss","goal","miss"),
    distance   = c(2, 5, 12, 10, 3, NA, 8, 15),
    pressure   = c("none","medium","high","none","none","high","medium","none"),
    shot_count = c(1,2,3,4,1,2,5,3),
    type       = c("long","short","long","short","long","short","long","short"),
    number     = c("10","10","11","11","10","11","10","11"),
    full_name  = c("john doe","john doe","jane roe","jane roe","john doe","jane roe","john doe","jane roe"),
    stringsAsFactors = FALSE
  )

  # run the distance analysis using the isolated function
  p <- sa(df, feature = "distance")

  # check that a ggplot is returned and the title matches
  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_identical(p$labels$title, "Scoring probability vs distance")

  # check that the validator was called exactly once
  testthat::expect_equal(called, 1L)

  # build the ggplot and check that plotted x values do not exceed 10 after capping
  b <- ggplot2::ggplot_build(p)
  x_points <- b$data[[1]]$x
  x_line   <- b$data[[2]]$x
  testthat::expect_true(max(x_points, na.rm = TRUE) <= 10 + 1e-9)
  testthat::expect_true(max(x_line,   na.rm = TRUE) <= 10 + 1e-9)
})

testthat::test_that("pressure drops unknown and invalid values and omits missing levels per player", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("dplyr")

  # build an isolated analysis function with a no op validator
  local_shot_analysis <- function(validator) {
    f <- TagR::tagr_shot_analysis
    f2 <- f
    e <- new.env(parent = environment(f))
    e$validate_teamtv_shots <- validator
    environment(f2) <- e
    f2
  }
  sa <- local_shot_analysis(function(df) TRUE)

  # create a dataset that includes invalid pressure values that should be dropped
  df <- data.frame(
    result     = c("goal","miss","goal","miss","goal","miss","goal","miss","miss","goal"),
    distance   = c(3,4,5,6,7,8,9,2,1,10),
    pressure   = c("none","medium","none","medium","high","high","onbekend","low","high","high"),
    shot_count = c(1,1,2,2,1,2,3,4,1,2),
    type       = rep("long", 10),
    number     = c("10","10","10","10","11","11","11","11","11","11"),
    full_name  = c(rep("john doe", 4), rep("jane roe", 6)),
    stringsAsFactors = FALSE
  )

  # run the pressure analysis with two players and include the team series
  p <- sa(df, feature = "pressure", players = c("10","11"), add_team = TRUE)

  # check that a ggplot is returned and the title matches
  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_identical(p$labels$title, "Scoring probability vs pressure")

  # build the plot and verify fitted points use only the three allowed factor positions
  b <- ggplot2::ggplot_build(p)
  fitted <- b$data[[2]]
  testthat::expect_true(all(fitted$x %in% 1:3))

  # verify at least one series omitted at least one pressure level
  testthat::expect_true(nrow(fitted) < 3 * length(unique(fitted$group)))
})

testthat::test_that("shot_count bins to 1 2 3 4plus and returns ggplot", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("dplyr")

  # build an isolated analysis function with a no op validator
  local_shot_analysis <- function(validator) {
    f <- TagR::tagr_shot_analysis
    f2 <- f
    e <- new.env(parent = environment(f))
    e$validate_teamtv_shots <- validator
    environment(f2) <- e
    f2
  }
  sa <- local_shot_analysis(function(df) TRUE)

  # create a dataset that includes shot_count values above 4 so the top band is used
  df <- data.frame(
    result     = c("goal","miss","goal","miss","goal","miss","goal","miss","goal","miss"),
    distance   = c(1,2,3,4,5,6,7,8,9,10),
    pressure   = rep("none", 10),
    shot_count = c(1,2,3,4,5,6,1,2,3,4),
    type       = rep("short", 10),
    number     = c(rep("10", 5), rep("11", 5)),
    full_name  = c(rep("john doe", 5), rep("jane roe", 5)),
    stringsAsFactors = FALSE
  )

  # run the shot_count analysis
  p <- sa(df, feature = "shot_count")

  # check that a ggplot is returned and the title matches
  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_identical(p$labels$title, "Scoring probability vs shot count")

  # build the plot and verify fitted x positions map to four bands and include the top band
  b <- ggplot2::ggplot_build(p)
  fitted <- b$data[[2]]
  testthat::expect_true(all(fitted$x %in% 1:4))
  testthat::expect_true(any(fitted$x == 4))
})

testthat::test_that("players supports number and fuzzy name and enforces max 3 players", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("dplyr")

  # build an isolated analysis function with a no op validator
  local_shot_analysis <- function(validator) {
    f <- TagR::tagr_shot_analysis
    f2 <- f
    e <- new.env(parent = environment(f))
    e$validate_teamtv_shots <- validator
    environment(f2) <- e
    f2
  }
  sa <- local_shot_analysis(function(df) TRUE)

  # create a dataset with two players and enough variation for fitting
  df <- data.frame(
    result     = c("goal","miss","goal","miss","goal","miss","goal","miss"),
    distance   = c(2,3,4,5,6,7,8,9),
    pressure   = c("none","none","medium","medium","high","high","none","medium"),
    shot_count = c(1,2,3,4,1,2,3,4),
    type       = rep("long", 8),
    number     = c("10","10","10","10","11","11","11","11"),
    full_name  = c(rep("john doe", 4), rep("jane roe", 4)),
    stringsAsFactors = FALSE
  )

  # run analysis selecting a player by number
  p1 <- sa(df, feature = "distance", players = "10", add_team = TRUE)
  testthat::expect_s3_class(p1, "ggplot")

  # run analysis selecting a player by fuzzy name
  p2 <- sa(df, feature = "distance", players = "jon doe", add_team = FALSE)
  testthat::expect_s3_class(p2, "ggplot")

  # verify requesting more than three players errors
  testthat::expect_error(
    sa(df, feature = "distance", players = c("a","b","c","d")),
    "up to 3 players"
  )
})

testthat::test_that("long_short_only keeps only long and short and errors if none", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("dplyr")

  # build an isolated analysis function with a no op validator
  local_shot_analysis <- function(validator) {
    f <- TagR::tagr_shot_analysis
    f2 <- f
    e <- new.env(parent = environment(f))
    e$validate_teamtv_shots <- validator
    environment(f2) <- e
    f2
  }
  sa <- local_shot_analysis(function(df) TRUE)

  # create a dataset with long and short values in varied formatting
  df_ok <- data.frame(
    result     = c("goal","miss","goal","miss"),
    distance   = c(1,2,3,4),
    pressure   = c("none","none","none","none"),
    shot_count = c(1,2,3,4),
    type       = c(" long ", "SHORT", "long", "short"),
    number     = c("10","10","11","11"),
    full_name  = c("john doe","john doe","jane roe","jane roe"),
    stringsAsFactors = FALSE
  )

  # verify long_short_only can run when long and short exist
  p <- sa(df_ok, feature = "distance", long_short_only = TRUE)
  testthat::expect_s3_class(p, "ggplot")

  # create a dataset that has no long or short types so filtering removes all rows
  df_bad <- df_ok
  df_bad$type <- c("pass","cross","header","assist")

  testthat::expect_error(
    sa(df_bad, feature = "distance", long_short_only = TRUE),
    "No LONG/SHORT shots available"
  )
})

testthat::test_that("result handling drops onbekend and requires both goal and miss", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("dplyr")

  # build an isolated analysis function with a no op validator
  local_shot_analysis <- function(validator) {
    f <- TagR::tagr_shot_analysis
    f2 <- f
    e <- new.env(parent = environment(f))
    e$validate_teamtv_shots <- validator
    environment(f2) <- e
    f2
  }
  sa <- local_shot_analysis(function(df) TRUE)

  # create a dataset with only goals so the model requirement should fail
  df_all_goal <- data.frame(
    result     = rep("goal", 6),
    distance   = c(1,2,3,4,5,6),
    pressure   = rep("none", 6),
    shot_count = c(1,1,2,2,3,4),
    type       = rep("long", 6),
    number     = rep("10", 6),
    full_name  = rep("john doe", 6),
    stringsAsFactors = FALSE
  )

  testthat::expect_error(
    sa(df_all_goal, feature = "distance"),
    "Need both GOAL and MISS"
  )

  # create a dataset where all results are onbekend so cleaning removes all usable rows
  df_only_onbekend <- df_all_goal
  df_only_onbekend$result <- rep("onbekend", 6)

  testthat::expect_error(
    sa(df_only_onbekend, feature = "distance"),
    "No non-missing result"
  )
})
