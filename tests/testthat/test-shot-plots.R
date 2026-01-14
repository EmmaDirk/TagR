# this test checks that the function refuses inputs that are not teamtv shots data
# cars is a built in dataset and should not match the expected teamtv columns
# validate_teamtv_shots is expected to throw an error mentioning column name mismatch
test_that("heatmap rejects non-TeamTV data", {
  testthat::expect_error(
    tagr_heatmap(cars),
    "Column-name mismatch"
  )
})

# this test checks the main success case for colouring by result
# it should return a ggplot object so it can be printed or extended with more layers
test_that("heatmap returns a ggplot (result)", {
  p <- tagr_heatmap(shots, colour_by = "result")
  expect_s3_class(p, "ggplot")
})

# this test checks colouring by type also returns a ggplot
# it ensures colour_by accepts type and the function does not error
test_that("heatmap returns a ggplot (type)", {
  p <- tagr_heatmap(shots, colour_by = "type")
  expect_s3_class(p, "ggplot")
})

# this test checks colouring by pressure returns a ggplot
# it ensures the pressure column can be used for colour mapping
test_that("heatmap returns a ggplot (pressure)", {
  p <- tagr_heatmap(shots, colour_by = "pressure")
  expect_s3_class(p, "ggplot")
})

# this test checks colouring by leg returns a ggplot
# it ensures the leg column can be used for colour mapping
test_that("heatmap returns a ggplot (leg)", {
  p <- tagr_heatmap(shots, colour_by = "leg")
  expect_s3_class(p, "ggplot")
})

# this test verifies player filtering by number works
# it picks the first available non missing number in the dataset
# if there are no numbers the test is skipped to avoid failing on missing fixtures
test_that("player filter by number works", {
  nm <- unique(shots$number)
  nm <- nm[!is.na(nm)]
  testthat::skip_if(length(nm) == 0)

  p <- tagr_heatmap(shots, colour_by = "result", player = nm[1])
  expect_s3_class(p, "ggplot")
})

# this test verifies fuzzy matching by name works
# it takes the first available full_name and removes a couple of characters
# the edit distance should still be within the default max_dist so a plot is returned
# if there are no names the test is skipped
test_that("player filter by fuzzy name works", {
  nm <- unique(shots$full_name)
  nm <- nm[!is.na(nm)]
  testthat::skip_if(length(nm) == 0)

  first <- nm[1]
  frag <- substr(first, 1, max(1, nchar(first) - 2))

  p <- tagr_heatmap(shots, colour_by = "result", player = frag)
  expect_s3_class(p, "ggplot")
})

# this test verifies filtering with multiple players works
# it requires at least two distinct player numbers
# if fewer than two are present the test is skipped
test_that("player filter by multiple players works", {
  nm <- unique(shots$number)
  nm <- nm[!is.na(nm)]
  testthat::skip_if(length(nm) < 2)

  p <- tagr_heatmap(shots, colour_by = "result", player = c(nm[1], nm[2]))
  expect_s3_class(p, "ggplot")
})

# this test verifies that an unknown player name triggers an error
# the function should attempt fuzzy matching and then stop when no close match exists
test_that("unknown player name errors", {
  testthat::expect_error(
    tagr_heatmap(shots, colour_by = "result", player = "Definitely Not A Player"),
    "No close match"
  )
})

# this test verifies that filters can legitimately remove all rows and produce an error
# it only runs if a pressure column exists and has at least one non missing value
# it passes a made up pressure code so tagr_apply_filters returns zero rows
test_that("filters can error to empty via filter=list(...)", {
  testthat::skip_if(!"pressure" %in% names(shots))
  testthat::skip_if(all(is.na(shots$pressure)))

  bad_pressure <- "THIS-IS-NOT-A-VALID-PRESSURE"
  testthat::expect_error(
    tagr_heatmap(shots, colour_by = "result", filter = list(pressure = bad_pressure)),
    "No rows left after filtering"
  )
})

# this test checks that result can be used as a filter when we are not colouring by result
# because colour_by is type here the result filter should be applied
# the function should still return a ggplot if at least one goal exists
test_that("result is a filter option for non-result plots", {
  testthat::skip_if(!"result" %in% names(shots))
  testthat::skip_if(all(is.na(shots$result)))

  p <- tagr_heatmap(shots, colour_by = "type", filter = list(result = "GOAL"))
  expect_s3_class(p, "ggplot")
})

# this test checks that type can be used as a filter when we are not colouring by type
# because colour_by is pressure here the type filter should be applied
# the function should return a ggplot if at least one short type shot exists
test_that("type is a filter option for non-type plots", {
  testthat::skip_if(!"type" %in% names(shots))
  testthat::skip_if(all(is.na(shots$type)))

  p <- tagr_heatmap(shots, colour_by = "pressure", filter = list(type = "SHORT"))
  expect_s3_class(p, "ggplot")
})

# this test checks that unsupported filter names are rejected
# the function validates filter names and should error with a message about unknown names
test_that("bad filter name errors", {
  testthat::expect_error(
    tagr_heatmap(shots, colour_by = "result", filter = list(not_a_real_filter = "x")),
    "Unknown filter name"
  )
})

# this test checks that filter must be a list
# passing a string should trigger the explicit type check error message
test_that("filter must be a list", {
  testthat::expect_error(
    tagr_heatmap(shots, colour_by = "result", filter = "goal"),
    "`filter` must be a named list"
  )
})


