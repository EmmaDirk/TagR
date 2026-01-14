# Shot analysis goal probability vs distance pressure shot count

fits a binomial logistic regression of scoring goal vs miss against one
predictor then plots the observed shots plus the fitted probability
curve or fitted points

## Usage

``` r
tagr_shot_analysis(
  df,
  feature = c("distance", "pressure", "shot_count"),
  players = NULL,
  add_team = TRUE,
  long_short_only = FALSE
)
```

## Arguments

- df:

  A TeamTV shots data.frame.

- feature:

  One of distance pressure shot_count.

- players:

  Optional NULL for team only or a vector length 1 to 3 of player names
  or shirt numbers.

- add_team:

  Logical if true and players is not null include team series too.

- long_short_only:

  Logical if true keep only type in c(long short) before fitting.

## Value

A ggplot object.

## Details

predictor choice is controlled by feature

- distance uses distance capped at 10 meters with distance_cap =
  pmin(distance, 10)

- pressure uses an ordered factor none then medium then high

- shot_count is binned as 1 2 3 4+

player comparison

- players can be null for team only or a vector of 1 to 3 players by
  name or number

- if players is provided and add_team is true a team series is included
  for comparison

- legend shows team and player labels the title never includes player
  names

missing and unknown handling

- unknown result onbekend is dropped then result must contain both goal
  and miss

- rows missing the chosen feature are dropped

- for pressure onbekend and values outside none medium high are dropped

- if a player has no shots in a given pressure or shot count level that
  level is omitted this avoids predict errors from new unused levels

input validation

- calls validate_teamtv_shots(df) before any processing
