# Team overview plot for one attribute

Plots goal percentage by one attribute (pressure, distance, type,
shot_count, leg). If split_by_player is TRUE, each player is a different
color with a legend.

## Usage

``` r
tagr_team_overview_plot(
  df,
  attribute = c("pressure", "distance", "type", "shot_count", "leg"),
  player = NULL,
  split_by_player = FALSE,
  max_players = 10,
  filter_type = NULL,
  filter_pressure = NULL,
  filter_leg = NULL,
  filter_result = NULL,
  filter_distance_band = NULL,
  filter_shot_count_band = NULL,
  exclude_types = NULL
)
```

## Arguments

- df:

  A TeamTV shots data.frame.

- attribute:

  One of: "pressure", "distance", "type", "shot_count", "leg".

- player:

  Optional. Player name (fuzzy match) or shirt number (exact match).
  Default NULL for all players.

- split_by_player:

  Logical. If TRUE, show players as different colors.

- max_players:

  Integer. When split_by_player is TRUE, limit to the top N players by
  number of shots.

- filter_type:

  Optional character vector. Keep only these shot types.

- filter_pressure:

  Optional character vector. Keep only these pressure values.

- filter_leg:

  Optional character vector. Keep only these leg values.

- filter_result:

  Optional character vector. Keep only these results.

- filter_distance_band:

  Optional character vector. Keep only these distance bands.

- filter_shot_count_band:

  Optional character vector. Keep only these shot count bands.

- exclude_types:

  Optional character vector. Shot types to exclude.

## Value

A ggplot object.
