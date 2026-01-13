# Plot scoring probability vs distance (binomial model)

Fits a logistic regression of GOAL vs distance and plots points + fitted
curve. Distance is capped at 10 meters (values above 10 are set to 10).
If player is provided, the plot includes both the player line and the
team line, and colors points so it is clear which points are player vs
team. Optionally restricts to LONG and SHORT shots only.

## Usage

``` r
tagr_plot_prob_by_distance(
  df,
  player = NULL,
  add_team_line = TRUE,
  long_short_only = FALSE
)
```

## Arguments

- df:

  A TeamTV shots data.frame.

- player:

  Optional. Player name (fuzzy match) or shirt number (exact match).
  Default NULL for all players.

- add_team_line:

  Logical. If TRUE and player is not NULL, add a team fitted line for
  comparison.

- long_short_only:

  Logical. If TRUE, keep only type in c("LONG","SHORT") before fitting.

## Value

A ggplot object.

## Details

Input validation is performed using validate_teamtv_shots().
