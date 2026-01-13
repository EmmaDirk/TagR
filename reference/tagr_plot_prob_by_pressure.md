# Plot scoring probability vs pressure (binomial model)

Fits a logistic regression of GOAL vs ordered pressure and plots
observed + fitted. Pressure is treated as ordered: NONE \< MEDIUM \<
HIGH. If player is provided, adds a team fitted point series for
comparison. Optionally restricts to LONG and SHORT shots only.

## Usage

``` r
tagr_plot_prob_by_pressure(
  df,
  player = NULL,
  long_short_only = FALSE,
  add_team_points = TRUE
)
```

## Arguments

- df:

  A TeamTV shots data.frame.

- player:

  Optional. Player name (fuzzy match) or shirt number (exact match).
  Default NULL for all players.

- long_short_only:

  Logical. If TRUE, keep only type in c("LONG","SHORT") before fitting.

- add_team_points:

  Logical. If TRUE and player is not NULL, add team fitted points for
  comparison.

## Value

A ggplot object.

## Details

Input validation is performed using validate_teamtv_shots().
