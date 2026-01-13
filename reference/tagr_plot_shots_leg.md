# Plot shot locations coloured by leg

Plots shot x/y coordinates on a fixed half korfball pitch background.
Points are coloured by leg.

## Usage

``` r
tagr_plot_shots_leg(
  df,
  player = NULL,
  result = NULL,
  type = NULL,
  pressure = NULL
)
```

## Arguments

- df:

  A TeamTV shots data.frame.

- player:

  Optional. Player name (fuzzy match) or shirt number (exact match).

- result:

  Optional. Filter on result.

- type:

  Optional. Filter on shot type.

- pressure:

  Optional. Filter on pressure.

## Value

A ggplot object.
