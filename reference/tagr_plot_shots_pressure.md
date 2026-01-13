# Plot shot locations coloured by pressure

Plots shot x/y coordinates on a fixed half korfball pitch background.
Points are coloured by pressure.

## Usage

``` r
tagr_plot_shots_pressure(
  df,
  player = NULL,
  result = NULL,
  type = NULL,
  leg = NULL
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

- leg:

  Optional. Filter on leg.

## Value

A ggplot object.
