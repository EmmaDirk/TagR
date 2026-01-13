# Plot shot locations coloured by result

Plots shot x/y coordinates on a fixed half korfball pitch background.
Points are coloured by result (goal or miss).

## Usage

``` r
tagr_plot_shots_result(
  df,
  player = NULL,
  type = NULL,
  pressure = NULL,
  leg = NULL
)
```

## Arguments

- df:

  A TeamTV shots data.frame.

- player:

  Optional. Player name (fuzzy match) or shirt number (exact match).

- type:

  Optional. Filter on shot type.

- pressure:

  Optional. Filter on pressure.

- leg:

  Optional. Filter on leg.

## Value

A ggplot object.
