# Plot shot locations coloured by shot type

Plots shot x/y coordinates on a fixed half korfball pitch background.
Points are coloured by shot type.

## Usage

``` r
tagr_plot_shots_type(
  df,
  player = NULL,
  result = NULL,
  pressure = NULL,
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

- pressure:

  Optional. Filter on pressure.

- leg:

  Optional. Filter on leg.

## Value

A ggplot object.
