# Shooting heatmap

Plots shot x/y coordinates on a fixed half korfball pitch background.
Points are coloured by one of: result, type, pressure, leg.

## Usage

``` r
tagr_heatmap(
  df,
  colour_by = c("result", "type", "pressure", "leg"),
  filter = list(),
  player = NULL
)
```

## Arguments

- df:

  A TeamTV shots data.frame.

- colour_by:

  What to colour points by. One of "result", "type", "pressure", "leg".

- filter:

  Optional named list of filters. Example: list(result = "goal",
  pressure = "HIGH"). Note: if you supply a filter for the same variable
  as `colour_by`, it is ignored.

- player:

  Optional. One or more player identifiers (numbers and/or names).
  Examples: "11", c("11","24"), "Jane Doe", c("Jane Doe","24").

## Value

A ggplot object.
