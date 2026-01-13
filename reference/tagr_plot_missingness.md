# Plot missingness patterns for TeamTV shot data

Pattern-based missingness heatmap (like md.pattern/VIM-style): complete
patterns on top, then patterns sorted by completeness. Adds percent
observed above columns.

## Usage

``` r
tagr_plot_missingness(df, person = NULL, max_patterns = 30)
```

## Arguments

- df:

  A data.frame containing TeamTV export columns.

- person:

  Optional string. If provided, filters to the closest matching
  `full_name` (fuzzy match). Use `NULL` for all players.

- max_patterns:

  Maximum number of patterns to display (most frequent kept).

## Value

A ggplot object.

## Details

Rules:

- Strings like "onbekend" are treated as missing (converted to NA).

- `leg` is only relevant for type in c("SHORT","LONG","FREEBALL"). For
  other shot types, leg is treated as N/A (not missing).
