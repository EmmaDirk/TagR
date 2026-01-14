# Plot missingness patterns for TeamTV shot data

Pattern based missingness heatmap like md.pattern or vim style complete
patterns are placed at the top then patterns are sorted by completeness
percent observed is printed above each variable column

## Usage

``` r
tagr_plot_missingness(df, person = NULL, max_patterns = 30)
```

## Arguments

- df:

  A data.frame containing TeamTV export columns.

- person:

  Optional string. If provided, filters to the closest matching
  full_name using fuzzy match. Use NULL for all players.

- max_patterns:

  Maximum number of patterns to display most frequent kept.

## Value

A ggplot object.

## Details

Rules:

- strings like onbekend are treated as missing and converted to na

- leg is only relevant for type in c(short long freeball) for other shot
  types leg is treated as not applicable and not counted as missing

Missing data handling:

- unknown tokens like onbekend unknown unk n a na and empty string are
  converted to na for character columns

- rows are not removed for missingness plotting patterns are computed
  from all rows after cleaning

- leg missingness is rule based and only counted when leg is required
  for the shot type
