# Player dashboard plots (coach overview)

Creates a player dashboard as a collection of ggplot objects the goal is
a quick descriptive overview of one players shooting data

## Usage

``` r
tagr_player_dashboard(df, player)
```

## Arguments

- df:

  A TeamTV shots data.frame.

- player:

  Player name fuzzy match or shirt number exact match.

## Value

If patchwork is available a patchwork object otherwise a named list of
ggplot objects.

## Details

Dashboard contains 5 plots all bar charts 1 scoring percent by pressure
only types short long free ball 2 scoring percent by distance only types
short long free ball bins less than 1 1 to 3 3 to 6 6 plus 3 scoring
percent by type no exclusions 4 scoring percent by shot count bands 1 2
3 4 plus no exclusions 5 scoring percent by leg types short long free
ball leg in left right both

Missing and onbekend handling

- character fields type pressure leg result are normalized to uppercase
  and separators become hyphen

- values equal to onbekend or empty string become na for these fields

- rows with missing result are excluded from all goal percentage
  computations

- a goal is defined as result equals goal

Player selection

- if player is all digits it matches shirt number exactly

- otherwise it fuzzy matches on full_name using utils adist and requires
  a close match
