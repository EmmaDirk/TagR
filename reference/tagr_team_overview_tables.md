# Team overview descriptives tables for TeamTV shot data

Computes goal percentages by pressure, distance band, shot type, shot
count band, and leg. Returns a named list of plain data.frames so they
can be used directly (and tested).

## Usage

``` r
tagr_team_overview_tables(
  df,
  player = NULL,
  split_by_player = FALSE,
  max_players = 12,
  filter_type = NULL,
  filter_pressure = NULL,
  filter_leg = NULL,
  filter_result = NULL,
  filter_distance_band = NULL,
  filter_shot_count_band = NULL,
  exclude_types = NULL
)
```

## Arguments

- df:

  A TeamTV shots data.frame.

- player:

  Optional. Player name (fuzzy match) or shirt number (exact match).
  Default NULL for all players.

- split_by_player:

  Logical. If TRUE, returns a named list per player (each containing the
  five tables).

- max_players:

  Integer. When split_by_player is TRUE, limit to the top N players by
  number of shots.

- filter_type:

  Optional character vector. Keep only these shot types.

- filter_pressure:

  Optional character vector. Keep only these pressure values.

- filter_leg:

  Optional character vector. Keep only these leg values.

- filter_result:

  Optional character vector. Keep only these results.

- filter_distance_band:

  Optional character vector. Keep only these distance bands. Allowed:
  "\<1 m", "1-3 m", "3-6 m", "6+ m".

- filter_shot_count_band:

  Optional character vector. Keep only these shot count bands. Allowed:
  "1", "2", "3", "4+".

- exclude_types:

  Optional character vector. Shot types to exclude.

## Value

If split_by_player is FALSE: a named list of data.frames: pressure,
distance, type, shot_count, leg. If split_by_player is TRUE: a named
list where each element is that same named list.

## Details

Filtering can be applied before computing the descriptives. You can
filter for a single player (fuzzy match on full_name or exact match on
number) or split results by player.
