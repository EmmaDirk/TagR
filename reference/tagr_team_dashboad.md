# Team dashboard plots (coach overview)

Creates a small dashboard as a collection of ggplot objects the goal is
a quick descriptive overview of a teams shooting data

## Usage

``` r
tagr_team_dashboad(df, max_players = 12)
```

## Arguments

- df:

  A TeamTV shots data.frame.

- max_players:

  Integer maximum number of players to show based on total shots with
  non missing full_name.

## Value

If patchwork is available a patchwork object otherwise a named list of
ggplot objects.

## Details

Dashboard contains 6 plots 1 leg preference players placed on a left to
right line no y axis left means left leg preference right means right
leg preference point size scales with the number of shots used for the
estimate 2 goals by player bar chart counting total goal events per
player uses the same player colors as the leg plot no legend no names on
x axis 3 scoring percent by pressure goal percentage by pressure
category only types short long free ball 4 scoring percent by distance
goal percentage by distance band only types short long free ball
distance bins are less than 1 1 to 3 3 to 6 6 plus 5 scoring percent by
type goal percentage by shot type no exclusions 6 scoring percent by
shot count goal percentage by shot_count band 1 2 3 4 plus

Missing and onbekend handling

- character fields type pressure leg result are normalized to uppercase
  and separators become hyphen

- values equal to onbekend or empty string become na for those fields

- rows with missing result are excluded from goal percentage
  computations

- a goal is defined as result equals goal

- for leg preference only rows with type short long free ball and result
  goal miss and leg left right both are used players without sufficient
  leg data are not shown in the leg plot
