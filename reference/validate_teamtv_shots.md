# Validate a TeamTV tagged-shots data.frame

Checks:

- column names: exact match (no missing/extra)

- column types: match expected types (with "integer-ish" tolerance)

- allowed values for: pressure, type, leg, result (case-insensitive; NA
  allowed)

## Usage

``` r
validate_teamtv_shots(x)
```

## Arguments

- x:

  A data.frame with TeamTV tagged shots

## Value

Invisibly returns TRUE if valid; otherwise errors.
