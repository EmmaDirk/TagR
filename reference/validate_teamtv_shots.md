# Validate a TeamTV tagged-shots data.frame

Checks:

- column names: exact match no missing or extra columns

- column order: must match the expected schema order

- column types: match expected types with integerish tolerance for
  integer columns

- allowed values for pressure type leg result case insensitive na
  allowed

## Usage

``` r
validate_teamtv_shots(x)
```

## Arguments

- x:

  A data.frame with TeamTV tagged shots

## Value

Invisibly returns TRUE if valid otherwise errors

## Details

Missing data handling:

- na values are allowed for pressure type leg result and are skipped in
  allowed value checks

- for integerish checks na values are ignored and only non missing
  values are validated

- if an entire column is the wrong type validation errors even if many
  values are na
