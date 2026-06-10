# Doctor Database

Doctor Database

## Usage

``` r
ts_doctor_db(
  check_limits = TRUE,
  check_period = TRUE,
  check_gaps = TRUE,
  check_triads = TRUE,
  fix = FALSE,
  conn = getOption("tsdbr.conn", NULL)
)
```

## Arguments

- check_limits:

  A flag indicating whether to check if corrected values outside the
  lower and upper limits are coded as erroneous.

- check_period:

  A flag indicating whether to check if the periods are valid.

- check_gaps:

  A flag indicating whether to check if there are any gaps in the data
  given the period.

- check_triads:

  A flag indicating whether to check if triads are consistent.

- fix:

  A flag indicating whether to fix any problems

- conn:

  An object of class SQLiteConnection.

## Value

A flag indicating whether or not the database passed the checks (or was
fixed)
