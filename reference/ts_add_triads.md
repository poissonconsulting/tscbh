# Add Triads

Add Triads

## Usage

``` r
ts_add_triads(triads, conn = getOption("tsdbr.conn", NULL))
```

## Arguments

- triads:

  A data frame of triads with columns Child, Parent1, Parent2.

- conn:

  An object of class SQLiteConnection.

## Value

The imported triad data.
