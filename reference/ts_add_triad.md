# Add Triad

Add Triad

## Usage

``` r
ts_add_triad(child, parent1, parent2, conn = getOption("tsdbr.conn", NULL))
```

## Arguments

- child:

  A string of the child station.

- parent1:

  A string of the first parent station.

- parent2:

  A string of the second parent station.

- conn:

  An object of class SQLiteConnection.

## Value

A data frame of the imported triad.
