# Read zrxp Data File

A utc_offset of -8 is equivalent to Pacific Standard Time.

## Usage

``` r
ts_read_zrxp(file = "tscbh.zrxp", utc_offset = -8L)
```

## Arguments

- file:

  A string specifying the path to the file.

- utc_offset:

  A integer of the utc offset which must lie between -12 and 14.
