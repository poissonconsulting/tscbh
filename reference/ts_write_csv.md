# Write csv

The initial columns in the csv file are as follows

- Year:

  The year of the value.

- Month:

  The month of the value.

- Day:

  The day of the value.

- Hour:

  The hour of the value.

- Station:

  The station

The csv file should also include the following columns

- Recorded:

  The value as provided by the station maintainer.

- Corrected:

  The corrected value (see below).

- Status:

  The reliability of the corrected value (see below).

- Parameter:

  The parameter type, ie, Discharge, Elevation, Water Temperature etc.

- Units:

  The units of the value.

- StationName:

  The name of the station.

- Comments:

  Comments on the value.

## Usage

``` r
ts_write_csv(data, file = "tsdbr.csv")
```

## Arguments

- data:

  The data frame with a minimum of columns Station and DateTime

- file:

  either a character string naming a file or a
  [connection](https://rdrr.io/r/base/connections.html) open for
  writing. `""` indicates output to the console.

## Value

An invisible copy of saved data.

## Details

There are three possible status values

- Reasonable:

  There is nothing obviously wrong with the value.

- Questionable:

  The value may be wrong.

- Erroneous:

  The value is definitely wrong.

In general questionable values are those which are in the range of
possible values but are inconsistent with spatial or temporal neighbours
while erroneous value are those which are outside the range of possible
values, ie negative discharge.

If a value is erroneous, questionable or missing (indicated by NA) then
we will (time permitting) provide a corrected value based on
neighbouring values. Otherwise the corrected value is simply the
recorded value. The status column refers to the corrected value and
corrected values which differ from the recorded value are coded as
Questionable.

If data gaps are not important it is recommended that you only include
Reasonable values. Otherwise it is recommended that you only include
Questionable values that you consider to be reliable based on plotting
and analysis.
