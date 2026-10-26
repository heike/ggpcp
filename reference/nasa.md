# Data set: NASA - Data Expo 2006

The data are geographic and atmospheric measures on a very coarse 24 by
24 grid covering Central America. This data was provided by the NASA
Langley Research Center Atmospheric Sciences Data Center as part of the
ASA Data Expo in 2006. Monthly averages of a set of atmospheric
measurements are provided for Jan 1995 to Dec 2000. A subset of this
data is available from the `GGally` package.

## Usage

``` r
nasa
```

## Format

A data frame with 41472 (= 24 x 24 x 72) rows and 15 variables:

## Source

<https://community.amstat.org/jointscsg-section/dataexpo/dataexpo2006>

## Structural variables

- time:

  time index for each month from 1 (= Jan 1995) to 72 (= Dec 2000)

- id:

  identifier for each grid point 1-1 to 24-24

- lat, long:

  geographic latitude and longitude

- elevation:

  altitude of the location in meters above sea level

- month, year, date:

  year/month of each measurement

## Measured variables

- cloudlow, cloudmid, cloudhigh:

  Cloud cover (in percent) at low, middle, and high levels.

- ozone:

  mean ozone abundance (in dobson)

- pressure:

  mean surface pressure (in millibars)

- surftemp, temperature:

  mean surface/near surface air temperature (in Kelvin)

## Examples

``` r
data(nasa)
library(ggplot2)
nasa |>
  dplyr::filter(id == "1-10") |>
  pcp_select(starts_with("cloud"), ozone, temperature) |>
  pcp_scale() |>
  ggplot(aes_pcp()) +
 geom_pcp(aes(colour=month))
```
