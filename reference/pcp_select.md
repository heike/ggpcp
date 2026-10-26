# Data wrangling for GPCPs: Step 1 variable selection

The `pcp_select` function allows a selection of variables from a data
set. These variables are transformed into an embellished long form of
the data.

## Usage

``` r
pcp_select(data, ...)
```

## Arguments

- data:

  a dataframe or tibble

- ...:

  choose the columns to be used in the parallel coordinate plot.
  Variables can be selected by position, name or any of the `tidyselect`
  selector functions.

## Value

dataframe of a long form of the selected variables with extra columns:

|                  |                                                    |
|------------------|----------------------------------------------------|
| **variable**     | **functionality**                                  |
| `pcp_x`, `pcp_y` | values for the mappings to x and y axes            |
| `pcp_yend`       | vertical endpoint of a line segment                |
| `pcp_class`      | type of each of the input variables                |
| `pcp_level`      | preserves order of levels in categorical variables |
| `pcp_id`         | identifier for each observation                    |

The dimensions of the returned data set are: 6 + the number of input
variables for its columns. The number of rows is given as the multiple
of the number of selected variables and the number of rows in the
original data.

## Details

The data pipeline feeding any of the geom layers in the `ggpcp` package
is implemented in a three-step modularized form rather than as the stat
functions more typical for `ggplot2` extensions. The three steps of data
pre-processing are:

|               |                                              |
|---------------|----------------------------------------------|
| **command**   | **data processing step**                     |
| `pcp_select`  | variable selection (and horizontal ordering) |
| `pcp_scale`   | (vertical) scaling of values                 |
| `pcp_arrange` | dealing with tie-breaks on categorical axes  |

Note that these data processing steps are executed before the call to
`ggplot2` and the identity function is used by default in all of the
`ggpcp` specific layers. Besides the speed-up by only executing the
processing steps once for all layers, the separation has the additional
benefit, that it provides the users with the possibility to make
specific choices at each step in the process. Additionally, separation
allows for a cleaner user interface: parameters affecting the data
preparation process can be moved to the relevant (set of) function(s)
only, thereby reducing the number of arguments without any loss of
functionality.

## See also

[`pcp_scale()`](https://heike.github.io/ggpcp/reference/pcp_scale.md),
[`pcp_arrange()`](https://heike.github.io/ggpcp/reference/pcp_arrange.md)

## Examples

``` r
data(Carcinoma)
dim(Carcinoma)
#> [1] 118   9
# select all variables
pcp_data <- Carcinoma |> pcp_select(1:9)
dim(pcp_data) # 6 more columns, 9 times as many observations
#> [1] 1062   16
head(pcp_data)
#> # A tibble: 6 × 16
#>   pcp_id pcp_x pcp_level pcp_y pcp_yend pcp_class    No Average A     B    
#>    <int> <fct> <chr>     <dbl>    <dbl> <chr>     <dbl>   <dbl> <fct> <fct>
#> 1      1 No    1             1        1 numeric       1    3.14 4     3    
#> 2      2 No    2             2        2 numeric       2    1    1     1    
#> 3      3 No    3             3        3 numeric       3    3    3     3    
#> 4      4 No    4             4        4 numeric       4    3.29 4     3    
#> 5      5 No    5             5        5 numeric       5    3    3     3    
#> 6      6 No    6             6        6 numeric       6    1.29 2     1    
#> # ℹ 6 more variables: C <fct>, D <fct>, E <fct>, F <fct>, G <fct>,
#> #   pcp_value <chr>
```
