# Data wrangling for GPCPs: Step 2 scale values

The function `pcp_scale` provides access to a set of transformations to
use in parallel coordinate plots. All transformations other than `raw`
tend to produce y values in the interval from 0 and 1.

## Usage

``` r
pcp_scale(data, method = "uniminmax", .by_group = TRUE)
```

## Arguments

- data:

  data frame as returned by `select_pcp`

- method:

  string specifying the method that should be used for scaling the
  values in a parallel coordinate plot (see Details).

- .by_group:

  logical value. If TRUE, scaling will respect any previous grouping
  variables. Applies to grouped data frames only.

## Value

data frame of the same size as the input data; values of `pcp_y` and
`pcp_yend` are scaled according to the specified method.

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

`method` is a character string that denotes how to scale the variables
in the parallel coordinate plot. Options are named in the same way as
the options in
[`GGally::ggparcoord()`](https://ggobi.github.io/ggally/reference/ggparcoord.html):

- `raw`:

  raw data used, no scaling is done.

- `std`:

  univariately, subtract mean and divide by standard deviation. To get
  values into a unit interval we use a linear transformation of f(y) =
  y/4+0.5.

- `robust`:

  univariately, subtract median and divide by median absolute deviation.
  To get values into an expected interval of unit interval we use a
  linear transformation of f(y) = y/4+0.5.

- `uniminmax`:

  univariately, scale so the minimum of the variable is zero, and the
  maximum is one.

- `globalminmax`:

  global scaling; the global maximum is mapped to 1, global minimum
  across the variables is mapped to 0.

## See also

[`pcp_select()`](https://heike.github.io/ggpcp/reference/pcp_select.md),
[`pcp_arrange()`](https://heike.github.io/ggpcp/reference/pcp_arrange.md)

## Examples

``` r
data(Carcinoma)
dim(Carcinoma)
#> [1] 118   9
# select all variables
pcp_data <- Carcinoma |> pcp_select(1:9)
summary(pcp_data)
#>      pcp_id          pcp_x         pcp_level        pcp_y        
#>  Min.   :  1.0   No     :118   Length   :1062   Min.   :  1.000  
#>  1st Qu.: 30.0   Average:118   N.unique : 136   1st Qu.:  1.607  
#>  Median : 59.5   A      :118   N.blank  :   0   Median :  2.929  
#>  Mean   : 59.5   B      :118   Min.nchar:   1   Mean   :  9.107  
#>  3rd Qu.: 89.0   C      :118   Max.nchar:  16   3rd Qu.:  3.000  
#>  Max.   :118.0   D      :118                    Max.   :126.000  
#>                  (Other):354                                     
#>     pcp_yend           pcp_class          No            Average      A      
#>  Min.   :  1.000   Length   :1062   Min.   :  1.00   Min.   :1.000   1:234  
#>  1st Qu.:  1.607   N.unique :   2   1st Qu.: 33.00   1st Qu.:1.571   2:234  
#>  Median :  2.929   N.blank  :   0   Median : 63.50   Median :2.429   3:342  
#>  Mean   :  9.107   Min.nchar:   6   Mean   : 63.47   Mean   :2.311   4:198  
#>  3rd Qu.:  3.000   Max.nchar:   7   3rd Qu.: 94.00   3rd Qu.:3.000   5: 54  
#>  Max.   :126.000                    Max.   :126.00   Max.   :5.000          
#>                                                                             
#>  B       C       D       E       F       G           pcp_value   
#>  1:243   1:279   1:342   1:144   1:558   1:288   Length   :1062  
#>  2:108   2:378   2:432   2:279   2:279   2:180   N.unique : 136  
#>  3:621   3:333   3:207   3:477   3:180   3:549   N.blank  :   0  
#>  4: 63   4: 54   4: 72   4:126   4:  9   4: 27   Min.nchar:   1  
#>  5: 27   5: 18   5:  9   5: 36   5: 36   5: 18   Max.nchar:  16  
#>                                                                  
#>                                                                  
pcp_data |> pcp_scale() |> summary()
#>      pcp_id          pcp_x         pcp_level        pcp_y        
#>  Min.   :  1.0   No     :118   Length   :1062   Min.   :0.00000  
#>  1st Qu.: 30.0   Average:118   N.unique : 136   1st Qu.:0.07143  
#>  Median : 59.5   A      :118   N.blank  :   0   Median :0.32143  
#>  Mean   : 59.5   B      :118   Min.nchar:   1   Mean   :0.34690  
#>  3rd Qu.: 89.0   C      :118   Max.nchar:  16   3rd Qu.:0.50000  
#>  Max.   :118.0   D      :118                    Max.   :1.00000  
#>                  (Other):354                                     
#>     pcp_yend           pcp_class          No            Average      A      
#>  Min.   :0.00000   Length   :1062   Min.   :  1.00   Min.   :1.000   1:234  
#>  1st Qu.:0.07143   N.unique :   2   1st Qu.: 33.00   1st Qu.:1.571   2:234  
#>  Median :0.32143   N.blank  :   0   Median : 63.50   Median :2.429   3:342  
#>  Mean   :0.34690   Min.nchar:   6   Mean   : 63.47   Mean   :2.311   4:198  
#>  3rd Qu.:0.50000   Max.nchar:   7   3rd Qu.: 94.00   3rd Qu.:3.000   5: 54  
#>  Max.   :1.00000                    Max.   :126.00   Max.   :5.000          
#>                                                                             
#>  B       C       D       E       F       G           pcp_value   
#>  1:243   1:279   1:342   1:144   1:558   1:288   Length   :1062  
#>  2:108   2:378   2:432   2:279   2:279   2:180   N.unique : 136  
#>  3:621   3:333   3:207   3:477   3:180   3:549   N.blank  :   0  
#>  4: 63   4: 54   4: 72   4:126   4:  9   4: 27   Min.nchar:   1  
#>  5: 27   5: 18   5:  9   5: 36   5: 36   5: 18   Max.nchar:  16  
#>                                                                  
#>                                                                  
# scaling gets values of pcp_y and pcp_yend between 0 and 1
```
