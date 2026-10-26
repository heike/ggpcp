# Wrapper for aes defaults

The function provides a mapping from `ggpcp` internal variable names to
the variables' functional purpose in the grammar of graphics framework.
Any of the defaults can be overwritten by the user or flexibly expanded
by other aesthetic mappings in the usual manner.

## Usage

``` r
aes_pcp(
  x = pcp_x,
  y = pcp_y,
  yend = pcp_yend,
  class = pcp_class,
  group = pcp_id,
  level = pcp_level,
  label = pcp_level,
  value = pcp_value,
  ...
)
```

## Arguments

- x:

  x axis

- y:

  y axis

- yend:

  end point of line segment

- class:

  specifying type of the variable

- group:

  identifier

- level:

  character string of factor levels

- label:

  label for factors

- value:

  original value of any of the variables

- ...:

  other aesthetics are directly passed on to `ggplot2`'s mapping

## Value

a list of default mappings for all required aesthetics

## See also

[`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html)

## Examples

``` r
library(ggplot2)
iris |>
  pcp_select(tidyselect::everything()) |>
  pcp_scale() |>
  pcp_arrange() |>
  ggplot(aes_pcp(colour = Species)) +
    geom_pcp() +
    theme_pcp()
```
