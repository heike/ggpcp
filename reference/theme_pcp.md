# Theme for parallel coordinate plots

The function `theme_pcp` provides a wrapper for thematic choices
suitable for parallel coordinate plots. In particular, the labeling of
axes in parallel coordinate plot is quite un-informative. In the default
theme axes labels are based on variable names derived during the data
wrangling step.

## Usage

``` r
theme_pcp(base_size = 11, base_family = "")
```

## Arguments

- base_size:

  base font size, given in pts.

- base_family:

  base font family

## Value

A `ggplot2` theme object based on
[`ggplot2::theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
without y axis and x axes labels.

## See also

[`ggplot2::theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)

## Examples

``` r
library(ggplot2)
gg <- iris |>
  pcp_select(tidyselect::everything()) |>
  pcp_scale() |>
  pcp_arrange() |>
  ggplot(aes_pcp(colour = Species)) +
    geom_pcp()

# plot with the default ggplot2 theme
gg

# better:
gg + theme_pcp()
```
