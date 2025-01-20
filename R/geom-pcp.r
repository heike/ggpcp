#' @importFrom tidyr nest unnest
#' @importFrom assertthat has_name assert_that
#' @importFrom dplyr desc all_of
small_on_top <- function(data) {
#  colour <- ncolour <- NULL # placate R CMD check
#  x <- xend <- NULL

  if (is.null(data$colour)) return(data)

  assert_that(has_name(data, "x"))
  assert_that(has_name(data, "xend"))
  assert_that(has_name(data, "class"))

  data_list <- data %>% group_by(.data$x, .data$xend) %>% nest()
  data_list <- data_list %>% mutate(
    data = data %>% purrr::map(.f = function(d) {
      if (all(d$class == "factor")) {
        d <- d %>% group_by(.data$colour, .data$level)
      } else {
        d %>% group_by(.data$colour)
      }
      d %>%
        mutate(ncolour = n()) %>%
        arrange(desc(.data$ncolour)) %>%
        ungroup() %>%
        select(-.data$ncolour)
    })
  )
#  browser()

  data <- data_list %>% unnest(.data$data)

}

#' Wrapper for aes defaults
#'
#' The function provides a mapping from `ggpcp` internal variable names to the
#' variables' functional purpose in the grammar of graphics framework. Any of
#' the defaults can be overwritten by the user or flexibly expanded by other
#' aesthetic mappings in the usual manner.
#' @param x x axis
#' @param y y axis
#' @param yend end point of line segment
#' @param class specifying type of the variable
#' @param group identifier
#' @param level character string of factor levels
#' @param label label for factors
#' @param ... other aesthetics are directly passed on to `ggplot2`'s mapping
#' @import ggplot2
#' @export
#' @return a list of default mappings for all required aesthetics
#' @seealso [ggplot2::aes()]
#' @examples
#' library(ggplot2)
#' iris |>
#'   pcp_select(tidyselect::everything()) |>
#'   pcp_scale() |>
#'   pcp_arrange() |>
#'   ggplot(aes_pcp(colour = Species)) +
#'     geom_pcp() +
#'     theme_pcp()
aes_pcp <- function(x = pcp_x, y = pcp_y, yend = pcp_yend, class = pcp_class, group = pcp_id, level = pcp_level, label = pcp_level, ...) {
  pcp_x <- pcp_y <- pcp_class <- pcp_id <- pcp_level <- pcp_yend <- NULL

  x <- enquo(x)
  y <- enquo(y)
  yend <- enquo(yend)
  class <- enquo(class)
  group <- enquo(group)
  level <- enquo(level)
  label <- enquo(label)

  #browser()
  ggplot2::aes(x = !!x, y = !!y, yend = !!yend, class = !!class, group = !!group,
               level = !!level, label = !!label, ...)
}

#' Generalized Parallel Coordinate plots
#'
#' @description
#' The `ggpcp` package for generalized parallel coordinate plots is implemented as a
#' `ggplot2` extension.
#' In particular, this implementation makes use of `ggplot2`'s layer framework,
#' allowing for a lot of flexibility in the choice and order of showing graphical elements.
#'
#' | **command**      | **graphical element**                    |
#' |--------------|--------------------------------------|
#' | `geom_pcp`     | line segments                        |
#' | `geom_pcp_axes` | vertical lines to represent all axes |
#' | `geom_pcp_box` | boxes for levels on categorical axes |
#' | `geom_pcp_labels` | labels for levels on categorical axes |
#'
#' These `ggpcp` specific layers can be mixed with `ggplot2`'s regular geoms,
#' such as e.g. [ggplot2::geom_point()], [ggplot2::geom_boxplot()], [ggdensity::geom_hdr()], etc.
#'
#' # About Parallel Coordinate Plots
#' Parallel coordinate plots are a multivariate visualization that allows several aspects of an
#' observed entity to be shown in a single plot. Each aspect is represented by a vertical
#' axis (giving the plot its name), values are marked on each of these axes. Values corresponding to the same entity are connected
#' by line segments between adjacent axes. This type of visualization was first
#' used by d’Ocagne (1985). Modern re-inventions go back to Inselberg (1985) and
#' Wegman (1990).
#' This implementation takes a more general approach in that it is also able to deal
#' with  categorical in the same principled way that allows a tracking of individual
#' observations across multiple dimensions.
#'
#' # Data wrangling
#' The data pipeline feeding `geom_pcp` is implemented in a three-step modularized
#' form rather than in a `stat_pcp` function more typical for `ggplot2` extensions.
#' The three steps of data pre-processing are:
#'
#' | **command**      | **data processing step**                    |
#' |--------------|--------------------------------------|
#' | `pcp_select`     | variable selection (and horizontal ordering)                       |
#' | `pcp_scale` | (vertical) scaling of values |
#' | `pcp_arrange` | dealing with tie-breaks on categorical axes |
#'
#' Note that these data processing steps are executed before the call to `ggplot2`
#' and the identity function is used by default in all of the `ggpcp` specific layers.
#' Besides the speed-up by only executing the processing steps once for all layers,
#' the separation has the additional benefit, that it provides the users with the
#' possibility to make specific choices at each step in the process. Additionally,
#' separation allows for a cleaner user interface: parameters affecting the data
#' preparation process can be moved to the relevant (set of) function(s) only, thereby
#' reducing the  number of arguments without any loss of functionality.
#'
#' @inheritParams ggplot2::layer
#' @param axiswidth vector of two values indicating the space  numeric and categorical axes are supposed to take. Minimum of 0, maximum of 1.Defaults to 0 for a numeric axis and 0.1 for a categorical axis.
#' @param overplot character value indicating which method should be used to mitigate overplotting of lines. Defaults to 'small-on-top'. The overplotting strategy 'small-on-top' identifies the number observations for each combination of levels between two categorical variables and plots the lines from highest frequency to smallest (effectively plotting small groups on top). The strategy 'none' gives most flexibility to the user - the plotting order is preserved by the order in which observations are included in the original data.
#' @param na.rm If `FALSE` (the default), removes missing values with a warning. If `TRUE` silently removes missing values.
#' @param ... other arguments passed on to `layer`. These are often aesthetics, used to set an aesthetic to a fixed value, like `color = 'red'` or `size = 3`. They may also be parameters to the paired geom/stat.
#' @references
#' M. d’Ocagne. (1885) *Coordonnées parallèles et axiales: Méthode de transformation géométrique et procédé nouveau de calcul graphique déduits de la considération des coordonnées parallèles.* Gauthier-Villars, page 112, \url{https://archive.org/details/coordonnesparal00ocaggoog/page/n10}.
#'
#' Al Inselberg. (1985) *The plane with parallel coordinates.* The Visual Computer, 1(2):69–91,  \doi{10.1007/BF01898350}.
#'
#' Ed J. Wegman. (1990) *Hyperdimensional data analysis using parallel coordinates.* Journal of the American Statistical Association, 85:664–675, \doi{10.2307/2290001}.
#' @export
#' @return a list consisting of a [ggplot2::layer()] object and its associated scales.
#' @examples
#' library(ggplot2)
#' data(mtcars)
#' mtcars_pcp <- mtcars |>
#'   dplyr::mutate(
#'     cyl = factor(cyl),
#'     vs = factor(vs),
#'     am = factor(am),
#'     gear = factor(gear),
#'     carb = factor(carb)
#'   ) |>
#'   pcp_select(1:11) |>  # select everything
#'   pcp_scale() |>
#'   pcp_arrange()
#'
#'  base <- mtcars_pcp |> ggplot(aes_pcp())
#'
#'
#'  # Just the base plot:
#'  base + geom_pcp()
#'
#'  # with the pcp theme
#'  base + geom_pcp() + theme_pcp()
#'
#'  # with boxplots:
#'  base +
#'   geom_pcp(aes(colour = cyl)) +
#'   geom_boxplot(aes(x = pcp_x, y = pcp_y),
#'    inherit.aes=FALSE,
#'    data = dplyr::filter(mtcars_pcp, pcp_class!="factor")) +
#'   theme_pcp()
#'
#' # base plot with boxes and labels
#'  base +
#'   geom_pcp(aes(colour = cyl)) +
#'   geom_pcp_boxes() +
#'   geom_pcp_labels() +
#'   theme_pcp()
geom_pcp <- function(mapping = NULL, data = NULL, stat = "identity",
                        position = "identity", na.rm = FALSE,
                        axiswidth = c(0, 0.1), overplot = "small-on-top",
                        show.legend = NA, inherit.aes = TRUE, ...)
{
  list(
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPcp,
    position = position,
    show.legend = show.legend,
    check.aes = FALSE,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      axiswidth = axiswidth,
      overplot = overplot,
      ...
    )
  ),
  scale_x_discrete(expand = expansion(add=0.5))
  )
}


#' Proto version of the pcp geoms
#'
#' These functions are only exported so that they are visible to
#' the ggplot2 internal functions.
#' User-relevant documentation can be found instead in [geom_pcp()].
#'
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom grid grobTree
GeomPcp <- ggplot2::ggproto(
  "GeomPcp",
  ggplot2::Geom,
  required_aes = c("x", "y", "group", "class"),
#  non_missing_aes = character(),
#  optional_aes = character(),
  default_aes = aes(
    colour = "grey30", size = 0.5, linetype = "solid", alpha = NA,
    linewidth=.5, stroke = 2
  ),

  extra_params = c("na.rm", "axiswidth", "overplot"),

  axiswidth = c(0, 0.1),
  overplot = "small-on-top",

  draw_key = ggplot2::draw_key_path,

  handle_na = function(self, data, params) {
    #browser()
    remove_missing(data, params$na.rm,
                   c(self$required_aes, self$non_missing_aes),
                   snake_class(self)
    )
  },

# #  draw_panel = function(self, data, params, coord, ...) {
#   draw_layer = function(self, data, params, layout, coord) {
#       #browser()
#
#
#   },

draw_layer = function(self, data, params, layout, coord) {
#  browser()
  if (ggplot2:::empty(data)) {
    n <- if (is.factor(data$PANEL)) nlevels(data$PANEL) else 1L
    return(rep(list(ggplot2::zeroGrob()), n))
  }

  # Trim off extra parameters
#  params <- params[intersect(names(params), self$parameters())]

  args <- c(list(quote(data), quote(panel_params), quote(coord)), params)
  lapply(split(data, data$PANEL), function(data) {
    if (ggplot2:::empty(data)) return(ggplot2:::zeroGrob())

    panel_params <- layout$panel_params[[data$PANEL[1]]]
    do.call(self$draw_panel, args)
  })
},

  draw_panel = function(self, data, panel_params, coord, axiswidth, overplot, ...) {
#    browser()
#cat("draw_panel \n")

data <- data %>% group_by(PANEL) %>% mutate(
  xend = x + ifelse(class == "factor", axiswidth[2], axiswidth[1]),
  x = x - ifelse(class == "factor", axiswidth[2], axiswidth[1])
  #      yend = y
)

data2 <- data %>% group_by(PANEL, group) %>% arrange(x) %>%
  mutate(
    x_new = xend,
    xend_new = c(x[-1], NA),
    y_new = yend,
    yend_new = c(y[-1],NA),
    last = c(rep(FALSE, n()-1), TRUE)
  )
data2 <- data2 %>% filter(!last) %>% select(-last) %>%
  mutate(
    x = x_new,
    y = y_new,
    xend = xend_new,
    yend = yend_new
  ) %>% select(-ends_with("_new"))
data <- rbind(data, data2)
# this is where we could use more sophisticated orderings
if (overplot != "none") {
  if (overplot == "small-on-top") {
    data <- small_on_top(data)
  } else {
  if (!is.null(data$colour))
    data <- data %>% arrange(colour)  # order by colour
  }
}

GeomSegment$draw_panel(data, panel_params,  coord=coord, lineend = "round",
                       linejoin = "round")



#    GeomSegment$draw_panel(data, panel_params, layout, coord)
  },

  setup_params = function(data, params) {
 #   browser()
    axiswidth = params$axiswidth
    overplot = params$overplot
    params
    }

)
