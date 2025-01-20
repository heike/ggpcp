#' Theme for parallel coordinate plots
#'
#' The function `theme_pcp` provides a wrapper for thematic
#' choices suitable for parallel coordinate plots. In particular,
#' the labeling of axes in parallel coordinate plot is quite un-informative.
#' In the default theme axes labels are based on variable names derived during the
#' data wrangling step.
#' @inheritParams ggplot2::theme_bw
#' @export
#' @importFrom ggplot2 %+replace%
#' @return A `ggplot2` theme object based on [ggplot2::theme_bw()] without y axis and x axes labels.
#' @seealso [ggplot2::theme_bw()]
#' @examples
#' library(ggplot2)
#' gg <- iris |>
#'   pcp_select(tidyselect::everything()) |>
#'   pcp_scale() |>
#'   pcp_arrange() |>
#'   ggplot(aes_pcp(colour = Species)) +
#'     geom_pcp()
#'
#' # plot with the default ggplot2 theme
#' gg
#' # better:
#' gg + theme_pcp()
theme_pcp <- function(base_size = 11, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title = element_blank()
#          panel.background = element_blank(),
#          panel.border = element_blank(),
#          panel.grid = element_blank(),
#          panel.spacing = unit(0, "lines"),
#          plot.background = element_blank(),
#          legend.justification = c(0, 0),
#          legend.position = c(0, 0))
    )
}
