#' @inherit geom_pcp
#' @export
geom_pcp_labels <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE,
                           ...)
{
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPcp_labels,
    position = position,
    show.legend = show.legend,
    check.aes = FALSE,
    check.param = FALSE,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname GeomPcp
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom grid grobTree
GeomPcp_labels <- ggplot2::ggproto(
  "GeomPcp_labels",
  ggplot2::Geom,
  required_aes = c("x", "y", "label",  "class"),
  #  non_missing_aes = character(),
  #  optional_aes = character(),
  extra_params = c("na.rm"),

  default_aes = aes(
    colour = "black", fill = "white", size = 3.88, angle = 0,
    hjust = 0.5, vjust = 0.5, alpha = 0.5, family = "", fontface = 1,
    lineheight = 1.2
  ),

  setup_params = function(data, params) {
    #  browser()

    params
  },

  setup_data = function(data, params) {
  #  browser()

    data <- data %>%
      filter(class == "factor") %>%
      group_by(PANEL, x, label) %>%
      summarize(
        y = (max(y, na.rm=TRUE)+ min(y, na.rm=TRUE))/2,
        class = class[1],
        group = group[1]
      )

    data
  },

  draw_key = ggplot2::draw_key_rect,

  draw_panel = function(self, data, panel_params, coord, ...) {

    GeomLabel$draw_panel(data = data, panel_params, coord, ...)
  },

  draw_group = function(self, data, panel_params, coord, ...) {
  }#,
)
