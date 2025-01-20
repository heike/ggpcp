#' @inherit geom_pcp
#' @export
geom_pcp_axes <- function(mapping = NULL, data = NULL, stat = "identity",
                        position = "identity", na.rm = FALSE,
                        show.legend = NA, inherit.aes = TRUE, ...)
{

  # browser()


  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPcp_axes,
    position = position,
    show.legend = show.legend,
    check.aes = FALSE,
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
GeomPcp_axes <- ggplot2::ggproto(
  "GeomPcp_axes",
  ggplot2::Geom,
  required_aes = c("x", "y"),

  default_aes = aes("x", "y",
    colour = "grey60", size = 0.25, linetype = "solid", alpha = NA,
    linewidth=.1, stroke = 2
  ),

  draw_layer = function(self, data, params, layout, coord) {
    data$xintercept = data$x

    GeomVline$draw_layer(data, params, layout, coord)
  },

  draw_panel = function(self, data, panel_params, coord, ...) {
    #browser()

    data$xintercept = data$x

    GeomVline$draw_layer(data, panel_params, layout, coord)
  },

  draw_group = function(self, data, panel_params, coord) {
    #browser()

    data$xintercept = data$x

    GeomVline$draw_layer(data, panel_params, layout, coord)
  }

)
