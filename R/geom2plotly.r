#' @name plotly_helpers
#' @title Plotly helpers
#' @export to_basic.GeomPcp
#' @importFrom plotly to_basic
#' @description Helper functions to make it easier to automatically create plotly charts
#' @export
#' @examples
#' # example code
#' flea_plotly <- highlight_key(flea_pcp, ~pcp_id)
#' p <- flea_plotly  |>
#'   ggplot(aes_pcp()) +
#'   geom_pcp_axes() +
#'   geom_pcp(aes(colour = species, label = pcp_id))
#
#' pp <- ggplotly(p)
#' highlight(pp, on="plotly_hover")
#'
to_basic.GeomPcp <- function(data, prestats_data, layout, params, p, ...) {
   data$hovertext <- paste0(as.character(data$x_plotlyDomain), ": ", data$value)

   data <- data[order(data[["x"]]), ]
   prefix_class(data, "GeomPath")
}



#' @rdname plotly_helpers
#' @export
to_basic.GeomPcp_axes <- function(data, prestats_data, layout, params, p, ...) {
  prefix_class(data, "GeomBlank") # no text for the axes
}

#' @rdname plotly_helpers
#' @export
to_basic.GeomPcp_labels <- function(data, prestats_data, layout, params, p, ...) {
  prefix_class(data, "GeomBlank") # no text for the axes
}

#' @rdname plotly_helpers
#' @export
to_basic.GeomPcp_boxes <- function(data, prestats_data, layout, params, p, ...) {
  prefix_class(data, "GeomBlank") # no text for the axes
}


#' @rdname plotly_helpers
#' @keywords internal
prefix_class <- function(x, y) {
  structure(x, class = unique(c(y, class(x))))
}
