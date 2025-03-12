#' @name plotly_helpers
#' @title Plotly helpers
#' @export to_basic.GeomPcp
#' @importFrom plotly to_basic
#' @description Helper functions to make it easier to automatically create plotly charts
#' @export
 to_basic.GeomPcp <- function(data, prestats_data, layout, params, p, ...) {
   data$hovertext <- paste0(as.character(data$x_plotlyDomain), ": ", data$value)

 #  browser()

#   data <- data %>% group_by(group) %>% mutate(
#     hovertext = paste0(as.character(x_plotlyDomain), ": ", label, collapse = "\n")
#   ) %>% ungroup()


   data <- data[order(data[["x"]]), ]
   prefix_class(data, "GeomPath")
#
#
#
#   getFromNamespace("to_basic.GeomLine", asNamespace("plotly"))
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
