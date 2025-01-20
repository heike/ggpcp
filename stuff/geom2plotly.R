#' @importFrom utils getFromNamespace
#' @importFrom plotly to_basic
#' @export
to_basic.GeomPcp <- function (data, prestats_data, layout, params, p, ...) {
 browser()

  data$hovertext <- paste0(as.character(data$x_plotlyDomain), ": ", data$label)
#  data <- data[order(data[["group"]]), ]
  getFromNamespace("prefix_class", asNamespace("plotly"))(data, "GeomPcp")

}

# note to future selves: restart the R session before deciding that something did
# not work.
# Changes don't activate until after a re-install.


#' @export
geom2trace.GeomPcp <- function(data, params, p) {
  browser()
  # seps <- get.separators()
  #  level <- NULL # visible binding
  #  data <- subset(data, level == max(level))
  #  data$hovertext <- gsub("\n", "<br>", data$label)
  # data$hovertext <- gsub(seps[1], ": ", data$hovertext)
  #  data$hovertext <- paste0(data$hovertext, "<br>Frequency: ", data[[".wt"]])
  data$key <- data$group
  data <- getFromNamespace("geom2trace.GeomPath", asNamespace("plotly"))(data)
browser()
  data
}

