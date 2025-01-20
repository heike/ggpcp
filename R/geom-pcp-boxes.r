#' @inherit geom_pcp
#' @export
#' @param boxwidth width of the box for a level on a categorical axis, defaults to 0.2.
#' @param ... other arguments passed on to `layer`. These are often aesthetics, used to set an aesthetic to a fixed value, like `color = 'red'` or `size = 3`. They may also be parameters to the paired geom/stat.
geom_pcp_boxes <- function(mapping = NULL, data = NULL, stat = "identity",
                        position = "identity", na.rm = FALSE,
                        show.legend = NA, inherit.aes = TRUE,
                        boxwidth = 0.2, ...)
{

  # browser()


  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPcp_boxes,
    position = position,
    show.legend = show.legend,
    check.aes = FALSE,
    check.param = FALSE,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      boxwidth = boxwidth,
#      space = space,
      ...
    )
  )
}

#' @rdname GeomPcp
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom grid grobTree
GeomPcp_boxes <- ggplot2::ggproto(
  "GeomPcp_boxes",
  ggplot2::Geom,
  required_aes = c("x", "y", "class", "level"),
#  non_missing_aes = character(),
#  optional_aes = character(),
  extra_params = c("na.rm", "boxwidth"),

  boxwidth = 0.1,

  default_aes = aes("x", "y",
    colour = "grey60",  fill = alpha("grey50", alpha = 0.5), size = 0.25,
    linetype = "solid", alpha = NA,
    linewidth=.1, stroke = 2
  ),

  setup_params = function(data, params) {
  #  browser()
    if (!dplyr::between(params$boxwidth, 0, 1)) {
      stop("parameter boxwidth must be a value between 0 and 1.", call. = FALSE)
    } else
      boxwidth <<- params$boxwidth

    # if (!dplyr::between(params$space, 0, 1)) {
    #   stop("parameter space must be a value between 0 and 1.", call. = FALSE)
    # } else
    #   space <<- params$space

    params
    },

  setup_data = function(data, params) {
 #   browser()
  #  data <- data %>% mutate(
  #    y = (y - min(y, na.rm=TRUE))/(max(y,na.rm=TRUE) - min(y, na.rm=TRUE))
  #  )

    data
  },

  draw_key = ggplot2::draw_key_rect,

  # handle_na = function(self, data, params) {
  #   #browser()
  #   remove_missing(data, params$na.rm,
  #                  c(self$required_aes, self$non_missing_aes),
  #                  snake_class(self)
  #   )
  # },

# draw_layer = function(self, data, params, layout, coord) {
#   browser()
#   nxy <- data %>% filter(class=="factor") %>% group_by(x, y) %>% summarize(ny = n())
#
#   nxy <- nxy %>% ungroup(y) %>% mutate(n = sum(ny), distinct = n())
#   nxy <- nxy %>% arrange(x, y) %>% mutate(cumsum_ny = cumsum(ny))
#   #    data$xintercept = data$x
#   nxy <- nxy %>% group_by(x) %>% mutate(
#     ymin = c(0, cumsum_ny[-n()])/n,
#     ymax = cumsum_ny/n
#   )
#
#   unique_rows <- row.names(unique(data[data$class=="factor",c("x", "y")]))
#   rows <- data[unique_rows,]
#   rows$x <- as.integer(rows$x)
#
#   subdata <- data.frame(
#     xmin = nxy$x-0.1, xmax = nxy$x+0.1, ymin = nxy$ymin, ymax = nxy$ymax,
#     x = nxy$x, y = nxy$y)
#   subdata <- left_join(subdata, rows, by=c("x", "y"))
#
#   GeomRect$draw_layer(subdata, params, layout, coord)
# },

  draw_panel = function(self, data, panel_params, coord, ...) {
#    browser()
# return if there are no factor variables and there is nothing to draw
    ns <- data %>% filter(class=="factor") %>% nrow()
    if (ns == 0) return()

    # dy the smallest entity between two successive values
# need to make sure that dy is not equal to 0 - which happens for a group of size 1
    dys <- data %>% filter(class=="factor") %>%
      group_by(x, level) %>%
      summarize(
        dy = diff(sort(y))[1])

    dy <- dys$dy[dys$dy>0][1]

    subdata <- data %>% filter(class=="factor") %>%
      group_by(x, level) %>%
      summarize(
        ymin = min(y, na.rm=TRUE)-dy/2,
        ymax = max(y, na.rm=TRUE)+dy/2,
        xmin = mean(x, na.rm=TRUE) - boxwidth/2,
        xmax = mean(x, na.rm=TRUE) + boxwidth/2,
        x = mean(x, na.rm=TRUE)
      )


#     nxy <- data %>% filter(class=="factor") %>% group_by(x, y) %>% summarize(ny = n())
#
#     nxy <- nxy %>% ungroup(y) %>% mutate(n = sum(ny))
#     nxy <- nxy %>% arrange(x, y) %>%
#       mutate(
#         cumsum_ny = cumsum(ny),
#         cumsum_n = 0:(n()-1)/(n()-1)
#         )
# #    data$xintercept = data$x
#     nxy <- nxy %>% group_by(x) %>% mutate(
#       ymin = (1-space) * c(0, cumsum_ny[-n()])/n + space*cumsum_n,
#       ymax = (1-space) * cumsum_ny/n + space*cumsum_n
#     )

    unique_rows <- row.names(unique(data[data$class=="factor",c("x", "level")]))
    rows <- data[unique_rows,]
    rows$x <- as.integer(rows$x)

#    subdata <- data.frame(
#      xmin = nxy$x-boxwidth/2, xmax = nxy$x+boxwidth/2, ymin = nxy$ymin, ymax = nxy$ymax,
#      x = nxy$x, y = nxy$y)
    subdata <- left_join(subdata, rows %>% mutate(y = as.numeric(y)), by=c("x", "level"))

  #  browser()
    # coords <- coord$transform(data, panel_params)

    GeomRect$draw_panel(data = subdata, panel_params, coord, ...)
  },

  draw_group = function(self, data, panel_params, coord, ...) {
    # browser()

#    data$xintercept = data$x

#    GeomVline$draw_layer(data, panel_params, layout, coord)
  }#,

  # setup_params = function(data, params) {
  #   #browser()
  #   params
  #   },
  #
  # setup_data = function(data, params) {
  #   #browser()
  #   data
  #   }

)
