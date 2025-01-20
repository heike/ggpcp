#' Data wrangling for GPCPs: Step 2 scale values
#'
#' @description
#' The function `pcp_scale` provides access to a set of transformations to use
#' in parallel coordinate plots. All transformations other than `raw` tend to
#' produce y values in the interval from 0 and 1.
#'
#' @details
#' The data pipeline feeding any of the geom layers in the `ggpcp` package is
#' implemented in a three-step modularized form rather than as the stat
#' functions more typical for `ggplot2` extensions.
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
#' `method` is a character string that denotes how to scale the variables
#' in the parallel coordinate plot. Options are named in the same way as the options in [GGally::ggparcoord()]:
#' \itemize{
#'   \item{`raw`}{: raw data used, no scaling will be done.}
#'   \item{`std`}{: univariately, subtract mean and divide by standard deviation. To get values into a unit interval we use a linear transformation of f(y) = y/4+0.5. }
#'   \item{`robust`}{: univariately, subtract median and divide by median absolute deviation. To get values into an expected interval of unit interval we use a linear transformation of f(y) = y/4+0.5. }
#'   \item{`uniminmax`}{: univariately, scale so the minimum of the variable is zero, and the maximum is one.}
#'   \item{`globalminmax`}{: global scaling; the global maximum is mapped to 1,
#'     global minimum across the variables is mapped to 0. }
#' }
#' @param data data frame as returned by `select_pcp`
#' @param method string specifying the method that should be used for scaling the values
#' in a parallel coordinate plot (see Details).
#' @param .by_group logical value. If TRUE, scaling will respect any previous grouping variables. Applies to grouped data frames only.
#' @return data frame of the same size as the input data; values of `pcp_y` and
#' `pcp_yend` are scaled according to the specified method.
#' @seealso [pcp_select()], [pcp_arrange()]
#' @importFrom stats median mad
#' @importFrom assertthat assert_that has_name
#' @export
#' @examples
#' data(Carcinoma)
#' dim(Carcinoma)
#' # select all variables
#' pcp_data <- Carcinoma |> pcp_select(1:9)
#' summary(pcp_data)
#' pcp_data |> pcp_scale() |> summary()
#' # scaling gets values of pcp_y and pcp_yend between 0 and 1
pcp_scale <- function(data, method = "uniminmax", .by_group = TRUE) {

  assert_that(has_name(data, "pcp_x"))
  assert_that(has_name(data, "pcp_level"))
  assert_that(has_name(data, "pcp_y"))
  assert_that(!is.null(method))
  assert_that(method %in% c("raw", "uniminmax", "robust", "std", "globalminmax"))
  assert_that(is.numeric(data$pcp_y))


  if (method == "raw") {
    data <- group_by(data, .data$pcp_x)
    # nothing else necessary but to run the above checks
  }
  if (method == "std") {
    data <- group_by(data, .data$pcp_x, .add = .by_group)
    data <- mutate(
      data,
      pcp_y = scale(.data$pcp_y, center = TRUE, scale=TRUE),
      pcp_y = .data$pcp_y/4.0+0.5
    )
  }
  if (method == "robust") {
    data <- group_by(data, .data$pcp_x, .add = .by_group)
    data <- mutate(
      data,
      pcp_y = (.data$pcp_y - median(.data$pcp_y, na.rm = TRUE)) / ifelse(mad(.data$pcp_y, na.rm = TRUE) == 0, 1, mad(.data$pcp_y, na.rm = TRUE)),
      pcp_y = .data$pcp_y/4.0+0.5
    )
  }

  if (method == "uniminmax") {
    data <- group_by(data, .data$pcp_x, .add = .by_group)
    data <- mutate(
      data,
      pcp_y = (.data$pcp_y-min(.data$pcp_y, na.rm=TRUE))/ifelse((max(.data$pcp_y, na.rm=TRUE)-min(.data$pcp_y, na.rm=TRUE)) == 0,
                                                    1, (max(.data$pcp_y, na.rm=TRUE)-min(.data$pcp_y, na.rm=TRUE)))
    )
  }
  if (method == "globalminmax") {
    # no grouping unless specified beforehand - do we need to ungroup? XXX should test that
    data <- mutate(
      data,
      pcp_y = (.data$pcp_y-min(.data$pcp_y, na.rm=TRUE))/ifelse((max(.data$pcp_y, na.rm=TRUE)-min(.data$pcp_y, na.rm=TRUE)) == 0,
                                                    1, (max(.data$pcp_y, na.rm=TRUE)-min(.data$pcp_y, na.rm=TRUE)))
    )
  }

  data$pcp_yend <- data$pcp_y
  data #%>% ungroup() # might want to keep track of grouping structure for use in later data processing steps
}
