#' Data wrangling for GPCPs: Step 1 variable selection
#'
#' @description
#' The `pcp_select` function allows a selection of variables from a data set.
#' These variables are transformed into an embellished long form of the data.
#' @details
#' The data pipeline feeding any of the geom layers in the `ggpcp` package is implemented in a three-step modularized
#' form rather than as the stat functions more typical for `ggplot2` extensions.
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
#' @param  data a dataframe or tibble
#' @param ... choose the columns to be used in the parallel coordinate plot.
#' Variables can be selected by position, name or any of the `tidyselect` selector functions.
#' @return dataframe of a long form of the selected variables with extra columns:
#'
#' | **variable**      | **functionality**                    |
#' |--------------|--------------------------------------|
#' | `pcp_x`, `pcp_y`     |  values for the mappings to x and y axes                       |
#' | `pcp_yend`     |  vertical endpoint of a line segment                        |
#' | `pcp_class` | type of each of the input variables |
#' | `pcp_level` | preserves order of levels in categorical variables |
#' | `pcp_id` | identifier for each observation |
#'
#' The dimensions of the returned data set are:  6 + the number of input variables for its
#' columns. The number of rows is given as the multiple of the number of selected
#' variables and the number of rows in the original data.
#' @importFrom dplyr left_join filter `%>%`
#' @importFrom tidyr gather
#' @importFrom tibble as_tibble
#' @importFrom ggplot2 fortify
#' @seealso [pcp_scale()], [pcp_arrange()]
#' @export
#' @examples
#' data(Carcinoma)
#' dim(Carcinoma)
#' # select all variables
#' pcp_data <- Carcinoma |> pcp_select(1:9)
#' dim(pcp_data) # 6 more columns, 9 times as many observations
#' head(pcp_data)
pcp_select <- function(data, ...) {
  pcp_x <- pcp_y <- pcp_yend <- NULL # make R CMD CHECK happy
  pcp_level <- NULL

  # coerce character variable
  data <- data %>% purrr::map(.f = function(x) {
    if (is.character(x)) return(factor(x))
    x
  })

  # make missing values in factors an explicit level
  data <- data %>% purrr::map(.f = function(x) {
    if (is.factor(x)) {
      if (any(is.na(x))) {
        newlevels <- c(levels(x), "NA")
        x <- as.character(x)
        x <- ifelse(is.na(x), "NA", x)
        x <- factor(x, levels=newlevels)
      }
    }
    x
  })

  data <- data %>% tibble::as_tibble()

  match <- match.call(expand.dots = TRUE)
  match <- match[-(1:2)]
  var_order <- names(data)[unlist(sapply(match, FUN = function(matchi) {
    tidyselect::eval_select(matchi, data)
  }))]

#  loc <- tidyselect::eval_select(expr(c(...)), data)

#  subdata <- data[,loc, drop=FALSE]
  subdata <- data[,var_order, drop=FALSE]
  var_order <- names(subdata)
  # make sure we distinguish between multiple calls to the same variable
  var_order <- make.unique(var_order)
  names(subdata) <- var_order

  data$pcp_id <- 1:nrow(data)
  data2 <- data
  data2[, var_order] <- lapply(subdata, FUN = as.character) # change the factors only in the copy
#  browser()
  suppressMessages({
  gather_data <- tidyr::gather(data2, pcp_x, pcp_level, var_order, factor_key=TRUE)
  })
  gather_data$pcp_y <- unlist(lapply(subdata, FUN = as.numeric))
  gather_data$pcp_yend <- gather_data$pcp_y
  # # revert factors, to make first level the top.
  # # why does ggplot2 use such effing defaults?
  # gather_data$pcp_y <- unlist(lapply(subdata, FUN = function(x) {
  #   if (is.factor(x)) x <- factor(x, levels=rev(levels(x)))
  #   as.numeric(x)
  #   }))
  gather_data$pcp_class <- rep(unlist(lapply(subdata, FUN = function(x)
    paste(class(x), collapse=" "))), each = nrow(data))
  gather_data$pcp_class <- gsub("[oO]rdered ", "", gather_data$pcp_class) # just ignore the ordered factors


  add_names <- c("pcp_id", setdiff(names(data), names(gather_data)))

  gather_data_wide <- left_join(gather_data, data[, add_names], by="pcp_id")
  gather_data_wide <- filter(gather_data_wide, !is.na(pcp_level))
  gather_data_wide <- fortify(gather_data_wide)
#  attr(gather_data_wide, "var_order") <- var_order

  gather_data_wide
}
