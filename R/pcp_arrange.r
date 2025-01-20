#' Data wrangling for GPCPs: Step 3 order observations in factor variables
#'
#' Break ties for levels in factor variables, space cases out equally and set an order.
#' Note that only ties in **factor** variables are addressed this way.
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
#' @param data data frame - preferably processed using `pcp_select` and `pcp_scale`.
#' @param method method for breaking ties, one of "from-right", "from-left" or "from-both".
#' @param space number between 0 and 1, indicating the proportion of space used for separating multiple levels.
#' @param .by_group logical value. If TRUE, scaling will respect any previous grouping variables. Applies to grouped data frames only.
#' @return data frame of the same size as the input data; values of `pcp_y` and
#' `pcp_yend` are adjusted for `pcp_class` == "factor"
#' @importFrom dplyr mutate select summarize across n rename starts_with ungroup
#' @importFrom dplyr arrange group_by
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang :=
#' @seealso [pcp_select()], [pcp_scale()]
#' @export
#' @examples
#' library(ggplot2)
#' data(Carcinoma)
#' # select scores
#' pcp_data <- Carcinoma |>
#'   pcp_select(A:G) |>
#'   pcp_scale()
#'
#' # y values are on five different values
#' table(pcp_data$pcp_y)
#'
#' # spread out y values
#' pcp_data  |> pcp_arrange() |>
#'   ggplot(aes(x = pcp_y)) + geom_histogram(binwidth=0.05)
pcp_arrange <- function(data, method = "from-right", space = 0.05, .by_group = TRUE) {
  pcp_id <- pcp_x <- pcp_y <- pcp_yend <-  pcp_class  <- NULL

  pcp_right <- pcp_left <- replace_y <- replace_yend <- NULL

  # figure out group structure
  if (.by_group == FALSE) data <- data %>% ungroup()
  groups <- names(attr(data, "groups"))
  if (length(groups) > 0) groups <- setdiff(groups, ".rows")

  add_space <- function(subdata, space, replace_var, y_var) {
    #    browser()

    subdata %>% mutate(
      "{{ replace_var }}" := (1- space) * {{ replace_var }} + {{ y_var }}*space
    )
  }


  from_right <- function(subdata, index_start, index_last, var_y, replace_var) {
#    ..replace.. <- NULL
#    browser()
    # if index_last is larger than index_start, it will be an ordering
    # from the right, otherwise it is an ordering from the left
    select1 <- setdiff(unique(c(groups, "pcp_id", paste0(".pcp.",vars$pcp_x[index_start:index_last]))), "pcp_x")
    select2 <- setdiff(unique(c(groups, "pcp_id", paste0(".pcp.",vars$pcp_x[index_start]))), "pcp_x")
    vary <- rlang::enquo(var_y)

    subdata <- subdata %>%
      tidyr::pivot_wider(names_from = .data$pcp_x, values_from = {{ var_y }},
                         names_prefix=".pcp.", names_repair="unique") %>%
      select(select1) %>%
      arrange(across(-.data$pcp_id)) %>%
      select(select2) %>%
      tidyr::pivot_longer(cols= paste0(".pcp.",vars$pcp_x[index_start]),
                          names_to = "pcp_x", values_to = as_label(vary), names_prefix = ".pcp.")


    subdata <- subdata %>% mutate(
      "{{ replace_var }}" := (1:n() - 0.5)/n()*max({{ var_y }}, na.rm=TRUE)
      #      replace = (1- space) * replace + value*(space-spacer) + spacer/2
    )
    # now remove the original variable and rename
#    replace_name <- as_label(replace_var)
#    subdata[[replace_name]] <- subdata$..replace..
#    subdata %>% select(-.data$..replace..)
    subdata
  }
  vars <- data %>% group_by(pcp_x, .add = FALSE) %>%
    summarize(pcp_class = pcp_class[1]) %>% mutate(pcp_x = as.character(pcp_x))
  targets <- which(vars$pcp_class == "factor")
  numvars <- nrow(vars)

  selects <- unique(c(groups, "pcp_id", "pcp_x", "pcp_y"))
  if (method == "from-right") { # order observations in factor variables by values of the variables from the right. If the last variable is a factor variable, use the same principle, but from the left.
    foo <- sapply(rev(targets), function(index_start) {
    #  browser()
      if (index_start < numvars)
        index_end <- numvars
      else
        index_end <- 1

      block <- data %>%
        filter(pcp_x %in% vars$pcp_x[index_start:index_end]) %>%
        select(selects)
      b1 <- from_right(block, index_start, index_end, pcp_y, replace)
      b1 <- add_space(b1, space = space, replace, pcp_y)
#      browser()


      bys <- unique(c(groups, "pcp_id", "pcp_x"))
      selects2 <- unique(c(groups, "pcp_id", "pcp_x", "replace"))
      data <<- data %>%
        left_join(b1 %>% select(selects2), by=bys)
      data <<- data %>% mutate(
        pcp_y = ifelse(!is.na(replace) & !is.na(pcp_y), replace, pcp_y),
        pcp_x = factor(pcp_x, levels = vars$pcp_x)
      ) %>% select(-replace)
    })
  }


  if (method == "from-left") { # order observations in factor variables by values of the variables from the left.
    foo <- sapply(targets, function(index_start) {
      if (index_start == 1)
        index_end <- numvars
      else
        index_end <- 1

 #     browser()
      block <- data %>%
        filter(pcp_x %in% vars$pcp_x[index_start:index_end]) %>%
        select(selects)
      b1 <- from_right(block, index_start, index_end, pcp_y, replace)
      b1 <- add_space(b1, space = space, replace, pcp_y)

      bys <- unique(c(groups, "pcp_id", "pcp_x"))
      selects2 <- unique(c(groups, "pcp_id", "pcp_x", "replace"))
      data <<- data %>%
        left_join(b1 %>% select(selects2), by=bys)
      data <<- data %>% mutate(
        pcp_y = ifelse(!is.na(replace), replace, pcp_y),
        pcp_x = factor(pcp_x, levels = vars$pcp_x)
      ) %>% select(-replace)
    })
  }

  if (method == "from-both") { # order observations in factor variables by values of the variables from the right. If the last variable is a factor variable, use the same principle, but from the left.

#    browser()

    right <- data %>% select(selects, "pcp_yend") %>% select(-pcp_y) %>%
      pivot_wider(names_from="pcp_x", values_from="pcp_yend")

    left <- data %>% select(selects, "pcp_yend") %>% select(-pcp_yend) %>%
      pivot_wider(names_from="pcp_x", values_from="pcp_y")

    foo <- sapply(targets, function(index_start) {
 #browser()
      index_right <- index_start
      if (index_start < numvars) index_right <- index_right + 1
      # sort from right
      block_right <- data.frame(pcp_id = left$pcp_id, pcp_yend = right[[vars$pcp_x[index_start]]],
                                left %>% select(vars$pcp_x[index_right:numvars]),
                                pcp_y = left %>% select(vars$pcp_x[index_start]))
      block_right <- block_right %>% arrange(across(-pcp_id)) %>% # the last variable's pcp_yend is copied from it's own pcp_y
        mutate(
          replace_yend = (1:n() - 0.5)/n()*max(pcp_yend, na.rm=TRUE)
        ) %>% arrange(pcp_id)
      block_right <- add_space(block_right, space = space, replace_yend, pcp_yend)
      right[[vars$pcp_x[index_start]]] <<- block_right$replace_yend


      # sort from left
      if (index_start == 1) {
        left[[vars$pcp_x[index_start]]] <<- block_right$replace_yend
      #  block$replace_y = block$replace_yend
      } else {
      block_left <- data.frame(pcp_id = right$pcp_id, pcp_y = left[[vars$pcp_x[index_start]]],
                               right %>% select(vars$pcp_x[(index_start-1):1]) %>% select(-pcp_id))  # the first variable's pcp_y is copied from pcp_yend
      block_left <- block_left %>% arrange(across(-pcp_id)) %>%
        mutate(
          replace_y = (1:n() - 0.5)/n()*max(pcp_y, na.rm=TRUE)
        ) %>% arrange(pcp_id)
      block_left <- add_space(block_left, space = space, replace_y, pcp_y)

      left[[vars$pcp_x[index_start]]] <<- block_left$replace_y
      }
    })

#    browser()
    # now incorporate left and right indices back into the data frame
    replace_yend <- right %>%
      pivot_longer(-pcp_id, names_to="pcp_x", values_to="replace_yend")

    replace_y <- left %>%
      pivot_longer(-pcp_id, names_to="pcp_x", values_to="replace_y")

    data <- data %>% left_join(replace_yend %>% select("pcp_id", "pcp_x", starts_with("replace_")),
                               by=c("pcp_x", "pcp_id"))
    data <- data %>% left_join(replace_y %>% select("pcp_id", "pcp_x", starts_with("replace_")),
                               by=c("pcp_x", "pcp_id"))
    if (!is.null(data$replace_y)) {
      data <- data %>% mutate(
        pcp_y = ifelse(!is.na(replace_y), replace_y, pcp_y)
      )
    }
    if (!is.null(data$replace_yend)) {
      data <- data %>% mutate(
        pcp_yend = ifelse(!is.na(replace_yend), replace_yend, pcp_yend)
      )
    }
#    browser()
    data <- data %>% select(-starts_with("replace_")) %>%
      mutate(pcp_x = factor(pcp_x, levels = vars$pcp_x))
  }



  if (method == "from-both-keep") { # order observations in factor variables by values of the variables from the right. If the last variable is a factor variable, use the same principle, but from the left.
  #  browser()

    foo <- sapply(targets, function(index_start) {
    #    browser()
        index_left <- index_start - 1
        index_right <- index_start + 1
        if (index_start == 1) {
          index_left <- index_start
        }
        if (index_start == numvars) {
          index_right <- index_start
        }
        block <- data %>%
          filter(pcp_x %in% vars$pcp_x[index_start])
        right <- data %>%
          filter(pcp_x %in% vars$pcp_x[index_right]) %>% ungroup()
        left <- data %>%
          filter(pcp_x %in% vars$pcp_x[index_left]) %>% ungroup()

        block <- block %>% left_join(
          right %>% select(pcp_y, pcp_id) %>% rename(pcp_right=pcp_y),
          by="pcp_id")
        block <- block %>% arrange(pcp_yend, pcp_right) %>%
          mutate(
            replace_yend = (1:n() - 0.5)/n()*max(pcp_yend, na.rm=TRUE)
            )

        if (index_start == 1) block$replace_y = block$replace_yend
        else {
          block <- block %>% left_join(
            left %>% select(pcp_yend, pcp_id) %>% rename(pcp_left=pcp_yend),
            by="pcp_id")
        block <- block %>% arrange(pcp_y, pcp_left) %>%
          mutate(
            replace_y = (1:n() - 0.5)/n()*max(pcp_y, na.rm=TRUE)
          )
        }

        block <- add_space(block, space = space, replace_y, pcp_y)
        block <- add_space(block, space = space, replace_yend, pcp_y)


        data <- data %>% left_join(block %>% select("pcp_id", "pcp_x", starts_with("replace_")),
                           by=c("pcp_x", "pcp_id"))
        if (!is.null(data$replace_y)) {
        data <- data %>% mutate(
          pcp_y = ifelse(!is.na(replace_y), replace_y, pcp_y)
        )
        }
        if (!is.null(data$replace_yend)) {
        data <- data %>% mutate(
          pcp_yend = ifelse(!is.na(replace_yend), replace_yend, pcp_yend)
        )
        }
        data <<- data %>% select(-starts_with("replace_"))


    })
  }

  if (method != "from-both")
    data$pcp_yend <- data$pcp_y
  data
}
