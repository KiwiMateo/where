#' Check whether expression is 'wrapped' in "{{}}"
#'
#' @param expr qan expression
#'
#' @return Boolean
#' @export
#'
#' @examples
#'   is_wrapped(substitute(a))
#'   is_wrapped(substitute({{a}}))
is_wrapped <- function(expr)
  length(expr) == 2 &&
  length(expr[[2]]) == 2 &&
  expr[[1]] == as.symbol("{") &&
  expr[[2]][[1]] == as.symbol("{") &&
  is.symbol(expr[[2]][[2]])

#' Extract wrapped value
#'
#' @param expr an expression
#'
#' @return character if `is_wrapped(expr)`, otherwise `NULL`
#' @export
#'
#' @examples
#'   get_wrapped_value(substitute(a))
#'   get_wrapped_value(substitute({{a}}))
get_wrapped_value <- function(expr) {
  if (is_wrapped(expr)) as.character(expr[[2]][[2]])
  else NULL
}

#' Create a new string
#'
#' @param x character vector
#'
#' @return a string not in `x`
#' @export
#'
#' @examples
#'   get_new_name(letters)
get_new_name <- function(x)
  paste0(".", x[which.max(nchar(x))])

#' Process interpolated values
#'
#'   Replaces values wrapped in `{{}}` with new variables.
#'
#' @param expr an expression
#'
#' @return list with (i) an expression, (ii) named character vector with names
#'   the interpolated variables, and values the new variable names.
#' @export
make_substitutions <- function(expr) {
  vars <- all.vars(expr)
  substitutions <- c()
  substitute <- function(expr) {
    x <- get_wrapped_value(expr)

    if (length(expr) == 1) expr
    else if (!is.null(x)) {
      if (x %in% names(substitutions)) as.symbol(substitutions[[x]])
      else {
        name <- get_new_name(c(substitutions, vars))
        substitutions[x] <<- name
        as.symbol(name)
      }
    }
    else as.call(lapply(expr, substitute))
  }

  list(expr      = substitute(expr),
       arguments = substitutions)
}


#' @rdname run
#' @export
interpolate <- function(expr) {
  e <- parent.frame()
  transformed_expr <- make_substitutions(substitute(expr))
  do.call(run, c(list(transformed_expr[["expr"]], e = e),
                 structure(lapply(names(transformed_expr[["arguments"]]),
                                  get, envir = e),
                           names = transformed_expr[["arguments"]])))
}
