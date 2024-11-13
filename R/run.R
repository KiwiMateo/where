#' Capture expressions
#'
#' @param ... code
#'
#' @return a list
#' @export
#'
#' @examples
#'    .(a = 1, b = x^2, c = filter(iris, Species == "veriscolor"))
`.` <- function(...) as.list(match.call()[-1])

#' Run interpolated code
#'
#'   Vectorised substitution of expressions into a large code block and
#'   execution.
#'
#'   `%where%` is a infixed version of run.
#'   `%for%` takes only one list of expressions to be substituted for `x`.
#'
#' @param expr the code to run
#' @param ... named values to be substituted by name into `expr`
#' @param e environment, for evaluation; defaults to `parent.frame()`
#' @param x list of expressions to be substituted for `x` in `expr`
#' @param pars a named list of values to be substituted by name into `expr`
#'
#' @return A list.
#' @export
#'
#' @examples
#'    library(dplyr)
#'
#'    subgroups = .(all        = TRUE,
#'                  long_sepal = Sepal.Length > 6,
#'                  long_petal = Petal.Length > 5.5)
#'    functions = .(mean, sum, prod)
#'
#'    run(
#'      iris %>%
#'        filter(subgroup) %>%
#'        summarise(across(Sepal.Length:Petal.Width,
#'                         summary),
#'                  .by = Species),
#'      subgroup = subgroups,
#'      summary  = functions
#'     )
#'
#'    library(data.table)
#'    df <- as.data.table(iris)
#'
#'    run(df[subgroup, lapply(.SD, functions), keyby = "Species",
#'          .SDcols = Sepal.Length:Petal.Width],
#'
#'       subgroup  = subgroups,
#'       functions = functions)
#'
#'    library(ggplot2)
#'
#'    plots <- run(
#'      ggplot(filter(iris, subgroup),
#'             aes(Sepal.Length, Sepal.Width)) +
#'        geom_point() +
#'        theme_minimal(),
#'    subgroup = subgroups
#'    )
#'    Map(function(plot, name) plot + ggtitle(name), plots, names(plots))
#'
#'    (
#'     iris %>%
#'       filter(subgroup) %>%
#'       summarise(across(Sepal.Length:Petal.Width,
#'                        summary),
#'                 .by = Species)
#'    ) %where%
#'     list(subgroup = subgroups,
#'          summary  = functions)
#'
#'   library(ggplot2)
#'   (
#'     ggplot(filter(iris, x),
#'            aes(Sepal.Length, Sepal.Width)) +
#'       geom_point() +
#'       theme_minimal()
#'   ) %for% subgroups
run <- function(expr,
               ...,
               e = parent.frame()) {
  expr   <- substitute(expr)
  if (...length() == 0) return(list(eval(expr)))
  values <- list(...)
  if (!all(names(values) %in% all.vars(expr, functions = TRUE)))
    stop("some values not in expr: ",
         paste(setdiff(names(values), all.vars(expr)), sep = ", "))

  ls     <- lengths(values)
  m      <- max(ls)
  if (!all(ls %in% c(1, m)))
    stop("all elements of `...` should have the same length or length 1")
  for (i in seq_along(values))
    if (ls[i] < m) values[[i]] <- list(values[[i]])[rep(1, m)]

  f <- function() NULL
  formals(f) <- structure(alist(x = )[rep(1, length(values))],
                          names = names(values))
  args <- as.call(c(as.name("list"), structure(lapply(names(values), as.name),
                                               names = names(values))))
  body(f) <- substitute(eval(do.call("substitute", list(expr, arguments)), values, e),
                        list(arguments = args))

  do.call("mapply", c(list(FUN = f), values, SIMPLIFY = FALSE))
}

#' @rdname run
#' @export
`%for%` <- function(expr, x) {
  e <- parent.frame()
  do.call(run, list(substitute(expr), x = x, e = e))
}

#' @rdname run
#' @export
`%where%` <- function(expr, pars){
  e <- parent.frame()
  do.call(run, c(list(substitute(expr), e = e), pars))
}

#' Posterior variable declaration
#'
#' @param expr expression to evaluate
#' @param variables expression with variable assignments
#'
#' @return The value of the evaluated expression.
#' @export
#'
#' @examples
#'   (a + b) %with% {
#'     a = 1
#'     b = 2
#'   }
`%with%` <- function(expr,
                     variables) {
  # The more elegant body {variables; expr} pollutes the call environment
  # even when wrapped in local
  e <- new.env(parent = parent.frame(2))
  eval(substitute(variables), envir = e)
  eval(substitute(expr), envir = e)
}
