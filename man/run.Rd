% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run.R
\name{run}
\alias{run}
\alias{\%for\%}
\alias{\%where\%}
\title{Run interpolated code}
\usage{
run(expr, ..., e = parent.frame())

expr \%for\% x

expr \%where\% pars
}
\arguments{
\item{expr}{the code to run}

\item{...}{named values to be substituted by name into `expr`}

\item{e}{environment, for evaluation; defaults to `parent.frame()`}

\item{x}{list of expressions to be substituted for `x` in `expr`}

\item{pars}{a named list of values to be substituted by name into `expr`}
}
\value{
A list.
}
\description{
Vectorised substitution of expressions into a large code block and
  execution.
}
\details{
`%where%` is a infixed version of run.
  `%for%` takes only one list of expressions to be substituted for `x`.
}
\examples{
   library(dplyr)

   subgroups = .(all        = TRUE,
                 long_sepal = Sepal.Length > 6,
                 long_petal = Petal.Length > 5.5)
   functions = .(mean, sum, prod)

   run(
     iris \%>\%
       filter(subgroup) \%>\%
       summarise(across(Sepal.Length:Petal.Width,
                        summary),
                 .by = Species),
     subgroup = subgroups,
     summary  = functions
    )

   library(data.table)
   df <- as.data.table(iris)

   run(df[subgroup, lapply(.SD, functions), keyby = "Species",
         .SDcols = Sepal.Length:Petal.Width],

      subgroup  = subgroups,
      functions = functions)

   library(ggplot2)

   plots <- run(
     ggplot(filter(iris, subgroup),
            aes(Sepal.Length, Sepal.Width)) +
       geom_point() +
       theme_minimal(),
   subgroup = subgroups
   )
   Map(function(plot, name) plot + ggtitle(name), plots, names(plots))

   (
    iris \%>\%
      filter(subgroup) \%>\%
      summarise(across(Sepal.Length:Petal.Width,
                       summary),
                .by = Species)
   ) \%where\%
    list(subgroup = subgroups,
         summary  = functions)

  library(ggplot2)
  (
    ggplot(filter(iris, x),
           aes(Sepal.Length, Sepal.Width)) +
      geom_point() +
      theme_minimal()
  ) \%for\% subgroups
}
