% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run.R
\name{.}
\alias{.}
\title{Capture expressions}
\usage{
.(...)
}
\arguments{
\item{...}{code}
}
\value{
a list
}
\description{
Capture expressions
}
\examples{
   .(a = 1, b = x^2, c = filter(iris, Species == "veriscolor"))
}
