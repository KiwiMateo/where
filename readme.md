# The `where` package

The `where` package has one main function `where::do` which provides a clean
syntax for vectorising the use of NSE, for example in `ggplot2`, `dplyr`, or
`data.table`. There are also two (infix) wrappers `%where%` and `%for%` that
provide arguably cleaner syntax.

A simple, if contrived, example:

```r
library(do)
library(dplyr)

subgroups = .(all        = TRUE,
              long_sepal = Sepal.Length > 6,
              long_petal = Petal.Length > 5.5)

(iris %>%
    filter(subgroup) %>%
    summarise(across(Sepal.Length:Petal.Width,
                     summary),
              .by = Species)) %where%
  list(subgroup = subgroups,
       summary  = .(mean, sum, prod))
```

## Installation

Can be installed with

```r
remotes::install_github("KiwiMateo/where")
```
