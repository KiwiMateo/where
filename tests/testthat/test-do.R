library(dplyr)
library(ggplot2)

test_that("test run function", {
  # Expression only
  expect_equal(run(1 + 1), list(2))
  # Error, unused value provided
  expect_error(run(1 + 1, a = 1:2), "some values not in expr")
  # Simple expression, 1 value
  expect_equal(run(a + 1, a = 1:2), list(2, 3))
  # Simple expression, 2 values
  expect_equal(run(a + b, a = 1:2, b = 3:4), list(4, 6))
  # Simple expression, 2 expr values
  expect_equal(run(a + 1, a = .(5, 6)), list(6, 7))

  # Typical expressions
  subgroups <- .(all        = TRUE,
                 long_sepal = Sepal.Length > 6,
                 long_petal = Petal.Length > 5.5)
  functions <- .(mean, sum, prod)

  outs1 <- list(
    one = run(# Typical expression, 1 value
      iris %>%
        filter(subgroup) %>%
        summarise(across(Sepal.Length:Petal.Width,
                         mean),
                  .by = Species),
      subgroup = subgroups
    ),
    two = run(# Typical expression, 2 values
      iris %>%
        filter(subgroup) %>%
        summarise(across(Sepal.Length:Petal.Width,
                         summary),
                  .by = Species),
      subgroup = subgroups,
      summary  = functions
    ),
    two_diff = run(# Typical expression, 2 values, 1 of length 1
      iris %>%
        filter(subgroup) %>%
        summarise(across(Sepal.Length:Petal.Width,
                         summary),
                  .by = Species),
      subgroup = subgroups,
      summary  = mean
    )
  )

  for (out in outs1) {
    expect_equal(names(out), names(subgroups))
    expect_true(all(sapply(out, is.data.frame)))
    expect_equal(sapply(out, nrow), c(all = 3, long_sepal = 2, long_petal = 1))
    expect_equal(out[["all"]][["Sepal.Length"]],
                 as.vector(tapply(iris[["Sepal.Length"]], iris[["Species"]], mean)))
  }
  expect_equal(outs1[["two"]][["long_sepal"]][["Sepal.Width"]],
               with(iris[iris[["Sepal.Length"]]  > 6,],
                    as.vector(tapply(Sepal.Width, as.vector(Species), sum))))
  for (i in seq_along(subgroups))
    expect_identical(outs1[["one"]][[i]],
                     outs1[["two_diff"]][[i]])


  # run within a function
  f <- function(df) run(with(df, df[subgroup, ]),
                       subgroup = subgroups)
  outs2 <- list(all        = f(iris),
                versicolor = f(iris[iris[["Species"]] == "versicolor", ]))

  for (out in outs2) {
    expect_equal(names(out), names(subgroups))
    expect_true(all(sapply(out, is.data.frame)))
  }
  expect_identical(outs2[["all"]],
                   run(with(iris, iris[subgroup, ]), subgroup = subgroups))
  expect_identical(outs2[["versicolor"]],
                   run(with(iris, iris[Species == "versicolor" & subgroup, ]),
                      subgroup = subgroups))

  # run within a function, passing expr
  apply_over_groups <- function(expr,
                                populations = subgroups) {
    e <- parent.frame()
    eval(substitute(run(expr, subgroup = populations, e = e),
                    list(expr = substitute(expr))))
  }

  expect_identical(run(with(iris, iris[subgroup, ]),
                          subgroup = subgroups),
                   apply_over_groups(with(iris, iris[subgroup, ])))

  expect_identical(outs1[["one"]],
                   apply_over_groups(
                     iris %>%
                       filter(subgroup) %>%
                       summarise(across(Sepal.Length:Petal.Width,
                                        mean),
                                 .by = Species)))

  expect_identical(run(ggplot(filter(iris, subgroup),
                                 aes(Sepal.Length, Sepal.Width)) +
                            geom_point() +
                            theme_minimal(),
                          subgroup = subgroups),
                   apply_over_groups(
                     ggplot(filter(iris, subgroup),
                            aes(Sepal.Length, Sepal.Width)) +
                       geom_point() +
                       theme_minimal()))

  # Infix functions
  expect_identical(outs1[[1]],
                   (iris %>%
                      filter(x) %>%
                      summarise(across(Sepal.Length:Petal.Width,
                                       mean),
                                .by = Species)) %for% subgroups)

  expect_identical(outs1[["two_diff"]],
                   (iris %>%
                      filter(subgroup) %>%
                      summarise(across(Sepal.Length:Petal.Width,
                                       summary),
                                .by = Species)) %where% list(subgroup = subgroups,
                                                             summary  = mean))

  expect_equal((a + b) %with% {a = 1
                               b = 2},
               3)

  e <- new.env()
  local((a + b) %with% {a = 1
                        b = 2},
        e)
  expect_length(ls(envir = e), 0)
})
