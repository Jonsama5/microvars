#Since the function does not return any value, the test is limited to the acceptance of a list.
test_that("plotwatcher accepts a list of ggplot objects without error", {

  p1 <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
  p2 <- ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Sepal.Width)) + ggplot2::geom_point()

  plotlist <- list(p1, p2)

  # Check that calling the function does not error out
  expect_error(plotwatcher(plotlist), NA)
})
