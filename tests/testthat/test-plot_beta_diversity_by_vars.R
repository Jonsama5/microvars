test_that("plot_beta_diversity_by_vars returns a plot list with PERMANOVA", {
  data(GlobalPatterns)
  plots <- plot_beta_diversity_by_vars(GlobalPatterns)
  expect_type(plots, "list")
  expect_true(all(sapply(plots, inherits, "ggplot")))
})
