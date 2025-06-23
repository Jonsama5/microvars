test_that("plot_alpha_diversity_by_vars returns a plot list", {
  data(GlobalPatterns)

  plots <- plot_alpha_diversity_by_vars(GlobalPatterns, measures = c("Shannon", "Simpson"))
  expect_type(plots, "list")
  expect_true(all(sapply(plots, inherits, "ggplot")))
})
