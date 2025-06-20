test_that("plot_bar_by_vars works correctly with default parameters", {
  data("GlobalPatterns", package = "phyloseq")
  # Execute function with example dataset
  plots <- plot_bar_by_vars(GlobalPatterns)
  # Verify a list type object is returned
  expect_type(plots, "list")
  # Verify there is a plot for each variable
  expect_length(plots, length(sample_variables(GlobalPatterns)))
  # Verify each element is a ggplot object
  lapply(plots, function(p) expect_s3_class(p, "gg"))
})
test_that("plot_bar_by_vars throws error for invalid rank", {
  data("GlobalPatterns", package = "phyloseq")
  expect_error(
    plot_bar_by_vars(GlobalPatterns, rank = "FakeRank"),
    "no existe en el objeto phyloseq"
  )
})
