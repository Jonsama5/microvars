test_that("plot_bar_by_tax returns a list of ggplots for each taxonomic rank", {
  # Phyloseq object created from example dataset
  data("GlobalPatterns", package = "phyloseq")
  # Execute function
  result <- plot_bar_by_tax(GlobalPatterns)
  # Verify object type
  expect_type(result, "list")
  # Verify each element is a ggplot type one
  expect_true(all(sapply(result, function(x) inherits(x, "gg"))))
  # Make sure the list contains an element for each taxonomic rank
  expect_equal(length(result), length(rank_names(GlobalPatterns)))
})
