test_that("microsummary returns correct structure and plot", {
  # Load example data
  data("GlobalPatterns", package = "phyloseq")
  phy <- GlobalPatterns

  result <- microsummary(phy)

  # Test structure: result is a list with expected elements
  expect_type(result, "list")
  expect_named(result, c("summary", "plot"))

  # Test that the summary is a tibble
  expect_s3_class(result$summary, "tbl_df")

  # Expected columns (adjust if you changed the naming in the function)
  expect_true(all(c(
    "TotalReads", "NumOTUs", "MeanReads", "MedianReads", "SDReads",
    "MinReads", "MaxReads", "Mean_Shannon", "Mean_Simpson",
    "Mean_bray_distance", "Mean_jaccard_distance"
  ) %in% names(result$summary)))

  # All summary values are numeric
  expect_true(all(purrr::map_lgl(result$summary, is.numeric)))

  # Expect one-row summary
  expect_equal(nrow(result$summary), 1)

  # Check that the plot is a ggplot object
  expect_s3_class(result$plot, "ggplot")
})

