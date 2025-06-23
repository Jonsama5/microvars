test_that("dunn_post_hoc_phy returns valid results", {
  # Load test data
  data(GlobalPatterns)

  # Run the function
  results <- dunn_post_hoc_phy(GlobalPatterns, measures = c("Shannon", "Simpson"))

  # Check that the result is a data frame
  expect_s3_class(results, "data.frame")

  # Check that it has some rows
  expect_gt(nrow(results), 0)

  # Check that it has expected columns
  expected_cols <- c("Comparison", "Z", "P_uncorrected", "P_adjusted", "Variable", "Index")
  expect_true(all(expected_cols %in% colnames(results)))
})
