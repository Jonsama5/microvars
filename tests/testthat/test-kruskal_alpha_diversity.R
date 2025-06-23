test_that("kruskal_alpha_diversity returns correct results", {
  data(GlobalPatterns)

  # Run the function with default measures
  results <- kruskal_alpha_diversity(GlobalPatterns, "SampleType")

  # Check that result is a data frame
  expect_s3_class(results, "data.frame")

  # Check that it contains the expected columns
  expect_true(all(c("Measure", "Statistic", "p_value") %in% colnames(results)))

  # Check that the number of rows equals the number of measures tested
  expected_measures <- c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher")
  expect_equal(nrow(results), length(expected_measures))
  expect_equal(results$Measure, expected_measures)

  # Test error when group_var not in sample_data
  expect_error(kruskal_alpha_diversity(GlobalPatterns, "nonexistent_var"))

  # Test with subset of measures
  subset_measures <- c("Observed", "Shannon")
  subset_results <- kruskal_alpha_diversity(GlobalPatterns, "SampleType", measures = subset_measures)
  expect_equal(nrow(subset_results), length(subset_measures))
  expect_equal(subset_results$Measure, subset_measures)
})
