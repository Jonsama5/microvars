test_that("microsummary returns correct structure", {
  # Load example data
  data("GlobalPatterns", package = "phyloseq")
  phy <- GlobalPatterns

  result <- microsummary(phy)

  # Verify it is a tibble
  expect_s3_class(result, "tbl_df")

  # Expected columns
  expect_named(result, c(
    "TotalReads", "NumOTUs", "MeanReads", "MedianReads", "SDReads",
    "MinReads", "MaxReads", "ShannonDiversity", "SimpsonDiversity",
    "BrayDiversity", "JaccardDiversity"
  ))

  # Returned values are numeric
  expect_true(all(purrr::map_lgl(result, is.numeric)))

  # Expect a singular row
  expect_equal(nrow(result), 1)
})
