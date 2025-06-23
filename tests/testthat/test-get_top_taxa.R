test_that("get_top_taxa returns correct number of taxa", {
  data(GlobalPatterns)

  # Obtain top 10 taxa
  top10 <- get_top_taxa(GlobalPatterns, 10)
  expect_s4_class(top10, "phyloseq")
  expect_equal(ntaxa(top10), 10)

  # Set a tax number greater than present
  n_total <- ntaxa(GlobalPatterns)
  top_all <- get_top_taxa(GlobalPatterns, n_total + 10)
  expect_equal(ntaxa(top_all), n_total)
})

test_that("get_top_taxa errors for invalid top_n", {
  data(GlobalPatterns)

  expect_error(get_top_taxa(GlobalPatterns, -5))
  expect_error(get_top_taxa(GlobalPatterns, 0))
  expect_error(get_top_taxa(GlobalPatterns, c(5, 10)))
  expect_error(get_top_taxa(GlobalPatterns, "10"))
})
