test_that("create_phyloseq creates a valid phyloseq object", {
  data(GlobalPatterns)

  otu_df <- as.data.frame(otu_table(GlobalPatterns))
  if (!taxa_are_rows(GlobalPatterns)) {
    otu_df <- t(otu_df) %>% as.data.frame()
  }

  tax_df <- as.data.frame(tax_table(GlobalPatterns))
  combined_table <- cbind(otu_df, tax_df)

  meta_df <- as.data.frame(sample_data(GlobalPatterns))
  meta_df$Sample.ID <- rownames(meta_df)
  rownames(meta_df) <- NULL

  n_sample <- ncol(otu_df)
  phy <- create_phyloseq(combined_table, meta_df, n_sample)

  expect_s4_class(phy, "phyloseq")

  expect_equal(sample_names(phy), meta_df$Sample.ID)

  expected_names <- sprintf("otu%03d", seq_len(nrow(otu_df)))
  expect_equal(taxa_names(phy), expected_names)
})


