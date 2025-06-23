test_that("MA_plot_from_phy works with default arguments", {
  data("GlobalPatterns")
  physeq <- prune_taxa(taxa_names(GlobalPatterns)[1:100], GlobalPatterns)
  sample_data(physeq)$test_group <- factor(rep(c("A", "B"), length.out = nsamples(physeq)))

  # Filtrado para evitar errores por ceros
  physeq <- prune_taxa(taxa_sums(physeq) > 0, physeq)
  otu_counts <- otu_table(physeq)
  keep_taxa <- apply(otu_counts, 1, function(x) sum(x == 0) < 0.9 * length(x))
  physeq <- prune_taxa(keep_taxa, physeq)

  expect_s3_class(MA_plot_from_phy(physeq, "test_group"), "gg")
})

test_that("Return data works", {
  data("GlobalPatterns")
  physeq <- prune_taxa(taxa_names(GlobalPatterns)[1:100], GlobalPatterns)
  sample_data(physeq)$test_group <- factor(rep(c("A", "B"), length.out = nsamples(physeq)))

  # Filtrado para evitar errores por ceros
  physeq <- prune_taxa(taxa_sums(physeq) > 0, physeq)
  otu_counts <- otu_table(physeq)
  keep_taxa <- apply(otu_counts, 1, function(x) sum(x == 0) < 0.9 * length(x))
  physeq <- prune_taxa(keep_taxa, physeq)

  res <- MA_plot_from_phy(physeq, "test_group", return_data = TRUE)
  expect_type(res, "list")
  expect_named(res, c("plot", "data", "significant"))
})

test_that("Errors for wrong input", {
  data("GlobalPatterns")
  physeq <- prune_taxa(taxa_names(GlobalPatterns)[1:100], GlobalPatterns)

  # Filtrado para evitar errores por ceros (opcional aquí, pero mejor mantener coherencia)
  physeq <- prune_taxa(taxa_sums(physeq) > 0, physeq)
  otu_counts <- otu_table(physeq)
  keep_taxa <- apply(otu_counts, 1, function(x) sum(x == 0) < 0.9 * length(x))
  physeq <- prune_taxa(keep_taxa, physeq)

  expect_error(MA_plot_from_phy(physeq, "nonexistent_var"))
  expect_error(MA_plot_from_phy(physeq, "SampleType", tax_level = "Nonexistent"))
})

