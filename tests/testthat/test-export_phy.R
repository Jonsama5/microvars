test_that("export_phy crea los archivos correctos", {
  data("GlobalPatterns", package = "phyloseq")
  # Tomamos solo las primeras 10 muestras
  GP_sub <- prune_samples(sample_names(GlobalPatterns)[1:10], GlobalPatterns)

  tmp_prefix <- tempfile("phy")
  export_phy(GP_sub, path_prefix = tmp_prefix)

  # Comprobar que se crean los archivos
  expect_true(file.exists(paste0(tmp_prefix, "_otu.csv")))
  expect_true(file.exists(paste0(tmp_prefix, "_tax.csv")))
  expect_true(file.exists(paste0(tmp_prefix, "_meta.csv")))
})
