test_that("beta_permanova_by_variables behaves correctly", {
  skip_if_not_installed("phyloseq")
  skip_if_not_installed("vegan")
  skip_if_not_installed("tibble")
  skip_if_not_installed("dplyr")

  library(phyloseq)
  data("GlobalPatterns")
  phy <- GlobalPatterns

  vars_to_test <- c("SampleType", "SampleSubType")

  meta <- as(sample_data(phy), "data.frame")
  valid_vars <- vars_to_test[sapply(vars_to_test, function(v) length(unique(meta[[v]])) > 1)]
  res <- beta_permanova_by_variables(phy, method = "bray", vars = vars_to_test)

  # Comprobar clase tibble
  expect_s3_class(res, "tbl_df")

  # Comprobar columnas esperadas
  expect_named(res, c("Variable", "R2", "PValue", "Permutations"))

  # Comprobar que todas las variables pedidas están en el resultado
  expect_true(all(valid_vars %in% res$Variable))

  # Comprobar que R2 y PValue son numéricos y tienen valores válidos
  expect_true(all(sapply(res$R2, is.numeric)))
  expect_true(all(sapply(res$PValue, is.numeric)))

  # Comprobar que el número de filas es el número de variables
  expect_equal(nrow(res), length(valid_vars))
})



