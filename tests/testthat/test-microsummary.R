test_that("microsummary returns correct structure", {
  # Cargar un objeto de ejemplo
  data("GlobalPatterns", package = "phyloseq")
  phy <- GlobalPatterns

  result <- microsummary(phy)

  # Es un tibble
  expect_s3_class(result, "tbl_df")

  # Tiene las columnas esperadas
  expect_named(result, c(
    "TotalReads", "NumOTUs", "MeanReads", "MedianReads", "SDReads",
    "MinReads", "MaxReads", "ShannonDiversity", "SimpsonDiversity",
    "BrayDiversity", "JaccardDiversity"
  ))

  # Los valores devueltos son numéricos
  expect_true(all(purrr::map_lgl(result, is.numeric)))

  # Solo una fila (resumen general)
  expect_equal(nrow(result), 1)
})
