#' Calculate PERMANOVA results for variables in phyloseq sample_data
#'
#' @param physeq A phyloseq object.
#' @param method Distance method for beta diversity (default "bray").
#' @param vars Character vector of variable names to test (default all variables in sample_data).
#' @param permutations Number of permutations for adonis2 (default 999).
#'
#' @return A tibble with variable names, R2, p-values and permutation count.
#' @importFrom phyloseq sample_data distance
#' @importFrom vegan adonis2
#' @export
#' @examples
#' data(GlobalPatterns)
#' res <- beta_permanova_by_variables(GlobalPatterns, method = "bray", permutations = 999)
#' print(res)
beta_permanova_by_variables <- function(physeq, method = "bray", vars = NULL, permutations = 999) {
  library(vegan)

  # Calcular matriz de distancias
  dist_mat <- phyloseq::distance(physeq, method = method)
  meta <- as(sample_data(physeq), "data.frame")

  # Selección de variables
  if (is.null(vars)) {
    vars <- names(meta)
  } else {
    vars <- vars[vars %in% names(meta)]  # filtrar las que existen
  }

  results <- lapply(vars, function(var) {
    if (length(unique(meta[[var]])) > 1) {
      # Construir fórmula dinámica
      formula <- as.formula(paste("dist_mat ~", var))

      # Ejecutar PERMANOVA
      adonis_res <- vegan::adonis2(formula, data = meta, permutations = permutations)

      # Extraer resultados
      data.frame(
        Variable = var,
        R2 = adonis_res$R2[1],
        PValue = adonis_res$`Pr(>F)`[1],
        Permutations = permutations
      )
    } else {
      NULL
    }
  })

  results_df <- do.call(rbind, results)
  return(tibble::as_tibble(results_df))
}



