#' Perform Dunn's post-hoc tests on multiple alpha diversity indices from a phyloseq object
#'
#' This function calculates alpha diversity indices for a phyloseq object
#' and performs Dunn's tests across all grouping variables with ≥3 levels.
#'
#' @param physeq A \code{phyloseq} object.
#' @param measures Character vector of alpha diversity measures to use.
#'                 Default is all available: \code{c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher")}.
#'
#' @return A \code{data.frame} with Dunn's test results: test index, grouping variable, comparison, z-scores, and p-values.
#' @export
#'
#' @examples
#' library(phyloseq)
#' data(GlobalPatterns)
#' dunn_results <- dunn_post_hoc_phy(GlobalPatterns)
#' head(dunn_results)
dunn_post_hoc_phy <- function(physeq, measures = NULL) {
  if (is.null(measures)) {
    measures <- c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher")
  }
  # Calculate alpha diversity and join with metadata
  df_alpha <- phyloseq::estimate_richness(physeq, measures = measures)
  df_alpha <- cbind(df_alpha, data.frame(phyloseq::sample_data(physeq)))
  results <- list()
  # Loop over metadata variables
  for (variable in phyloseq::sample_variables(physeq)) {
    if (length(unique(df_alpha[[variable]])) < 3) next  # Skip if <3 groups
    df_alpha[[variable]] <- as.factor(df_alpha[[variable]])
    for (index in measures) {
      formula <- stats::as.formula(paste(index, "~", variable))
      # Perform Dunn test with BH correction
      dunn_res <- tryCatch({
        FSA::dunnTest(formula, data = df_alpha, method = "bh")
      }, error = function(e) {
        message(paste("Error with variable:", variable, "and index:", index))
        return(NULL)
      })
      if (is.null(dunn_res)) next
      # Format results
      dunn_df <- dunn_res$res
      dunn_df$Variable <- variable
      dunn_df$Index <- index
      colnames(dunn_df) <- c("Comparison", "Z", "P_uncorrected", "P_adjusted", "Variable", "Index")
      results[[paste(variable, index, sep = "_")]] <- dunn_df
    }
  }
  # Combine into one dataframe
  combined <- dplyr::bind_rows(results)
  return(combined)
}
