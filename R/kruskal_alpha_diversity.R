#' Perform Kruskal-Wallis tests for multiple alpha diversity indices
#'
#' This function calculates alpha diversity indices for a phyloseq object and
#' performs Kruskal-Wallis tests to compare diversity across groups.
#'
#' @param physeq A \code{phyloseq} object containing your microbiome data.
#' @param group_var Character. The name of the grouping variable in the sample_data of \code{physeq}.
#' @param measures Character vector or NULL. Alpha diversity measures to calculate and test.
#'        Default is all available indices: \code{c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher")}.
#'
#' @return A \code{data.frame} with the Kruskal-Wallis test statistic and p-value for each measure.
#'
#' @export
#'
#' @examples
#' library(phyloseq)
#' data(GlobalPatterns)
#' resuls <- kruskal_alpha_diversity(GlobalPatterns, "SampleType")
#' print(results)
kruskal_alpha_diversity <- function(physeq, group_var, measures = NULL) {
  if (is.null(measures)) {
    measures <- c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher")
  }

  alpha_div <- phyloseq::estimate_richness(physeq, measures = measures)

  sample_df <- data.frame(phyloseq::sample_data(physeq))
  if (!(group_var %in% colnames(sample_df))) {
    stop("group_var is not present in sample_data(physeq)")
  }

  alpha_div[[group_var]] <- sample_df[[group_var]]

  res <- lapply(measures, function(measure) {
    formula <- stats::as.formula(paste(measure, "~", group_var))
    test <- stats::kruskal.test(formula, data = alpha_div)
    data.frame(
      Measure = measure,
      Statistic = test$statistic,
      p_value = test$p.value,
      stringsAsFactors = FALSE
    )
  })

  res_df <-do.call(rbind, res)
  return(res_df)
}
