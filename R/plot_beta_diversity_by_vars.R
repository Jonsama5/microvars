#' Generate beta diversity ordination plots by sample variables
#'
#' This function performs ordination on a phyloseq object and generates
#' plots grouped by variables from sample_data, along with PERMANOVA results.
#'
#' @param physeq A phyloseq object.
#' @param method Distance method. Default is "bray". See vegan::vegdist for options.
#' @param ordination_method Ordination method. Default is "PCoA".
#'
#' @return A named list of ggplot2 ordination plots.
#' @export
#'
#' @examples
#' data(GlobalPatterns)
#' beta_plots <- plot_beta_diversity_by_vars(GlobalPatterns)
#' print(beta_plots[["SampleType"]])
plot_beta_diversity_by_vars <- function(physeq, method = "bray", ordination_method = "PCoA") {
  if (!requireNamespace("phyloseq") || !requireNamespace("ggplot2") || !requireNamespace("vegan")) {
    stop("This function requires the packages: phyloseq, ggplot2, vegan")
  }
  # Ordination
  ord <- phyloseq::ordinate(physeq, method = ordination_method, distance = method)
  vars <- colnames(phyloseq::sample_data(physeq))
  plot_list <- list()
  # Distance matrix for PERMANOVA
  dist <- phyloseq::distance(physeq, method = method)
  metadata <- as.data.frame(phyloseq::sample_data(physeq))
  for (var in vars) {
    if (length(unique(metadata[[var]])) < 2) next

    p <- phyloseq::plot_ordination(physeq, ord, color = var) +
      ggplot2::geom_point(size = 2, alpha = 0.7) +
      ggplot2::ggtitle(paste("Beta diversity by", var)) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        axis.text = ggplot2::element_text(size = 10),
        axis.title = ggplot2::element_text(size = 12)
      )
    # PERMANOVA (vegan::adonis2)
    perm_test <- tryCatch({
      vegan::adonis2(dist ~ metadata[[var]], permutations = 999)
    }, error = function(e) NULL)

    if (!is.null(perm_test)) {
      pval <- perm_test$`Pr(>F)`[1]
      p <- p + ggplot2::labs(subtitle = paste0("PERMANOVA p = ", signif(pval, 3)))
    }
    plot_list[[var]] <- p
  }
  return(plot_list)
}
