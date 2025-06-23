#' Generate richness plots (boxplots + jitter) for multiple variables
#'
#' This function creates alpha diversity plots (e.g. Shannon, Simpson) for each variable
#' in the sample_data of a phyloseq object.
#'
#' @param physeq A phyloseq object.
#' @param measures A character vector of alpha diversity indices to plot (e.g. "Shannon", "Simpson").
#'                 Default is all available: c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher").
#'
#' @return A named list of ggplot2 objects (one per variable in sample_data).
#' @export
#'
#' @examples
#' data(GlobalPatterns)
#' plots <- plot_beta_diversity_by_vars(GlobalPatterns, measures = c("Shannon", "Simpson"))
#' print(plots[["SampleType"]])
plot_alpha_diversity_by_vars <- function(physeq, measures = NULL) {
  if (is.null(measures)) {
    measures <- c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher")
  }
  plot_list <- list()
  vars <- colnames(sample_data(physeq))
  for (var in vars) {
    p <- phyloseq::plot_richness(physeq, x = var, measures = measures, title = paste("Alpha diversity by", var)) +
      ggplot2::geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
      ggplot2::geom_boxplot(alpha = 0.6) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.grid = ggplot2::element_line(color = "grey", size = 0.5),
        plot.title = ggplot2::element_text(hjust = 0.5, size = 16, color = "black"),
        axis.text.x = ggplot2::element_text(angle = 90)
      ) +
      ggplot2::ylab("Alpha diversity index")

    plot_list[[var]] <- p
  }
  return(plot_list)
}
