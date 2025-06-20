#' Create composition plots by each sample variable
#'
#' Generates a list of bar plots grouped by each variable (e.g., sample metadata)
#' found in the given \code{phyloseq} object. Note that this function assumes
#' the taxonomic rank "Genus" exists. Plots might not be meaningful for continuous
#' variables or those with many levels.
#'
#' @param physeq A \code{phyloseq} class object
#' @param rank Character. Taxonomic rank to use for bar plot fill (default is "Genus")
#' @param palette Optional color vector. If NULL or shorter than the number of taxa,
#'               a palette will be generated automatically.
#'
#' @return A list of \code{ggplot} bar plots, one per sample variable
#' @export
#'
#' @importFrom phyloseq sample_variables plot_bar
#' @importFrom ggplot2 facet_wrap theme element_text ggtitle scale_fill_manual
#'
#' @examples plot_bar_by_vars(phy)
#' Create composition plots by each sample variable
#'
#' Generates a list of bar plots grouped by each variable (e.g., sample metadata)
#' found in the given \code{phyloseq} object. The user can specify the taxonomic rank
#' used for coloring the bars (e.g., "Phylum", "Genus"). Plots might not be meaningful
#' for continuous variables or those with many levels.
#'
#' @param physeq A \code{phyloseq} class object
#' @param rank Character. Taxonomic rank to use for bar plot fill (default is "Genus")
#' @param palette Optional color vector. If NULL or shorter than the number of taxa,
#'               a palette will be generated automatically.
#'
#' @return A list of \code{ggplot} bar plots, one per sample variable
#' @export
#'
#' @importFrom phyloseq sample_variables plot_bar
#' @importFrom ggplot2 facet_wrap theme element_text ggtitle scale_fill_manual
#'
#' @examples plot_bar_by_vars(phy, rank = "Phylum")
plot_bar_by_vars <- function(physeq, rank = "Genus", palette = NULL) {
  plot_bar_list <- list()
  #Make sure the rank exists within the given phyloseq object
  if (!rank %in% rank_names(physeq)) {
    stop(paste("El rango taxonómico '", rank, "' no existe en el objeto phyloseq."), call. = FALSE)
  }
  for (var in sample_variables(physeq)) {
    plot_bar_list[[var]] <- plot_bar(physeq, fill = rank) +
      facet_wrap(as.formula(paste("~", var)), scales = "free_x") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggtitle(paste0("Composition by ", rank, " grouped by ", var)) +
      scale_fill_manual(values = palette)
  }
  return(plot_bar_list)
}
