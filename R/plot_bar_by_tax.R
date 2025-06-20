#' Create composition plots by each taxonomic rank
#'
#' Generates a list of bar plots for each taxonomic rank found in the
#' given \code{phyloseq} object. Be mindful that OTU trimming or filtering
#' might be necessary to visualize the plots adequately.
#'
#' @param physeq A \code{phyloseq} class object
#' @param palette Optional color vector. If NULL or shorter than the number of taxa,
#'               a palette will be generated automatically.
#'
#' @return A list of \code{ggplot} bar plots, one per taxonomic rank
#' @export
#'
#' @importFrom phyloseq rank_names get_taxa_unique plot_bar
#' @importFrom ggplot2 labs scale_fill_manual
#' @importFrom scales hue_pal
#'
#' @examples plot_bar_by_tax(phy)
plot_bar_by_tax <- function(physeq, palette = NULL) {
  plot_bar_list <- list()
  # Iterate over every tax rank
  for (rank in rank_names(physeq)) {
    taxa <- get_taxa_unique(physeq, rank)
    # If no color palette is given, automatically generate one with distinct colors
    # If a personal color vector is used, make sure to have as many colors as species found in the dataset
    if (is.null(palette) || length(palette) < length(taxa)) {
      autopaleta <- hue_pal()(length(taxa))
      names(autopaleta) <- taxa
      pal <- autopaleta
    } else {
      pal <- palette
    }
    # Create and save plots
    plot_bar_list[[rank]] <- plot_bar(physeq, fill = rank) +
      labs(title = paste("Composition by", rank)) +
      scale_fill_manual(values = pal)
  }
  return(plot_bar_list)
}
