#' Get the top taxa with most reads in your data
#'
#' @param physeq a \code{phyloseq} class object
#' @param top_n Integer. The number of top taxa you want to keep.
#'
#' @return A \code{phyloseq} object pruned to keep only the top taxa by abundance.
#' @export
#'
#' @examples get_top_taxa(phy, 50)
get_top_taxa <- function(physeq, top_n) {
  if (!is.numeric(top_n) || length(top_n) != 1 || top_n <= 0) {
    stop("top_n must be a single positive numeric value.")
  }
  taxa_sums_all <- phyloseq::taxa_sums(physeq)
  n_taxa <- length(taxa_sums_all)
  if (top_n > n_taxa) {
    warning(sprintf("top_n '%s' is greater than the total number of taxa ('%s'). Returning all taxa", top_n, n_taxa))
    top_n <- n_taxa
  }
  top_taxa <- names(sort(taxa_sums_all, decreasing = TRUE)[1:top_n])
  pruned_object <- phyloseq::prune_taxa(top_taxa, physeq)
  return(pruned_object)
}

