#' Export your phyloseq type object as a comma separated value file.
#'
#' @param phy A phyloseq object.
#' @param path_prefix The path the file will be saved to.
#'
#' @return A comma separated value file in the specified path
#' @export
#'
#' @examples
#' data <- export_phy(GlobalPatterns, path_prefix = "your_path")
export_phy <- function(phy, path_prefix = "output") {
  write.csv(as.data.frame(otu_table(phy)), paste0(path_prefix, "_otu.csv"))
  write.csv(as.data.frame(tax_table(phy)), paste0(path_prefix, "_tax.csv"))
  write.csv(as.data.frame(sample_data(phy)), paste0(path_prefix, "_meta.csv"))
}
