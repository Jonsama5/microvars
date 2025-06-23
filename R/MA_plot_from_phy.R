#' Create a differential abundance plot using the DESeq2 package from a phyloseq object
#'
#' @param physeq A \code{phyloseq} object containing count data, sample metadata, and taxonomy
#' @param condition_var A \code{string} indicating the name of a variable in the sample metadata to compare groups
#' @param tax_level A \code{string} indicating the taxonomic level at which to label the taxa (e.g. "Genus", "Family")
#' @param title A \code{string} to customize the plot title (optional)
#' @param alpha A \code{numeric} value indicating the adjusted p-value threshold for significance (default = 0.05)
#' @param return_data Logical; if \code{TRUE}, returns a \code{list} with the plot, full data, and significant taxa
#'
#' @return A ggplot object showing differentially abundant taxa. If \code{return_data = TRUE}, a list with the plot, data, and significant taxa.
#' @export
#'
#' @examples
#' MA_plot_from_phy(physeq, condition_var = "group", tax_level = "Genus")
MA_plot_from_phy <- function(physeq, condition_var, tax_level = "Genus", title = NULL, alpha = 0.05, return_data = FALSE) {
  # Verify variable is found in the phyloseq metadata
  if (!(condition_var %in% colnames(phyloseq::sample_data(physeq)))) {
    stop(sprintf("Variable '%s' is not present in the sample metadata.", condition_var))
  }
  # Verify the variable is not numeric
  if (!is.factor(phyloseq::sample_data(physeq)[[condition_var]])) {
    warning(sprintf("'%s' is not a factor. Converting to factor.", condition_var))
    phyloseq::sample_data(physeq)[[condition_var]] <- factor(phyloseq::sample_data(physeq)[[condition_var]])
  }
  # Verify the tax_level argument is present exactly as stated
  if (!(tax_level %in% colnames(phyloseq::tax_table(physeq)))) {
    stop(sprintf("The taxonomic level '%s' is not present in the taxonomy table.", tax_level))
  }
  # Customizable title
  if (is.null(title)) {
    title <- paste("Differential abundance by", condition_var)
  }
  # DESeq2
  dds <- phyloseq::phyloseq_to_deseq2(physeq, stats::as.formula(paste("~", condition_var)))
  dds <- DESeq2::DESeq(dds, sfType = "poscounts") # This sdType argument is needed when using Paul McMurdie's phyloseq package for metagenomic samples
  res <- DESeq2::results(dds, alpha = alpha)
  # Data for MA plot
  ma_data <- data.frame(
    baseMean = res$baseMean,
    log2FoldChange = res$log2FoldChange,
    padj = res$padj,
    OTU = rownames(res)
  )
  # Taxonomy labels
  tax <- as.data.frame(phyloseq::tax_table(physeq))
  ma_data$Label <- tax[match(ma_data$OTU, rownames(tax)), tax_level]
  # Filter significant data
  label_data <- subset(ma_data, padj < alpha)
  label_data$Label <- ifelse(is.na(label_data$Label), label_data$OTU, label_data$Label)
  ma_data <- subset(ma_data, is.finite(baseMean) & is.finite(log2FoldChange) & !is.na(padj))
  # Generate MA plot
  plot <- ggplot2::ggplot(ma_data, ggplot2::aes(x = log10(baseMean + 1), y = log2FoldChange)) +
    ggplot2::geom_point(ggplot2::aes(color = padj < alpha), size = 3, alpha = 0.7) +
    ggplot2::scale_color_manual(values = c("FALSE" = "gray70", "TRUE" = "red")) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
    ggplot2::geom_hline(yintercept = c(-1, 1), linetype = "dashed", color = "darkgray") +
    ggrepel::geom_text_repel(data = label_data, ggplot2::aes(label = Label), size = 4, max.overlaps = 20) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0, size = 17, face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 70, vjust = 1, hjust = 0.5, size = 9),
      axis.text.y = ggplot2::element_text(size = 8),
      axis.title.y.left = ggplot2::element_text(size = 13, face = "bold"),
      legend.title = ggplot2::element_text(size = 13, face = "bold"),
      legend.text = ggplot2::element_text(size = 14),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey", size = 0.5)
    ) +
    ggplot2::labs(
      title = title,
      x = "Log10(Mean abundance + 1)",
      y = paste("Change in Log2 (by", condition_var, ")"),
      color = paste("Significant (padj <", alpha, ")")
    )
  if (return_data) {
    return(list(plot = plot, data = ma_data, significant = label_data))
  }
  return(plot)
}
