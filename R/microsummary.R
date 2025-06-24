#' Obtain data about the phyloseq dataset and plot sequencing depth
#'
#' @param x A phyloseq-class object.
#' @param alpha_measures Character vector of alpha diversity indices to compute.
#' @param beta_measures Character vector of beta diversity distance metrics to compute.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{summary}: A tibble with sequencing and diversity statistics.
#'   \item \code{plot}: A ggplot2 object showing the distribution of reads per sample.
#' }
#' @export
#' @importFrom tibble tibble
#' @importFrom phyloseq sample_sums otu_table estimate_richness distance
#' @importFrom ggplot2 ggplot aes geom_histogram labs theme_minimal
#' @examples
#' data(GlobalPatterns)
#' res <- microsummary(GlobalPatterns)
#' res$summary
#' res$plot
microsummary <- function(x,
                         alpha_measures = c("Shannon", "Simpson"),
                         beta_measures = c("bray", "jaccard")) {
  if (!inherits(x, "phyloseq")) {
    stop("Input must be a phyloseq object.")
  }

  sample_reads <- phyloseq::sample_sums(x)

  total_reads <- sum(sample_reads)
  n_otu <- nrow(phyloseq::otu_table(x))

  mean_reads <- mean(sample_reads)
  median_reads <- median(sample_reads)
  sd_reads <- sd(sample_reads)
  min_reads <- min(sample_reads)
  max_reads <- max(sample_reads)

  diversity <- phyloseq::estimate_richness(x, measures = alpha_measures)

  alpha_summary <- lapply(alpha_measures, function(measure) {
    mean(diversity[[measure]], na.rm = TRUE)
  })
  names(alpha_summary) <- paste0("Mean_", alpha_measures)

  beta_summary <- lapply(beta_measures, function(method) {
    d <- try(phyloseq::distance(x, method = method, weighted = TRUE), silent = TRUE)
    if (inherits(d, "try-error")) NA else mean(d)
  })
  names(beta_summary) <- paste0("Mean_", beta_measures, "_distance")

  stats_tibble <- tibble::tibble(
    TotalReads = total_reads,
    NumOTUs = n_otu,
    MeanReads = mean_reads,
    MedianReads = median_reads,
    SDReads = sd_reads,
    MinReads = min_reads,
    MaxReads = max_reads,
    !!!alpha_summary,
    !!!beta_summary
  )

  # --- Visualization: histogram of reads per sample ---
  reads_df <- tibble::tibble(Sample = names(sample_reads),
                             Reads = as.numeric(sample_reads))

  read_plot <- ggplot2::ggplot(reads_df, ggplot2::aes(x = Reads)) +
    ggplot2::geom_histogram(bins = 30, fill = "#2C3E50", color = "white") +
    ggplot2::labs(title = "Distribution of sequencing depth per sample",
                  x = "Reads per sample",
                  y = "Number of samples") +
    ggplot2::theme_minimal()

  return(list(
    summary = stats_tibble,
    plot = read_plot
  ))
}

