#' Obtain data about the phyloseq dataset
#'
#' @param x A phyloseq class Object.
#'
#' @return A tibble containing the total number of reads, OTUs, descriptive statistics of reads, alpha and beta diversity indexes.
#' @export
#' @importFrom tibble tibble
#' @importFrom phyloseq sample_sums otu_table estimate_richness distance
#'
#' @examples
#' microsummary(phy)
microsummary <- function(x) {
  # Total read number across all samples
  total_reads <- sum(phyloseq::sample_sums(x))

  # Number of detected OTUs across all samples.
  n_otu <- nrow(phyloseq::otu_table(x))

  # Descriptive statistics
  mean_reads <- mean(phyloseq::sample_sums(x))
  median_reads <- median(phyloseq::sample_sums(x))
  devs <- sd(phyloseq::sample_sums(x))
  min_reads <- min(phyloseq::sample_sums(x))
  max_reads <- max(phyloseq::sample_sums(x))

  # Diversity indexes
  diversity <- phyloseq::estimate_richness(x, measures = c("Shannon", "Simpson"))
  beta_b <- phyloseq::distance(x, method = "bray", weighted = TRUE)
  beta_j <- phyloseq::distance(x, method = "jaccard", weighted = TRUE)

  # Tibble creation
  stats_tibble <- tibble::tibble(
    TotalReads = total_reads,
    NumOTUs = n_otu,
    MeanReads = mean_reads,
    MedianReads = median_reads,
    SDReads = devs,
    MinReads = min_reads,
    MaxReads = max_reads,
    ShannonDiversity = mean(diversity$Shannon),
    SimpsonDiversity = mean(diversity$Simpson),
    BrayDiversity = mean(beta_b),
    JaccardDiversity = mean(beta_j)
  )
  return(stats_tibble)
}
