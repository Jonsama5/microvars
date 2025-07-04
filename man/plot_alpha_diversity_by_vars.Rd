% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_alpha_diversity_by_vars.R
\name{plot_alpha_diversity_by_vars}
\alias{plot_alpha_diversity_by_vars}
\title{Generate richness plots (boxplots + jitter) for multiple variables}
\usage{
plot_alpha_diversity_by_vars(physeq, measures = NULL)
}
\arguments{
\item{physeq}{A phyloseq object.}

\item{measures}{A character vector of alpha diversity indices to plot (e.g. "Shannon", "Simpson").
Default is all available: c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher").}
}
\value{
A named list of ggplot2 objects (one per variable in sample_data).
}
\description{
This function creates alpha diversity plots (e.g. Shannon, Simpson) for each variable
in the sample_data of a phyloseq object.
}
\examples{
data(GlobalPatterns)
plots <- plot_richness_by_variables(GlobalPatterns, measures = c("Shannon", "Simpson"))
print(plots[["SampleType"]])
}
