% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kruskal_alpha_diversity.R
\name{kruskal_alpha_diversity}
\alias{kruskal_alpha_diversity}
\title{Perform Kruskal-Wallis tests for multiple alpha diversity indices}
\usage{
kruskal_alpha_diversity(physeq, group_var, measures = NULL)
}
\arguments{
\item{physeq}{A \code{phyloseq} object containing your microbiome data.}

\item{group_var}{Character. The name of the grouping variable in the sample_data of \code{physeq}.}

\item{measures}{Character vector or NULL. Alpha diversity measures to calculate and test.
Default is all available indices: \code{c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher")}.}
}
\value{
A \code{data.frame} with the Kruskal-Wallis test statistic and p-value for each measure.
}
\description{
This function calculates alpha diversity indices for a phyloseq object and
performs Kruskal-Wallis tests to compare diversity across groups.
}
\examples{
library(phyloseq)
data(GlobalPatterns)
resuls <- kruskal_alpha_diversity(GlobalPatterns, "SampleType")
print(results)
}
