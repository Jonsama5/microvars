# microvars: an analysis package to quickly explore your microbiome data

This repository contains the scripts for the "microvars" package, a homebrew project design to aid in the early exploration of 
microbiome data. Below you can find how to install the package and learn how to use it.

## Instalation

devtools::install_github("Jonsama5/microvars")
library(tuPaquete)

## How to use microvars

* **Data wrangling**
First of all, you'll need some data to analyze. The package has a small dataset with randomly generated abundance, taxonomy
and metadata tables to swiftly introduce you to the package. Let's look at the data:

abundance <- read.csv("abund_tax.csv")
metadata <- read.csv("metadata")

tibble(abundance)
tibble(metadata)

As you can see, We've got a table with the assigned taxonomic classification and the abundance of each OTU as well as the
associated metadata. In this case, the taxonomy and abundance tables are in the same file and we will need to format them
so that the sample designation is in the starting columns while the abundance data is in the left side of th dataframe.
