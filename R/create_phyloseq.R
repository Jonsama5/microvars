#' Create a phyloseq object from a metdata file and a table containing the abundance and taxonomy data
#'
#' @param table A \code{data.frame} containing the abundance matrix in the first \code{n_sample} columns,
#'              followed by taxonomic classification columns.
#' @param metadata A \code{data.frame} containing sample metadata. It must either:
#'   - include a column named "Sample.ID" with sample identifiers (in which case any row names will be removed automatically), or
#'   - have sample identifiers as row names (in which case they will be converted into a new "Sample.ID" column and row names will be cleared).
#'
#' @return A \code{phyloseq} object combining abundance, taxonomy, and sample metadata.
#' @export
#'
#' @examples create_phyloseq(main_table, metadata, n_sample = 221)
create_phyloseq <- function(table, metadata, n_sample) {
  # Debug
    # If metadata contains rownames and no "Sample.ID" columns is given, rownames are converted into the new "Sample.ID" column.
  if (!"Sample.ID" %in% colnames(metadata) && !is.null(rownames(metadata))) {
    metadata$Sample.ID <- rownames(metadata)
    rownames(metadata) <- NULL
  }
    # If metadata has rownames and "Sample.ID" column, nullify rownames
  if ("Sample.ID" %in% colnames(metadata) && !is.null(rownames(metadata))) {
    rownames(metadata) <- NULL
  }
    # Make sure metadata contains "Sample.ID" column
  if (!"Sample.ID" %in% colnames(metadata)) {
    stop("Metadata must contain a 'Sample.ID' column or row names convertible to 'Sample.ID'.")
  }
  # Verify the select sample number does not exceed total column size of the table
  if (n_sample >= ncol(table)) {
    stop("The number of selected sample columns (n_sample) must be lower than the column size of the table argument.")
  }

  # Format data
  otu_tab <- table[,1:n_sample]
  tax_tab <- table[,(n_sample +1):ncol(table)]
  samples <- metadata
  samples <- samples %>% tibble::column_to_rownames("Sample.ID")
  # Assign unique new names to rows
  new_row_names <- sprintf("otu%03d", seq_len(nrow(otu_tab)))
  rownames(otu_tab) <- new_row_names
  rownames(tax_tab) <- new_row_names
  # Data for phyloseq object
  OTU <- phyloseq::otu_table(otu_tab, taxa_are_rows = TRUE)
  TAX <- phyloseq::tax_table(as.matrix(tax_tab))
  SAMPLES <- phyloseq::sample_data(samples)
  # Create phyloseq class object
  phy <- phyloseq(OTU, TAX, SAMPLES)
  return(phy)
}
