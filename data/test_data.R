setwd("C:/Users/jonco/Desktop/Bioinformática/R/Packages/microvars/data/")

# Random data

  # Abundance and taxonomy table

abund_tax <- data.frame(
  OTU_ID   = c("OTU_1", "OTU_2", "OTU_3", "OTU_4", "OTU_5"),
  Kingdom  = c("Bacteria", "Bacteria", "Bacteria", "Fungi", "Bacteria"),
  Phylum   = c("Proteobacteria", "Firmicutes", "Actinobacteria", "Ascomycota", "Bacteroidetes"),
  Class    = c("Gammaproteobacteria", "Bacilli", "Actinobacteria", "Saccharomycetes", "Bacteroidia"),
  Order    = c("Enterobacterales", "Lactobacillales", "Bifidobacteriales", "Saccharomycetales", "Bacteroidales"),
  Family   = c("Enterobacteriaceae", "Lactobacillaceae", "Bifidobacteriaceae", "Saccharomycetaceae", "Bacteroidaceae"),
  Genus    = c("Escherichia", "Lactobacillus", "Bifidobacterium", "Saccharomyces", "Bacteroides"),
  Species  = c("E. coli", "L. reuteri", "B. longum", "S. cerevisiae", "B. fragilis"),
  Sample1  = c(150, 80, 20, 10, 300),
  Sample2  = c(230, 100, 0, 40, 200),
  Sample3  = c(50, 200, 15, 25, 100),
  Sample4  = c(120, 300, 5, 0, 150),
  stringsAsFactors = FALSE
)

  # Metadata table

metadata <- data.frame(
  SampleID  = paste0("Sample", 1:6),
  Host_Sex  = c("Female", "Male", "Female", "Male", "Female", "Male"),
  Condition = c("Control", "Control", "Treated", "Treated", "Control", "Treated"),
  Treatment = c("None", "None", "Antibiotic", "Insecticide", "None", "Antibiotic"),
  Location  = c("North", "Central", "South", "West", "East", "Southwest"),
  stringsAsFactors = FALSE
)

# Export
write.csv(abund_tax, file = "abund_tax.csv", row.names = FALSE)
write.csv(metadata, file = "metadata.csv", row.names = FALSE)
