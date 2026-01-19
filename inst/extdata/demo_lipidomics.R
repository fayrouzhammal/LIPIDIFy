# Load your package (from dev tools development mode)
devtools::load_all("/Users/hammalfayrouz/Library/CloudStorage/OneDrive-PeterMac/Documents/Working package/V15/LIPIDIFy")

# Path to your MetaboAnalyst file
file_path <- "/Users/hammalfayrouz/Documents/Lipidomics/Lipidomics_Data/Original_data_from_Metabolomics_Australia/Metaboanalyst_MA-AR-658_Lipidomics_Data_Matrix-MA59095.csv"

metadata_cols <- c("Sample Name", "Sample Group")

#Load Data
loaded <- load_lipidomics_data(
  file_path,
  metadata_columns = metadata_cols
)

# Quick sanity check
str(loaded$metadata)
dim(loaded$numeric_data)

#Classify Lipids
lipid_names <- colnames(loaded$numeric_data)
classification <- classify_lipids(lipid_names)

head(classification)
table(classification$LipidGroup)

#Normalise
norm_mat <- apply_normalizations(
  loaded$numeric_data,
  c("TIC", "Log2")
)

# Keep same rownames
rownames(norm_mat) <- rownames(loaded$numeric_data)

normalized <- list(
  data        = cbind(loaded$metadata, norm_mat),
  metadata    = loaded$metadata,
  numeric_data = norm_mat
)

#PCA on normalised
group_col <- if ("Sample Group" %in% names(loaded$metadata)) {
  "Sample Group"
} else if ("Group" %in% names(loaded$metadata)) {
  "Group"
} else {
  names(loaded$metadata)[1]  # fallback
}

pca_res <- perform_pca(
  norm_mat,
  loaded$metadata,
  group_column = group_col
)

p_pca <- create_pca_plot_with_ellipses(
  pca_data          = pca_res$pca_data,
  variance_explained = pca_res$variance_explained,
  ellipse_type      = "none"
)

print(p_pca)

# Save PCA plot
dir.create("demo_output", showWarnings = FALSE)
ggplot2::ggsave("demo_output/PCA_normalized.png", p_pca, width = 8, height = 6, dpi = 300)

#Differenial analysis
groups <- unique(loaded$metadata[[group_col]])
groups
# If there are only 1 group: you can't do DE, skip this part.

contrasts <- create_default_contrasts(groups)
contrasts

diff_res <- perform_differential_analysis(
  data_matrix   = norm_mat,
  metadata      = loaded$metadata,
  group_column  = group_col,
  contrasts_list = contrasts,
  method        = "limma"
)

names(diff_res$results)

# Look at first contrast
first_contrast_name <- names(diff_res$results)[1]
de_table <- diff_res$results[[first_contrast_name]]
head(de_table)
summary(de_table$adj.P.Val < 0.05)

#Volcano plot
groups <- unique(loaded$metadata[[group_col]])
groups
# If there are only 1 group: you can't do DE, skip this part.

contrasts <- create_default_contrasts(groups)
contrasts

diff_res <- perform_differential_analysis(
  data_matrix   = norm_mat,
  metadata      = loaded$metadata,
  group_column  = group_col,
  contrasts_list = contrasts,
  method        = "limma"
)

names(diff_res$results)

# Look at first contrast
first_contrast_name <- names(diff_res$results)[1]
de_table <- diff_res$results[[first_contrast_name]]
head(de_table)
summary(de_table$adj.P.Val < 0.05)

#Enrichment analysis
enrich <- perform_enrichment_analysis(
  results_list       = diff_res$results,
  classification_data = classification,
  min_set_size       = 5,
  max_set_size       = 500,
  custom_sets        = NULL
)

names(enrich)

first_contrast_enrich <- enrich[[first_contrast_name]]
names(first_contrast_enrich)

# Pick one enrichment type with non-empty results
enrich_df <- NULL
for (et in names(first_contrast_enrich)) {
  if (nrow(first_contrast_enrich[[et]]) > 0) {
    enrich_df <- first_contrast_enrich[[et]]
    enrich_type <- et
    break
  }
}

if (!is.null(enrich_df) && nrow(enrich_df) > 0) {
  p_enrich <- create_enrichment_dotplot(
    enrichment_data = enrich_df,
    title           = paste("Enrichment:", first_contrast_name, "-", enrich_type),
    max_pathways    = 15
  )
  
  print(p_enrich)
  ggplot2::ggsave(
    "demo_output/enrichment_first_contrast.png",
    p_enrich,
    width = 8, height = 6, dpi = 300
  )
} else {
  message("No non-empty enrichment results to plot.")
}

