#' Visualize Raw Data
#'
#' @param data_list List containing data components from load_lipidomics_data
#' @param plot_type Type of plot ("boxplot", "density", "histogram")
#' @return ggplot object
#' @export
visualize_raw_data <- function(data_list, plot_type = "boxplot") {
  
  numeric_data <- data_list$numeric_data
  
  # Convert matrix to data frame for pivot_longer
  if (is.matrix(numeric_data)) {
    numeric_data <- as.data.frame(numeric_data, check.names = FALSE)
  }
  
  if (plot_type == "boxplot") {
    data_long <- tidyr::pivot_longer(numeric_data, dplyr::everything(), names_to = "Sample", values_to = "Intensity")
    
    p <- ggplot2::ggplot(data_long, ggplot2::aes(x = Sample, y = Intensity)) +
      ggplot2::geom_boxplot() +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::labs(title = "Raw Data Distribution", y = "Intensity")
    
  } else if (plot_type == "density") {
    data_long <- tidyr::pivot_longer(numeric_data, dplyr::everything(), names_to = "Sample", values_to = "Intensity")
    
    p <- ggplot2::ggplot(data_long, ggplot2::aes(x = Intensity, color = Sample)) +
      ggplot2::geom_density(alpha = 0.7) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::labs(title = "Raw Data Density Distribution", x = "Intensity")
    
  } else if (plot_type == "histogram") {
    data_long <- tidyr::pivot_longer(numeric_data, dplyr::everything(), names_to = "Sample", values_to = "Intensity")
    
    p <- ggplot2::ggplot(data_long, ggplot2::aes(x = Intensity)) +
      ggplot2::geom_histogram(bins = 50, alpha = 0.7) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Raw Data Histogram", x = "Intensity", y = "Count")
  }
  
  return(p)
}

#' Visualize Raw Data with Sample/Lipid Toggle
#'
#' @param data_list List containing data components; must include $numeric_data (samples x lipids)
#' @param plot_type Type of plot ("boxplot", "violin", "density", "histogram")
#' @param view_mode Either "sample" or "lipid"
#' @param top_n Number of top variable lipids to show (for lipid mode)
#' @return ggplot object
#' @export
visualize_raw_data_improved <- function(data_list,
                                        plot_type = "boxplot",
                                        view_mode = "sample",
                                        top_n = 30) {
  
  numeric_data <- data_list$numeric_data
  if (is.null(numeric_data)) {
    return(ggplot2::ggplot() + ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No numeric_data found") + ggplot2::theme_void())
  }
  
  # Coerce to data.frame with a Sample column based on rownames
  df <- as.data.frame(numeric_data, check.names = FALSE)
  if (is.null(rownames(df))) {
    rownames(df) <- paste0("S", seq_len(nrow(df)))
  }
  df$Sample <- rownames(df)
  
  # Helper to pivot long safely
  pivot_long_safe <- function(d, id) {
    tidyr::pivot_longer(d, cols = -tidyr::all_of(id), names_to = "Lipid", values_to = "Intensity")
  }
  
  # Validate plot_type
  plot_type <- match.arg(plot_type, c("boxplot", "violin", "density", "histogram"))
  
  if (identical(view_mode, "sample")) {
    long <- pivot_long_safe(df, "Sample")
    
    if (plot_type == "boxplot") {
      p <- ggplot2::ggplot(long, ggplot2::aes(x = Sample, y = Intensity)) +
        ggplot2::geom_boxplot(outlier.alpha = 0.3) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 8)) +
        ggplot2::labs(title = "Sample-wise Distribution", x = "Sample", y = "Intensity")
      
    } else if (plot_type == "violin") {
      p <- ggplot2::ggplot(long, ggplot2::aes(x = Sample, y = Intensity)) +
        ggplot2::geom_violin(trim = FALSE) +
        ggplot2::geom_boxplot(width = 0.12, outlier.alpha = 0.2) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 8)) +
        ggplot2::labs(title = "Sample-wise Distribution (Violin)", x = "Sample", y = "Intensity")
      
    } else if (plot_type == "density") {
      p <- ggplot2::ggplot(long, ggplot2::aes(x = Intensity, color = Sample)) +
        ggplot2::geom_density(alpha = 0.7) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::labs(title = "Sample-wise Density", x = "Intensity", y = "Density")
      
    } else {
      p <- ggplot2::ggplot(long, ggplot2::aes(x = Intensity)) +
        ggplot2::geom_histogram(bins = 50, alpha = 0.7, fill = "steelblue") +
        ggplot2::theme_minimal() +
        ggplot2::labs(title = "Sample-wise Histogram", x = "Intensity", y = "Count")
    }
    
  } else if (identical(view_mode, "lipid")) {
    if (is.null(top_n)) top_n <- 30
    vars <- apply(df[, setdiff(colnames(df), "Sample"), drop = FALSE], 2, stats::var, na.rm = TRUE)
    vars[is.na(vars)] <- 0
    ordered <- names(sort(vars, decreasing = TRUE))
    if (length(ordered) == 0) {
      return(ggplot2::ggplot() + ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No lipid columns found") + ggplot2::theme_void())
    }
    top_lipids <- utils::head(ordered, n = min(top_n, length(ordered)))
    
    df_sub <- df[, c("Sample", top_lipids), drop = FALSE]
    long <- pivot_long_safe(df_sub, "Sample")
    
    if (plot_type == "boxplot") {
      p <- ggplot2::ggplot(long, ggplot2::aes(x = Lipid, y = Intensity)) +
        ggplot2::geom_boxplot(outlier.alpha = 0.3) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 6)) +
        ggplot2::labs(title = paste("Top", length(top_lipids), "Variable Lipids"), x = "Lipid", y = "Intensity")
      
    } else if (plot_type == "violin") {
      p <- ggplot2::ggplot(long, ggplot2::aes(x = Lipid, y = Intensity)) +
        ggplot2::geom_violin(trim = FALSE) +
        ggplot2::geom_boxplot(width = 0.12, outlier.alpha = 0.2) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 6)) +
        ggplot2::labs(title = paste("Top", length(top_lipids), "Variable Lipids (Violin)"), x = "Lipid", y = "Intensity")
      
    } else if (plot_type == "density") {
      p <- ggplot2::ggplot(long, ggplot2::aes(x = Intensity, color = Lipid)) +
        ggplot2::geom_density(alpha = 0.7) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::labs(title = paste("Density - Top", length(top_lipids), "Variable Lipids"),
                      x = "Intensity", y = "Density")
      
    } else {
      p <- ggplot2::ggplot(long, ggplot2::aes(x = Intensity)) +
        ggplot2::geom_histogram(bins = 50, alpha = 0.7, fill = "darkgreen") +
        ggplot2::theme_minimal() +
        ggplot2::labs(title = paste("Histogram - Top", length(top_lipids), "Variable Lipids"),
                      x = "Intensity", y = "Count")
    }
    
  } else {
    p <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = paste("Unknown view mode:", view_mode)) +
      ggplot2::theme_void()
  }
  
  return(p)
}

#' Create a quick QC plot for a normalization pipeline
#'
#' @param data_matrix Numeric matrix or data.frame with samples in rows and lipids in columns.
#' @param title Title to display above the plot.
#'
#' @return A ggplot2 object (boxplots of intensities per sample).
#' @export
create_pipeline_plot <- function(data_matrix, title = "Normalization pipeline") {
  
  # Convert matrix/data.frame to a data frame with proper sample names
  if (is.matrix(data_matrix)) {
    data_df <- as.data.frame(data_matrix, check.names = FALSE)
  } else {
    data_df <- as.data.frame(data_matrix, check.names = FALSE)
  }
  
  # Ensure rownames exist
  if (is.null(rownames(data_df)) ||
      all(rownames(data_df) == as.character(seq_len(nrow(data_df))))) {
    rownames(data_df) <- paste0("Sample_", seq_len(nrow(data_df)))
  }
  
  # Add Sample column from rownames
  data_df$Sample <- rownames(data_df)
  
  # Pivot to long format (exclude Sample column from pivoting)
  data_long <- tidyr::pivot_longer(
    data_df,
    cols = -Sample,
    names_to = "Lipid",
    values_to = "Intensity"
  )
  
  ggplot2::ggplot(data_long, ggplot2::aes(x = Sample, y = Intensity)) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 6)
    ) +
    ggplot2::labs(
      title = title,
      x = "Sample",
      y = "Intensity"
    )
}


#' Create Heatmap with Sample Matching
#'
#' @param data_matrix Data matrix for heatmap (features as rows, samples as columns)
#' @param metadata Metadata data frame (samples as rows)
#' @param group_column Group column name
#' @param top_n Number of top features to include
#' @param classification_data Optional classification data for row annotation
#' @param title Plot title
#' @return pheatmap object or ggplot object as fallback
#' @export
create_heatmap_robust <- function(data_matrix, metadata, group_column = "Sample Group",
                                  top_n = 50, classification_data = NULL, title = "Heatmap") {
  
  tryCatch({
    # Input validation
    if (is.null(data_matrix) || nrow(data_matrix) == 0 || ncol(data_matrix) == 0) {
      stop("Data matrix is empty or NULL")
    }
    
    if (is.null(metadata) || nrow(metadata) == 0) {
      stop("Metadata is empty or NULL")
    }
    
    # Ensure correct orientation (features as rows, samples as columns)
    if (nrow(data_matrix) < ncol(data_matrix)) {
      data_matrix <- t(data_matrix)
    }
    
    # Ensure data_matrix is a matrix
    if (!is.matrix(data_matrix)) {
      data_matrix <- as.matrix(data_matrix)
    }
    
    # Get sample names from metadata
    if ("Sample Name" %in% colnames(metadata)) {
      sample_names <- metadata$`Sample Name`
    } else if (!is.null(rownames(metadata))) {
      sample_names <- rownames(metadata)
    } else {
      sample_names <- paste0("Sample_", 1:nrow(metadata))
    }
    
    # Match samples between data and metadata
    common_samples <- intersect(colnames(data_matrix), sample_names)
    
    if (length(common_samples) == 0) {
      # Try matching by position
      n_samples <- min(ncol(data_matrix), nrow(metadata))
      data_matrix <- data_matrix[, 1:n_samples, drop = FALSE]
      metadata <- metadata[1:n_samples, , drop = FALSE]
      colnames(data_matrix) <- sample_names[1:n_samples]
    } else {
      data_matrix <- data_matrix[, common_samples, drop = FALSE]
      metadata <- metadata[sample_names %in% common_samples, , drop = FALSE]
    }
    
    # Select top variable features
    if (nrow(data_matrix) > top_n) {
      feature_vars <- apply(data_matrix, 1, stats::var, na.rm = TRUE)
      top_features <- names(sort(feature_vars, decreasing = TRUE)[1:top_n])
      data_matrix <- data_matrix[top_features, , drop = FALSE]
    }
    
    # Scale data
    scaled_data <- t(scale(t(data_matrix)))
    scaled_data[is.na(scaled_data)] <- 0
    
    # Create annotation
    if (group_column %in% colnames(metadata)) {
      annotation_col <- data.frame(
        Group = metadata[[group_column]],
        row.names = colnames(data_matrix)
      )
    } else {
      annotation_col <- NA
    }
    
    # Create heatmap
    heatmap <- pheatmap::pheatmap(
      scaled_data,
      annotation_col = annotation_col,
      main = title,
      cluster_rows = TRUE,
      cluster_cols = TRUE,
      show_rownames = nrow(scaled_data) <= 50,
      show_colnames = TRUE,
      fontsize_row = 8,
      fontsize_col = 8,
      silent = TRUE
    )
    
    return(heatmap)
    
  }, error = function(e) {
    # Return error plot
    error_plot <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, 
                        label = paste("Heatmap failed:\n", e$message), 
                        size = 4) +
      ggplot2::theme_void() +
      ggplot2::labs(title = "Heatmap Error")
    
    return(error_plot)
  })
}

#' Fix Sample Alignment between data and metadata
#'
#' @param data_matrix Data matrix (features as rows, samples as columns)
#' @param metadata Metadata data frame
#' @return List with aligned data_matrix and metadata
#' @export
fix_sample_alignment <- function(data_matrix, metadata) {
  
  # Get sample identifiers
  data_samples <- colnames(data_matrix)
  
  if ("Sample Name" %in% colnames(metadata)) {
    metadata_samples <- metadata$`Sample Name`
  } else if (!is.null(rownames(metadata))) {
    metadata_samples <- rownames(metadata)
  } else {
    metadata_samples <- paste0("Sample_", 1:nrow(metadata))
  }
  
  # Find common samples
  common_samples <- intersect(data_samples, metadata_samples)
  
  if (length(common_samples) > 0) {
    data_matrix <- data_matrix[, common_samples, drop = FALSE]
    
    if ("Sample Name" %in% colnames(metadata)) {
      metadata <- metadata[metadata$`Sample Name` %in% common_samples, , drop = FALSE]
      rownames(metadata) <- metadata$`Sample Name`
    } else {
      metadata <- metadata[rownames(metadata) %in% common_samples, , drop = FALSE]
    }
    
    metadata <- metadata[colnames(data_matrix), , drop = FALSE]
  } else {
    n_samples <- min(ncol(data_matrix), nrow(metadata))
    data_matrix <- data_matrix[, 1:n_samples, drop = FALSE]
    metadata <- metadata[1:n_samples, , drop = FALSE]
  }
  
  return(list(
    data_matrix = data_matrix,
    metadata = metadata
  ))
}

#' Create Volcano Plot with Labels - FIXED: Only color SIGNIFICANT lipids by classification
#'
#' @param results Differential analysis results data frame
#' @param title Plot title
#' @param logfc_threshold Log fold change threshold for significance
#' @param pval_threshold P-value threshold for significance
#' @param top_labels Number of top features to label
#' @param classification_data Optional classification data for coloring
#' @param color_by Column to use for coloring (if classification_data provided)
#' @return ggplot object
#' @export
create_volcano_plot_labeled <- function(results, title = "Volcano Plot",
                                        logfc_threshold = 1, pval_threshold = 0.05,
                                        top_labels = 15, classification_data = NULL,
                                        color_by = NULL) {
  
  # Ensure results is a data frame
  if (!is.data.frame(results)) {
    results <- as.data.frame(results)
  }
  
  # Add lipid names if not present
  if (!"Lipid" %in% colnames(results)) {
    results$Lipid <- rownames(results)
  }
  
  # FIXED: Create significance categories with correct terminology
  results$Significance <- "Not Significant"
  results$Significance[results$adj.P.Val < pval_threshold & results$logFC > logfc_threshold] <- "Increased"
  results$Significance[results$adj.P.Val < pval_threshold & results$logFC < -logfc_threshold] <- "Decreased"
  
  # Determine if lipid is significant (for classification coloring)
  results$IsSignificant <- results$Significance != "Not Significant"
  
  # Add -log10 p-value
  results$neg_log10_pval <- -log10(results$adj.P.Val)
  
  # Select top features to label
  results <- results[order(results$adj.P.Val), ]
  top_features <- utils::head(results$Lipid[results$Significance != "Not Significant"], top_labels)
  results$Label <- ifelse(results$Lipid %in% top_features, results$Lipid, "")
  
  # FIXED: Create plot with classification coloring ONLY for significant lipids
  if (!is.null(classification_data) && !is.null(color_by)) {
    # Merge with classification
    results <- merge(results, classification_data, by = "Lipid", all.x = TRUE)
    
    # Create a combined color column: classification for significant, "Not Significant" for others
    results$ColorGroup <- ifelse(
      results$IsSignificant,
      as.character(results[[color_by]]),
      "Not Significant"
    )
    results$ColorGroup[is.na(results$ColorGroup)] <- "Unknown"
    
    # Get unique classification values (excluding "Not Significant")
    unique_classes <- unique(results$ColorGroup[results$ColorGroup != "Not Significant"])
    unique_classes <- unique_classes[!is.na(unique_classes)]
    
    # Create color palette
    n_colors <- length(unique_classes)
    if (n_colors > 0) {
      class_colors <- scales::hue_pal()(n_colors)
      names(class_colors) <- unique_classes
    } else {
      class_colors <- c()
    }
    
    # Add gray for non-significant
    all_colors <- c(class_colors, "Not Significant" = "grey70")
    
    p <- ggplot2::ggplot(results, ggplot2::aes(x = logFC, y = neg_log10_pval, color = ColorGroup)) +
      ggplot2::geom_point(alpha = 0.6, size = 2) +
      ggplot2::scale_color_manual(
        values = all_colors,
        name = color_by,
        breaks = c(unique_classes, "Not Significant")  # Order legend
      )
    
  } else {
    # Standard coloring by significance - FIXED terminology
    p <- ggplot2::ggplot(results, ggplot2::aes(x = logFC, y = neg_log10_pval, color = Significance)) +
      ggplot2::geom_point(alpha = 0.6, size = 2) +
      ggplot2::scale_color_manual(
        values = c("Increased" = "red", "Decreased" = "blue", "Not Significant" = "grey50"),
        name = "Abundance Change"
      )
  }
  
  # Add labels and thresholds
  p <- p +
    ggplot2::geom_vline(xintercept = c(-logfc_threshold, logfc_threshold), 
                        linetype = "dashed", color = "grey40") +
    ggplot2::geom_hline(yintercept = -log10(pval_threshold), 
                        linetype = "dashed", color = "grey40") +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = Label),
      max.overlaps = 20,
      size = 3,
      box.padding = 0.5,
      show.legend = FALSE
    ) +
    ggplot2::labs(
      title = title,
      x = "Log2 Fold Change",
      y = "-Log10 Adjusted P-value"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    )
  
  return(p)
}

#' Create Lipid Expression Barplot (FIXED VERSION)
#'
#' @param data_matrix Data matrix (samples as rows, lipids as columns)
#' @param metadata Metadata data frame
#' @param selected_lipids Vector of lipid names to plot
#' @param selected_samples Vector of sample names to include (optional)
#' @param selected_groups Vector of group names to include (optional)
#' @param group_column Name of the group column in metadata
#' @param data_type Label for data type ("raw" or "normalized")
#' @return ggplot object or list of ggplot objects
#' @export
create_lipid_expression_barplot <- function(data_matrix, metadata, 
                                            selected_lipids, 
                                            selected_samples = NULL,
                                            selected_groups = NULL,
                                            group_column = "Sample Group",
                                            data_type = "normalized") {
  
  # Convert to matrix if needed and ensure correct orientation
  if (!is.matrix(data_matrix)) {
    data_matrix <- as.matrix(data_matrix)
  }
  
  # Ensure data is in correct format (samples as rows)
  if (ncol(data_matrix) < nrow(data_matrix)) {
    data_matrix <- t(data_matrix)
  }
  
  # Ensure metadata has proper row alignment
  if (is.null(rownames(data_matrix))) {
    rownames(data_matrix) <- paste0("Sample_", seq_len(nrow(data_matrix)))
  }
  
  # Get sample names from metadata
  if ("Sample Name" %in% colnames(metadata)) {
    metadata_sample_names <- metadata$`Sample Name`
  } else {
    metadata_sample_names <- rownames(metadata)
    if (is.null(metadata_sample_names)) {
      metadata_sample_names <- paste0("Sample_", seq_len(nrow(metadata)))
    }
  }
  
  # Filter by groups if specified
  if (!is.null(selected_groups) && length(selected_groups) > 0) {
    if (group_column %in% colnames(metadata)) {
      group_mask <- metadata[[group_column]] %in% selected_groups
      metadata <- metadata[group_mask, , drop = FALSE]
      metadata_sample_names <- metadata_sample_names[group_mask]
    }
  }
  
  # Filter by samples if specified
  if (!is.null(selected_samples) && length(selected_samples) > 0) {
    sample_mask <- metadata_sample_names %in% selected_samples
    metadata <- metadata[sample_mask, , drop = FALSE]
    metadata_sample_names <- metadata_sample_names[sample_mask]
  }
  
  # Match data matrix rows to metadata
  data_sample_names <- rownames(data_matrix)
  common_samples <- intersect(data_sample_names, metadata_sample_names)
  
  if (length(common_samples) > 0) {
    data_matrix <- data_matrix[common_samples, , drop = FALSE]
    if ("Sample Name" %in% colnames(metadata)) {
      metadata <- metadata[metadata$`Sample Name` %in% common_samples, , drop = FALSE]
      metadata <- metadata[match(common_samples, metadata$`Sample Name`), , drop = FALSE]
    } else {
      metadata <- metadata[rownames(metadata) %in% common_samples, , drop = FALSE]
      metadata <- metadata[match(common_samples, rownames(metadata)), , drop = FALSE]
    }
  } else {
    # Positional matching as fallback
    n_samples <- min(nrow(data_matrix), nrow(metadata))
    data_matrix <- data_matrix[1:n_samples, , drop = FALSE]
    metadata <- metadata[1:n_samples, , drop = FALSE]
  }
  
  # Check if we have data
  if (nrow(data_matrix) == 0 || nrow(metadata) == 0) {
    stop("No samples remaining after filtering. Check sample/group selection.")
  }
  
  # Filter lipids
  available_lipids <- intersect(selected_lipids, colnames(data_matrix))
  if (length(available_lipids) == 0) {
    stop("None of the selected lipids are available in the data")
  }
  
  data_subset <- data_matrix[, available_lipids, drop = FALSE]
  
  # Create plot for each lipid
  plots <- list()
  
  for (lipid in available_lipids) {
    # Create plot data with proper alignment
    plot_data <- data.frame(
      Sample = rownames(data_subset),
      Abundance = as.numeric(data_subset[, lipid]),
      stringsAsFactors = FALSE
    )
    
    # Add group information safely
    if (group_column %in% colnames(metadata)) {
      plot_data$Group <- as.character(metadata[[group_column]])
    } else {
      plot_data$Group <- "Unknown"
    }
    
    # Verify dimensions match
    if (nrow(plot_data) != nrow(metadata)) {
      warning(paste("Dimension mismatch for lipid:", lipid))
      next
    }
    
    # Create barplot
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Sample, y = Abundance, fill = Group)) +
      ggplot2::geom_bar(stat = "identity", position = "dodge") +
      ggplot2::labs(
        title = paste("Expression of", lipid),
        subtitle = paste("Data type:", data_type),
        x = "Sample",
        y = "Abundance"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5),
        legend.position = "bottom"
      ) +
      ggplot2::scale_fill_brewer(palette = "Set2")
    
    plots[[lipid]] <- p
  }
  
  # If only one lipid, return single plot; otherwise return list
  if (length(plots) == 1) {
    return(plots[[1]])
  } else {
    return(plots)
  }
}

#' Create PCA Plot with Optional Ellipses
#'
#' @param pca_data Data frame with PC1, PC2, and Group columns
#' @param variance_explained Vector of variance explained by PC1 and PC2
#' @param ellipse_type Type of ellipse: "none", "confidence", or "visual"
#' @param confidence_level Confidence level for confidence ellipses (default 0.95)
#' @param title Plot title
#' @return ggplot object
#' @export
create_pca_plot_with_ellipses <- function(pca_data, 
                                          variance_explained, 
                                          ellipse_type = "none",
                                          confidence_level = 0.95,
                                          title = "PCA Analysis") {
  
  p <- ggplot2::ggplot(pca_data, ggplot2::aes(x = PC1, y = PC2, color = Group)) +
    ggplot2::geom_point(size = 4, alpha = 0.7) +
    ggplot2::labs(
      title = title,
      x = paste0("PC1 (", round(variance_explained[1], 2), "% variance)"),
      y = paste0("PC2 (", round(variance_explained[2], 2), "% variance)")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    )
  
  if (ellipse_type == "confidence") {
    p <- p + ggplot2::stat_ellipse(
      type = "norm",
      level = confidence_level,
      geom = "polygon",
      alpha = 0.1,
      ggplot2::aes(fill = Group)
    ) +
      ggplot2::stat_ellipse(
        type = "norm",
        level = confidence_level,
        linetype = 2,
        linewidth = 1
      )
  } else if (ellipse_type == "visual") {
    p <- p + ggplot2::stat_ellipse(
      type = "t",
      level = 0.8,
      geom = "polygon",
      alpha = 0.1,
      ggplot2::aes(fill = Group)
    ) +
      ggplot2::stat_ellipse(
        type = "t",
        level = 0.8,
        linetype = 2,
        linewidth = 1
      )
  }
  
  return(p)
}

#' Create PLS-DA Plot with Optional Ellipses
#'
#' @param plsda_data Data frame with Comp1, Comp2, and Group columns
#' @param ellipse_type Type of ellipse: "none", "confidence", or "visual"
#' @param confidence_level Confidence level for confidence ellipses (default 0.95)
#' @param title Plot title
#' @return ggplot object
#' @export
create_plsda_plot_with_ellipses <- function(plsda_data, 
                                            ellipse_type = "none",
                                            confidence_level = 0.95,
                                            title = "PLS-DA Analysis") {
  
  p <- ggplot2::ggplot(plsda_data, ggplot2::aes(x = Comp1, y = Comp2, color = Group)) +
    ggplot2::geom_point(size = 4, alpha = 0.8) +
    ggplot2::labs(
      title = title,
      x = "PLS-DA Component 1",
      y = "PLS-DA Component 2"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    )
  
  if (ellipse_type == "confidence") {
    p <- p + ggplot2::stat_ellipse(
      type = "norm",
      level = confidence_level,
      geom = "polygon",
      alpha = 0.1,
      ggplot2::aes(fill = Group)
    ) +
      ggplot2::stat_ellipse(
        type = "norm",
        level = confidence_level,
        linetype = 2,
        linewidth = 1
      )
  } else if (ellipse_type == "visual") {
    p <- p + ggplot2::stat_ellipse(
      type = "t",
      level = 0.8,
      geom = "polygon",
      alpha = 0.1,
      ggplot2::aes(fill = Group)
    ) +
      ggplot2::stat_ellipse(
        type = "t",
        level = 0.8,
        linetype = 2,
        linewidth = 1
      )
  }
  
  return(p)
}

#' Create Enrichment Dotplot - FIXED terminology
#'
#' @param enrichment_data Enrichment results data frame
#' @param title Plot title
#' @param max_pathways Maximum number of pathways to show
#' @return ggplot object
#' @export
create_enrichment_dotplot <- function(enrichment_data, title = "Enrichment Analysis", max_pathways = 15) {
  
  if (nrow(enrichment_data) == 0) {
    return(ggplot2::ggplot() + 
             ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No enrichment results") +
             ggplot2::theme_void())
  }
  
  # Select top pathways by p-value
  enrichment_data <- enrichment_data[order(enrichment_data$pval), ]
  enrichment_data <- utils::head(enrichment_data, max_pathways)
  
  # Create dotplot
  p <- ggplot2::ggplot(enrichment_data, 
                        ggplot2::aes(x = NES, y = stats::reorder(pathway, NES), 
                                     size = size, color = -log10(padj))) +
    ggplot2::geom_point() +
    ggplot2::scale_color_gradient(low = "blue", high = "red", name = "-log10(padj)") +
    ggplot2::scale_size_continuous(name = "Set Size") +
    ggplot2::labs(
      title = title,
      x = "Normalized Enrichment Score",
      y = "Pathway"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      axis.text.y = ggplot2::element_text(size = 8)
    )
  
  return(p)
}

#' Create Enrichment Barplot - FIXED terminology
#'
#' @param enrichment_data Enrichment results data frame
#' @param title Plot title
#' @param max_pathways Maximum number of pathways to show
#' @return ggplot object
#' @export
create_enrichment_barplot <- function(enrichment_data, title = "Enrichment Analysis", max_pathways = 15) {
  
  if (nrow(enrichment_data) == 0) {
    return(ggplot2::ggplot() + 
             ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No enrichment results") +
             ggplot2::theme_void())
  }
  
  # Select top pathways by p-value
  enrichment_data <- enrichment_data[order(enrichment_data$pval), ]
  enrichment_data <- utils::head(enrichment_data, max_pathways)
  
  # FIXED: Add direction with correct terminology
  enrichment_data$Direction <- ifelse(enrichment_data$NES > 0, "Increased abundance", "Decreased abundance")
  
  # Create barplot
  p <- ggplot2::ggplot(enrichment_data, 
                        ggplot2::aes(x = NES, y = stats::reorder(pathway, NES), fill = Direction)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(values = c("Increased abundance" = "red", "Decreased abundance" = "blue")) +
    ggplot2::labs(
      title = title,
      x = "Normalized Enrichment Score",
      y = "Pathway"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      axis.text.y = ggplot2::element_text(size = 8),
      legend.position = "bottom"
    )
  
  return(p)
}
