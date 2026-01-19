#' Perform Differential Analysis
#'
#' @param data_matrix Normalized data matrix (features as rows, samples as columns)
#' @param metadata Metadata data frame
#' @param group_column Column name in metadata containing group information
#' @param contrasts_list List of contrasts to perform
#' @param method Method to use: "limma" or "edger"
#' @return List containing results for each contrast
#' @export
perform_differential_analysis <- function(data_matrix, metadata, group_column = "Sample Group", 
                                          contrasts_list = NULL, method = "limma") {
  
  if (method == "limma") {
    return(perform_differential_analysis_limma(data_matrix, metadata, group_column, contrasts_list))
  } else if (method == "edger") {
    return(perform_differential_analysis_edger(data_matrix, metadata, group_column, contrasts_list))
  } else {
    stop("Method must be either 'limma' or 'edger'")
  }
}

#' Perform Differential Analysis with limma
#'
#' @param data_matrix Normalized data matrix (features as rows, samples as columns)
#' @param metadata Metadata data frame
#' @param group_column Column name in metadata containing group information
#' @param contrasts_list List of contrasts to perform
#' @return List containing LIMMA results for each contrast
perform_differential_analysis_limma <- function(data_matrix, metadata, group_column = "Sample Group", contrasts_list = NULL) {
  
  # Convert to matrix if needed
  if (!is.matrix(data_matrix)) {
    data_matrix <- as.matrix(data_matrix)
  }
  
  # Ensure data matrix is transposed correctly (features as rows)
  if (nrow(data_matrix) < ncol(data_matrix)) {
    data_matrix <- t(data_matrix)
  }
  
  # Create design matrix
  groups <- factor(metadata[[group_column]])
  design <- stats::model.matrix(~ 0 + groups)
  colnames(design) <- levels(groups)
  
  # Fit linear model
  fit <- limma::lmFit(data_matrix, design)
  
  # If no contrasts specified, create default contrasts
  if (is.null(contrasts_list)) {
    contrasts_list <- create_default_contrasts(levels(groups))
  }
  
  # Create contrast matrix
  contrast_matrix <- limma::makeContrasts(contrasts = contrasts_list, levels = design)
  
  # Fit contrasts and apply empirical Bayes
  fit2 <- limma::contrasts.fit(fit, contrast_matrix)
  fit2 <- limma::eBayes(fit2)
  
  # Extract results for each contrast
  results <- list()
  for (i in 1:ncol(contrast_matrix)) {
    contrast_name <- colnames(contrast_matrix)[i]
    results[[contrast_name]] <- limma::topTable(fit2, coef = i, number = Inf, adjust.method = "fdr")
  }
  
  return(list(
    fit = fit2,
    results = results,
    design = design,
    contrasts = contrast_matrix,
    method = "limma"
  ))
}

#' Perform Differential Analysis with EdgeR
#'
#' @param data_matrix Normalized data matrix (features as rows, samples as columns)
#' @param metadata Metadata data frame
#' @param group_column Column name in metadata containing group information
#' @param contrasts_list List of contrasts to perform
#' @return List containing EdgeR results for each contrast
perform_differential_analysis_edger <- function(data_matrix, metadata, group_column = "Sample Group", contrasts_list = NULL) {
  
  # Convert to matrix if needed
  if (!is.matrix(data_matrix)) {
    data_matrix <- as.matrix(data_matrix)
  }
  
  # Ensure data matrix is transposed correctly (features as rows)
  if (nrow(data_matrix) < ncol(data_matrix)) {
    data_matrix <- t(data_matrix)
  }
  
  # Convert to counts-like data (EdgeR expects integer counts, but can work with continuous data)
  # For lipidomics, we'll round the normalized data
  data_matrix <- round(data_matrix)
  data_matrix[data_matrix < 0] <- 0  # Ensure no negative values
  
  # Create groups factor
  groups <- factor(metadata[[group_column]])
  
  # Create DGEList object
  dge <- edgeR::DGEList(counts = data_matrix, group = groups)
  
  # Estimate dispersion
  dge <- edgeR::estimateDisp(dge)
  
  # Create design matrix
  design <- model.matrix(~ 0 + groups)
  colnames(design) <- levels(groups)
  
  # If no contrasts specified, create default contrasts
  if (is.null(contrasts_list)) {
    contrasts_list <- create_default_contrasts(levels(groups))
  }
  
  # Create contrast matrix
  contrast_matrix <- limma::makeContrasts(contrasts = contrasts_list, levels = design)
  
  # Fit GLM
  fit <- edgeR::glmQLFit(dge, design)
  
  # Extract results for each contrast
  results <- list()
  for (i in 1:ncol(contrast_matrix)) {
    contrast_name <- colnames(contrast_matrix)[i]
    qlf <- edgeR::glmQLFTest(fit, contrast = contrast_matrix[, i])
    
    # Extract results in similar format to limma
    res <- edgeR::topTags(qlf, n = Inf)$table
    
    # Rename columns to match limma output
    colnames(res)[colnames(res) == "logCPM"] <- "AveExpr"
    colnames(res)[colnames(res) == "PValue"] <- "P.Value"
    colnames(res)[colnames(res) == "FDR"] <- "adj.P.Val"
    
    results[[contrast_name]] <- res
  }
  
  return(list(
    fit = fit,
    results = results,
    design = design,
    contrasts = contrast_matrix,
    method = "edger"
  ))
}

#' Create Default Contrasts
#'
#' @param group_levels Character vector of group level names (e.g., c("Control","Treatment","Resistant"))
#' @return Character vector of limma-style contrast strings (e.g., "Treatment - Control")
#' @examples
#' create_default_contrasts(c("A","B","C"))
#' @export
create_default_contrasts <- function(group_levels) {
  group_levels <- as.character(group_levels)
  group_levels <- unique(group_levels[!is.na(group_levels)])
  if (length(group_levels) < 2) return(character(0))
  
  contrasts <- c()
  for (i in 1:(length(group_levels) - 1)) {
    for (j in (i + 1):length(group_levels)) {
      contrasts <- c(contrasts, paste0(group_levels[j], " - ", group_levels[i]))
    }
  }
  contrasts
}


#' Perform Enrichment Analysis
#'
#' @param results_list List of differential analysis results
#' @param classification_data Lipid classification data frame
#' @param min_set_size Minimum pathway set size
#' @param max_set_size Maximum pathway set size
#' @param custom_sets Optional named list of custom lipid sets
#' @return List containing GSEA results
#' @export
perform_enrichment_analysis <- function(results_list, classification_data, 
                                        min_set_size = 5, max_set_size = 500,
                                        custom_sets = NULL) {
  
  enrichment_results <- list()
  
  for (contrast_name in names(results_list)) {
    cat("Processing enrichment for:", contrast_name, "\n")
    
    results <- results_list[[contrast_name]]
    
    # Create ranked list
    ranked_vector <- results$logFC
    names(ranked_vector) <- rownames(results)
    ranked_vector <- sort(ranked_vector, decreasing = TRUE)
    
    # Merge with classification
    merged_data <- merge(
      data.frame(Lipid = names(ranked_vector), logFC = ranked_vector, stringsAsFactors = FALSE),
      classification_data,
      by = "Lipid",
      all.x = TRUE
    )
    
    # Create pathway sets from classification columns
    pathway_sets_list <- list()
    
    # Get all classification columns (excluding Lipid and logFC)
    class_columns <- setdiff(colnames(classification_data), "Lipid")
    
    for (class_col in class_columns) {
      if (class_col %in% colnames(merged_data)) {
        sets <- create_pathway_sets(merged_data, class_col)
        if (length(sets) > 0) {
          pathway_sets_list[[class_col]] <- sets
        }
      }
    }
    
    # Add custom sets if provided
    if (!is.null(custom_sets)) {
      pathway_sets_list[["custom"]] <- custom_sets
    }
    
    # Run GSEA for each set of pathways
    gsea_results <- list()
    for (set_name in names(pathway_sets_list)) {
      gsea_res <- run_fgsea_safe(pathway_sets_list[[set_name]], ranked_vector, min_set_size, max_set_size)
      if (!is.null(gsea_res) && nrow(gsea_res) > 0) {
        gsea_results[[set_name]] <- gsea_res
      }
    }
    
    enrichment_results[[contrast_name]] <- gsea_results
  }
  
  return(enrichment_results)
}

#' Load Custom Enrichment Sets
#'
#' Load custom lipid sets for enrichment analysis from a CSV file.
#' File should have columns: Lipid, Set_Name
#'
#' @param file_path Path to CSV file with Lipid and Set_Name columns
#' @return Named list of lipid sets
#' @export
#' @examples
#' \dontrun{
#' custom_sets <- load_custom_enrichment_sets("my_sets.csv")
#' }
load_custom_enrichment_sets <- function(file_path) {
  # Read the file
  sets_df <- utils::read.csv(file_path, check.names = FALSE, stringsAsFactors = FALSE)
  
  # Validate columns
  if (!all(c("Lipid", "Set_Name") %in% colnames(sets_df))) {
    stop("Custom sets file must contain 'Lipid' and 'Set_Name' columns")
  }
  
  # Create named list of sets
  sets_list <- split(sets_df$Lipid, sets_df$Set_Name)
  
  return(sets_list)
}

#' Create Pathway Sets
#'
#' @param merged_data Data frame with lipids and classifications
#' @param classification_column Column name for classification
#' @return Named list of pathway sets
create_pathway_sets <- function(merged_data, classification_column) {
  # Use base R instead of dplyr pipeline
  grouped_data <- split(merged_data$Lipid, merged_data[[classification_column]])
  
  # Remove any NULL or empty groups
  pathway_sets <- grouped_data[sapply(grouped_data, length) > 0]
  
  return(pathway_sets)
}

#' Run FGSEA with Error Handling
#'
#' @param pathway_sets Named list of pathway sets
#' @param ranked_vector Named numeric vector of ranked statistics
#' @param min_size Minimum set size
#' @param max_size Maximum set size
#' @return FGSEA results data frame or NULL if error
run_fgsea_safe <- function(pathway_sets, ranked_vector, min_size, max_size) {
  if (length(pathway_sets) == 0) {
    return(data.frame())
  }
  
  tryCatch({
    # Add small random noise to avoid ties
    ranked_vector <- ranked_vector + stats::rnorm(length(ranked_vector), sd = 1e-6)
    
    gsea_results <- fgsea::fgseaMultilevel(
      pathways = pathway_sets,
      stats = ranked_vector,
      minSize = min_size,
      maxSize = max_size
    )
    
    # Convert list columns to strings
    gsea_results <- convert_list_columns_to_strings(gsea_results)
    
    return(gsea_results)
    
  }, error = function(e) {
    # Check if it's the corrupt database error
    if (grepl("corrupt", e$message, ignore.case = TRUE)) {
      warning("fgsea database appears corrupt. Try reinstalling: install.packages('fgsea')")
    }
    warning(paste("FGSEA error:", e$message))
    return(NULL)
  })
}

#' Run FGSEA (original function kept for compatibility)
#'
#' @param pathway_sets Named list of pathway sets
#' @param ranked_vector Named numeric vector of ranked statistics
#' @param min_size Minimum set size
#' @param max_size Maximum set size
#' @return FGSEA results data frame
run_fgsea <- function(pathway_sets, ranked_vector, min_size, max_size) {
  return(run_fgsea_safe(pathway_sets, ranked_vector, min_size, max_size))
}

#' Convert List Columns to Strings
#'
#' @param df Data frame with potential list columns
#' @return Data frame with list columns converted to strings
convert_list_columns_to_strings <- function(df) {
  for (col in names(df)) {
    if (is.list(df[[col]])) {
      df[[col]] <- sapply(df[[col]], function(x) {
        if (is.null(x)) {
          return(NA)
        } else {
          paste(x, collapse = ",")
        }
      })
    }
  }
  return(df)
}

#' Perform PCA Analysis
#'
#' @param data_matrix Data matrix (samples as rows)
#' @param metadata Metadata data frame
#' @param group_column Group column name
#' @return List containing PCA results and plot
#' @export
perform_pca <- function(data_matrix, metadata, group_column = "Sample Group") {
  
  # Ensure data is in correct format (samples as rows)
  if (ncol(data_matrix) < nrow(data_matrix)) {
    data_matrix <- t(data_matrix)
  }
  
  # Remove any columns with zero variance
  var_cols <- apply(data_matrix, 2, stats::var, na.rm = TRUE) > 0
  data_matrix <- data_matrix[, var_cols, drop = FALSE]
  
  # Perform PCA
  pca_results <- FactoMineR::PCA(data_matrix, scale.unit = TRUE, graph = FALSE)
  
  # Create PCA plot data
  pca_data <- data.frame(
    Sample = rownames(data_matrix),
    PC1 = pca_results$ind$coord[, 1],
    PC2 = pca_results$ind$coord[, 2],
    Group = metadata[[group_column]],
    stringsAsFactors = FALSE
  )
  
  # Create plot
  variance_explained <- pca_results$eig[1:2, 2]
  
  pca_plot <- ggplot2::ggplot(pca_data, ggplot2::aes(x = PC1, y = PC2, color = Group)) +
    ggplot2::geom_point(size = 4, alpha = 0.7) +
    ggplot2::labs(
      title = "PCA Analysis",
      x = paste0("PC1 (", round(variance_explained[1], 2), "% variance)"),
      y = paste0("PC2 (", round(variance_explained[2], 2), "% variance)")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
  
  return(list(
    pca_results = pca_results,
    pca_data = pca_data,
    plot = pca_plot,
    variance_explained = variance_explained
  ))
}

#' Perform PLS-DA Analysis (FIXED VERSION)
#'
#' @param data_matrix Data matrix (samples as rows)
#' @param metadata Metadata data frame
#' @param group_column Group column name
#' @param n_comp Number of components
#' @return List containing PLS-DA results and plot
#' @export
perform_plsda <- function(data_matrix, metadata, group_column = "Sample Group", n_comp = 2) {
  
  tryCatch({
    # CRITICAL FIX: Ensure it's a matrix, not a data.frame or list
    if (!is.matrix(data_matrix)) {
      cat("Converting data to matrix\n")
      data_matrix <- as.matrix(data_matrix)
    }
    
    # Ensure data is in correct format (samples as rows)
    if (ncol(data_matrix) < nrow(data_matrix)) {
      cat("Transposing data matrix for PLS-DA\n")
      data_matrix <- t(data_matrix)
    }
    
    # Remove any columns with zero variance
    var_cols <- apply(data_matrix, 2, stats::var, na.rm = TRUE) > 0
    data_matrix <- data_matrix[, var_cols, drop = FALSE]
    
    cat("PLS-DA data matrix dimensions:", dim(data_matrix), "\n")
    cat("PLS-DA metadata dimensions:", dim(metadata), "\n")
    
    # Prepare data
    groups <- factor(metadata[[group_column]])
    
    # Check for sample correspondence
    if (nrow(data_matrix) != length(groups)) {
      stop(paste("Number of samples in data matrix (", nrow(data_matrix), 
                 ") doesn't match metadata (", length(groups), ")"))
    }
    
    # Ensure we have at least 2 groups
    if (length(levels(groups)) < 2) {
      stop("PLS-DA requires at least 2 groups")
    }
    
    # Create dummy variables for groups
    group_dummy <- model.matrix(~ groups - 1)
    
    # Handle column naming for model.matrix output
    colnames(group_dummy) <- paste0("Group_", levels(groups))
    
    cat("Group dummy matrix dimensions:", dim(group_dummy), "\n")
    cat("Unique groups:", levels(groups), "\n")
    
    # Perform PLS-DA using pls package
    plsda_results <- pls::plsr(group_dummy ~ data_matrix, ncomp = n_comp, validation = "LOO")
    
    # Extract scores
    scores_data <- data.frame(
      Sample = rownames(data_matrix),
      Comp1 = plsda_results$scores[, 1],
      Comp2 = if(n_comp > 1) plsda_results$scores[, 2] else rep(0, nrow(plsda_results$scores)),
      Group = groups,
      stringsAsFactors = FALSE
    )
    
    # Create plot
    plsda_plot <- ggplot2::ggplot(scores_data, ggplot2::aes(x = Comp1, y = Comp2, color = Group)) +
      ggplot2::geom_point(size = 4, alpha = 0.8) +
      ggplot2::labs(
        title = "PLS-DA Analysis",
        x = "PLS-DA Component 1",
        y = "PLS-DA Component 2"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom")
    
    cat("PLS-DA completed successfully\n")
    
    return(list(
      plsda_results = plsda_results,
      scores_data = scores_data,
      plot = plsda_plot
    ))
    
  }, error = function(e) {
    cat("PLS-DA failed with error:", e$message, "\n")
    warning(paste("PLS-DA failed:", e$message))
    
    # Fallback: create simple plot showing error
    error_plot <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, 
                        label = paste("PLS-DA failed:", e$message),
                        size = 5) +
      ggplot2::theme_void() +
      ggplot2::labs(title = "PLS-DA Analysis (Error)")
    
    return(list(
      plsda_results = NULL,
      scores_data = data.frame(),
      plot = error_plot
    ))
  })
}
