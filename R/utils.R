#' Load Lipidomics Data
#'
#' @param file_path Path to the lipidomics data file
#' @param metadata_columns Vector of metadata column names
#' @return List containing data matrix and metadata
#' @export
load_lipidomics_data <- function(file_path, metadata_columns = c("Sample Name", "Sample Group", "Tumour ID", "Weight (mg)")) {
  # Read the data
  data <- utils::read.csv(file_path, check.names = FALSE, stringsAsFactors = FALSE)
  
  # Filter out PBQC samples if present
  if ("Sample Group" %in% colnames(data)) {
    data <- data[!grepl("PBQC", data$`Sample Group`, ignore.case = TRUE), ]
  }
  
  # Separate metadata and numeric data
  metadata <- data[, names(data) %in% metadata_columns, drop = FALSE]
  numeric_data <- data[, !names(data) %in% metadata_columns, drop = FALSE]
  
  # Convert numeric data to matrix
  numeric_data <- as.matrix(numeric_data)
  
  # Set rownames based on Sample Name if available
  if ("Sample Name" %in% colnames(metadata)) {
    rownames(numeric_data) <- metadata$`Sample Name`
    rownames(metadata) <- metadata$`Sample Name`
  }
  
  # Remove the last column if it appears to be irrelevant (empty or constant)
  if (ncol(numeric_data) > 0) {
    last_col <- numeric_data[, ncol(numeric_data)]
    if (all(is.na(last_col)) || length(unique(last_col[!is.na(last_col)])) <= 1) {
      numeric_data <- numeric_data[, -ncol(numeric_data), drop = FALSE]
    }
  }
  
  return(list(
    data = data,
    metadata = metadata,
    numeric_data = numeric_data
  ))
}

#' Classify Lipids Based on Names
#'
#' @param lipid_names Vector of lipid names
#' @return Data frame with classification results
#' @export
classify_lipids <- function(lipid_names) {
  classify_single_lipid <- function(lipid) {
    lipid_clean <- stringr::str_trim(lipid)
    
    group <- "Other/Unclassified"
    type <- "Other"
    saturation <- "Unclassified"
    
    # Classification logic for lipid groups and types
    if (stringr::str_detect(lipid_clean, stringr::regex("^PC", ignore_case = TRUE))) {
      group <- "Glycerophospholipids"
      type <- "Phosphatidylcholine"
    } else if (stringr::str_detect(lipid_clean, stringr::regex("^PE", ignore_case = TRUE))) {
      group <- "Glycerophospholipids"
      type <- "Phosphatidylethanolamine"
    } else if (stringr::str_detect(lipid_clean, stringr::regex("^PG", ignore_case = TRUE))) {
      group <- "Glycerophospholipids"
      type <- "Phosphatidylglycerol"
    } else if (stringr::str_detect(lipid_clean, stringr::regex("^PI", ignore_case = TRUE))) {
      group <- "Glycerophospholipids"
      type <- "Phosphatidylinositol"
    } else if (stringr::str_detect(lipid_clean, stringr::regex("^PS", ignore_case = TRUE))) {
      group <- "Glycerophospholipids"
      type <- "Phosphatidylserine"
    } else if (stringr::str_detect(lipid_clean, stringr::regex("^LPC", ignore_case = TRUE))) {
      group <- "Lysophospholipids"
      type <- "Lysophosphatidylcholine"
    } else if (stringr::str_detect(lipid_clean, stringr::regex("^LPE", ignore_case = TRUE))) {
      group <- "Lysophospholipids"
      type <- "Lysophosphatidylethanolamine"
    } else if (stringr::str_detect(lipid_clean, stringr::regex("^LPI", ignore_case = TRUE))) {
      group <- "Lysophospholipids"
      type <- "Lysophosphatidylinositol"
    } else if (stringr::str_detect(lipid_clean, stringr::regex("^DG", ignore_case = TRUE))) {
      group <- "Glycerolipids"
      type <- "Diacylglycerol"
    } else if (stringr::str_detect(lipid_clean, stringr::regex("^TG", ignore_case = TRUE))) {
      group <- "Glycerolipids"
      type <- "Triacylglycerol"
    } else if (stringr::str_detect(lipid_clean, stringr::regex("^dhCer", ignore_case = TRUE))) {
      group <- "Sphingolipids"
      type <- "Dihydroceramide"
    } else if (stringr::str_detect(lipid_clean, stringr::regex("^Cer", ignore_case = TRUE))) {
      group <- "Sphingolipids"
      type <- "Ceramide"
    } else if (stringr::str_detect(lipid_clean, stringr::regex("^Hex", ignore_case = TRUE))) {
      group <- "Sphingolipids"
      type <- "Hexosylceramide"
    } else if (stringr::str_detect(lipid_clean, stringr::regex("^SM", ignore_case = TRUE))) {
      group <- "Sphingolipids"
      type <- "Sphingomyelin"
    } else if (stringr::str_detect(lipid_clean, stringr::regex("^GM", ignore_case = TRUE))) {
      group <- "Sphingolipids"
      type <- "Ganglioside"
    } else if (stringr::str_detect(lipid_clean, stringr::regex("^Sulfatide", ignore_case = TRUE))) {
      group <- "Sphingolipids"
      type <- "Sulfatide"
    } else if (stringr::str_detect(lipid_clean, stringr::regex("^CE", ignore_case = TRUE))) {
      group <- "Sterol Lipids"
      type <- "Cholesteryl Ester"
    } else if (stringr::str_detect(lipid_clean, stringr::regex("^Desmosterol", ignore_case = TRUE))) {
      group <- "Sterol Lipids"
      type <- "Desmosterol"
    } else if (stringr::str_detect(lipid_clean, stringr::regex("^COH", ignore_case = TRUE))) {
      group <- "Sterol Lipids"
      type <- "Cholesterol/Related"
    } else if (stringr::str_detect(lipid_clean, stringr::regex("^Ubiquinone", ignore_case = TRUE))) {
      group <- "Sterol Lipids"
      type <- "Ubiquinone"
    } else if (stringr::str_detect(lipid_clean, stringr::regex("^AcylCarnitine", ignore_case = TRUE))) {
      group <- "Acylcarnitines"
      type <- "Acylcarnitine"
    } else if (stringr::str_detect(lipid_clean, stringr::regex("(IS)", ignore_case = TRUE))) {
      group <- "Internal Standard"
      type <- lipid_clean
    }
    
    # ENHANCED SATURATION DETECTION
    saturation <- determine_saturation(lipid_clean)
    
    return(data.frame(
      Lipid = lipid_clean,
      LipidGroup = group,
      LipidType = type,
      Saturation = saturation,
      stringsAsFactors = FALSE
    ))
  }
  
  # Apply to all lipids
  results <- do.call(rbind, lapply(lipid_names, classify_single_lipid))
  return(results)
}

#' Enhanced Saturation Determination Function
#'
#' @param lipid_name Single lipid name
#' @return Saturation classification (SFA, MUFA, PUFA, or Unclassified)
determine_saturation <- function(lipid_name) {
  
  tryCatch({
    # Pattern 1: Standard notation (e.g., "PC 16:0_18:1", "LPC 18:2", "TG 16:0_18:1_20:4")
    pattern1 <- stringr::str_extract_all(lipid_name, "\\b\\d+:\\d+\\b")[[1]]
    
    if (length(pattern1) > 0) {
      # Extract double bond numbers
      double_bonds <- sapply(pattern1, function(x) {
        as.numeric(stringr::str_split(x, ":")[[1]][2])
      })
      
      total_double_bonds <- sum(double_bonds, na.rm = TRUE)
      
      if (total_double_bonds == 0) {
        return("SFA")  # Saturated Fatty Acid
      } else if (total_double_bonds == 1) {
        return("MUFA") # Monounsaturated Fatty Acid
      } else if (total_double_bonds > 1) {
        return("PUFA") # Polyunsaturated Fatty Acid
      }
    }
    
    # Pattern 2: Alternative notation with parentheses (e.g., "PE(P-18:0_20:5)")
    pattern2 <- stringr::str_extract_all(lipid_name, "\\(.*?\\)")[[1]]
    
    if (length(pattern2) > 0) {
      # Extract content within parentheses
      content <- stringr::str_remove_all(pattern2[1], "[\\(\\)]")
      
      # Look for fatty acid patterns within parentheses
      fa_patterns <- stringr::str_extract_all(content, "\\d+:\\d+")[[1]]
      
      if (length(fa_patterns) > 0) {
        double_bonds <- sapply(fa_patterns, function(x) {
          as.numeric(stringr::str_split(x, ":")[[1]][2])
        })
        
        total_double_bonds <- sum(double_bonds, na.rm = TRUE)
        
        if (total_double_bonds == 0) {
          return("SFA")
        } else if (total_double_bonds == 1) {
          return("MUFA")
        } else if (total_double_bonds > 1) {
          return("PUFA")
        }
      }
    }
    
    # Pattern 3: Space-separated notation (e.g., "PC 18:0 20:4")
    pattern3 <- stringr::str_extract_all(lipid_name, "\\s\\d+:\\d+")[[1]]
    
    if (length(pattern3) > 0) {
      # Clean and extract double bonds
      cleaned_patterns <- stringr::str_trim(stringr::str_remove(pattern3, "^\\s"))
      
      double_bonds <- sapply(cleaned_patterns, function(x) {
        parts <- stringr::str_split(x, ":")[[1]]
        if (length(parts) >= 2) {
          as.numeric(parts[2])
        } else {
          0
        }
      })
      
      total_double_bonds <- sum(double_bonds, na.rm = TRUE)
      
      if (total_double_bonds == 0) {
        return("SFA")
      } else if (total_double_bonds == 1) {
        return("MUFA")
      } else if (total_double_bonds > 1) {
        return("PUFA")
      }
    }
    
    # Pattern 4: Total carbon:double bond notation (e.g., "PE 35:2" where 35 is total carbons, 2 is total double bonds)
    pattern4 <- stringr::str_extract(lipid_name, "\\s(\\d{2,3}):(\\d+)\\s*$")
    
    if (!is.na(pattern4)) {
      # Extract the double bond number directly
      db_match <- stringr::str_match(pattern4, "\\s\\d{2,3}:(\\d+)")
      
      if (!is.na(db_match[1,2])) {
        total_double_bonds <- as.numeric(db_match[1,2])
        
        if (total_double_bonds == 0) {
          return("SFA")
        } else if (total_double_bonds == 1) {
          return("MUFA")
        } else if (total_double_bonds > 1) {
          return("PUFA")
        }
      }
    }
    
    # If no pattern matches, return unclassified
    return("Unclassified")
    
  }, error = function(e) {
    return("Unclassified")
  })
}

#' Test function to check saturation classification
#'
#' @param test_lipids Vector of test lipid names
#' @return Data frame showing classification results
#' @export
test_saturation_classification <- function(test_lipids = NULL) {
  
  if (is.null(test_lipids)) {
    # Default test cases
    test_lipids <- c(
      "PC 16:0_18:1",    # Should be MUFA (0+1=1)
      "LPC 18:2",        # Should be PUFA (2)
      "PE 18:0_18:0",    # Should be SFA (0+0=0)
      "TG 16:0_18:1_20:4", # Should be PUFA (0+1+4=5)
      "PE(P-18:0_20:5)", # Should be PUFA (0+5=5)
      "SM 16:0",         # Should be SFA (0)
      "CE 18:1",         # Should be MUFA (1)
      "PE 35:2",         # Should be PUFA (total 2 double bonds)
      "PC 34:1",         # Should be MUFA (total 1 double bond)
      "LPC 20:5"         # Should be PUFA (5)
    )
  }
  
  results <- data.frame(
    Lipid = test_lipids,
    Saturation = sapply(test_lipids, determine_saturation),
    stringsAsFactors = FALSE
  )
  
  cat("Saturation Classification Test Results:\n")
  print(results)
  
  return(results)
}

#' Get Available Normalization Methods
#'
#' @return Vector of normalization method names
#' @export
get_normalization_methods <- function() {
  c("TIC", "PQN", "Quantile", "VSN", "Median", "Mean", "Log2", "Log10", "Sqrt", "None")
}

#' Apply Multiple Normalizations
#'
#' @param data Numeric data matrix
#' @param methods Vector of normalization methods to apply in order
#' @return Normalized data matrix
#' @export
apply_normalizations <- function(data, methods) {
  
  # Ensure data is a matrix
  if (!is.matrix(data)) {
    data <- as.matrix(data)
  }
  
  normalized_data <- data
  
  for (method in methods) {
    normalized_data <- switch(method,
      "TIC" = normalize_tic(normalized_data),
      "PQN" = normalize_pqn(normalized_data),
      "Quantile" = normalize_quantile(normalized_data),
      "VSN" = normalize_vsn(normalized_data),
      "Median" = normalize_median(normalized_data),
      "Mean" = normalize_mean(normalized_data),
      "Log2" = log2(normalized_data + 1),
      "Log10" = log10(normalized_data + 1),
      "Sqrt" = sqrt(abs(normalized_data)),
      "None" = normalized_data,
      normalized_data
    )
  }
  
  # Preserve rownames
  if (!is.null(rownames(data))) {
    rownames(normalized_data) <- rownames(data)
  }
  
  return(normalized_data)
}

#' TIC Normalization
#'
#' @param data Numeric data matrix (samples as rows)
#' @return TIC normalized data
normalize_tic <- function(data) {
  # Calculate TIC per sample (row sums)
  tic <- rowSums(data, na.rm = TRUE)
  mean_tic <- mean(tic, na.rm = TRUE)
  
  # Normalize each sample
  normalized_data <- sweep(data, 1, tic, "/") * mean_tic
  
  return(normalized_data)
}

#' PQN Normalization
#'
#' @param data Numeric data matrix (samples as rows)
#' @return PQN normalized data
normalize_pqn <- function(data) {
  # Calculate reference spectrum (median across samples for each feature)
  reference <- apply(data, 2, median, na.rm = TRUE)
  
  # Calculate quotients for each sample
  quotients <- sweep(data, 2, reference, "/")
  
  # Calculate median quotient for each sample
  median_quotients <- apply(quotients, 1, median, na.rm = TRUE)
  
  # Normalize by median quotients
  normalized_data <- sweep(data, 1, median_quotients, "/")
  return(normalized_data)
}

#' Quantile Normalization
#'
#' @param data Numeric data matrix
#' @return Quantile normalized data
normalize_quantile <- function(data) {
  # Transpose for quantile normalization (features as rows for standard approach)
  data_t <- t(data)
  
  # Simple quantile normalization
  ranks <- apply(data_t, 2, rank, ties.method = "average")
  sorted_data <- apply(data_t, 2, sort)
  mean_sorted <- rowMeans(sorted_data, na.rm = TRUE)
  
  normalized_data_t <- data_t
  for (i in 1:ncol(data_t)) {
    normalized_data_t[, i] <- mean_sorted[ranks[, i]]
  }
  
  return(t(normalized_data_t))
}

#' VSN Normalization (simplified)
#'
#' @param data Numeric data matrix
#' @return VSN normalized data
normalize_vsn <- function(data) {
  # Simplified VSN - log2 transform and median centering
  log_data <- log2(data + 1)
  medians <- apply(log_data, 1, median, na.rm = TRUE)
  global_median <- stats::median(medians, na.rm = TRUE)
  normalized_data <- sweep(log_data, 1, medians - global_median, "-")
  return(2^normalized_data - 1)
}

#' Median Normalization
#'
#' @param data Numeric data matrix (samples as rows)
#' @return Median normalized data
normalize_median <- function(data) {
  # Calculate median per sample
  medians <- apply(data, 1, median, na.rm = TRUE)
  global_median <- stats::median(medians, na.rm = TRUE)
  normalized_data <- sweep(data, 1, medians / global_median, "/")
  return(normalized_data)
}

#' Mean Normalization
#'
#' @param data Numeric data matrix (samples as rows)
#' @return Mean normalized data
normalize_mean <- function(data) {
  # Calculate mean per sample
  means <- apply(data, 1, mean, na.rm = TRUE)
  global_mean <- mean(means, na.rm = TRUE)
  normalized_data <- sweep(data, 1, means / global_mean, "/")
  return(normalized_data)
}

#' Normalize Lipidomics Data
#'
#' @param data Numeric data matrix  
#' @param methods Vector of normalization methods
#' @return Normalized data matrix
#' @export
normalize_lipidomics_data <- function(data, methods = c("TIC", "Log2")) {
  return(apply_normalizations(data, methods))
}

#' Load Custom Lipid Classification
#'
#' Loads a custom lipid classification from a CSV file. The file should have
#' a 'Lipid' column followed by one or more classification columns.
#'
#' @param file_path Path to the CSV file containing lipid classifications
#' @return Data frame with lipid names and their classifications
#' @export
#' @examples
#' \dontrun{
#' custom_class <- load_custom_classification("my_classification.csv")
#' }
load_custom_classification <- function(file_path) {
  # Read the classification file
  classification <- utils::read.csv(file_path, check.names = FALSE, stringsAsFactors = FALSE)
  
  # Validate that there is a 'Lipid' column
  if (!"Lipid" %in% colnames(classification)) {
    stop("Classification file must contain a 'Lipid' column")
  }
  
  # Ensure Lipid is the first column
  lipid_col <- classification$Lipid
  other_cols <- classification[, colnames(classification) != "Lipid", drop = FALSE]
  classification <- data.frame(Lipid = lipid_col, other_cols, stringsAsFactors = FALSE)
  
  return(classification)
}

#' Export Lipid Classification
#'
#' Exports the lipid classification to a CSV file.
#'
#' @param classification Data frame with lipid classifications
#' @param file_path Output file path
#' @return Invisibly returns TRUE on success
#' @export
#' @examples
#' \dontrun{
#' lipids <- c("PC 16:0_18:1", "PE 18:0_20:4")
#' classification <- classify_lipids(lipids)
#' export_classification(classification, "lipid_classification.csv")
#' }
export_classification <- function(classification, file_path) {
  utils::write.csv(classification, file_path, row.names = FALSE)
  invisible(TRUE)
}

#' Get Lipid Classification from Names
#'
#' Wrapper function to classify lipids and return the classification data frame.
#' This function is exported for use in scripts.
#'
#' @param lipid_names Vector of lipid names to classify
#' @return Data frame with lipid classifications (Lipid, LipidGroup, LipidType, Saturation)
#' @export
#' @examples
#' lipids <- c("PC 16:0_18:1", "PE 18:0_20:4", "TG 16:0_18:1_20:4")
#' classification <- get_lipid_classification(lipids)
#' print(classification)
get_lipid_classification <- function(lipid_names) {
  return(classify_lipids(lipid_names))
}
