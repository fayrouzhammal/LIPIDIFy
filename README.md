# 🧪 LIPIDIFy

**LIPIDIFy** is a comprehensive **R package for lipidomics data analysis**, designed to support both:

- 🧬 **Experimental biologists**, via an interactive **Shiny application**
- 💻 **Bioinformaticians and data scientists**, via a fully scriptable R workflow

The package provides an end-to-end pipeline from raw lipidomics data to biological interpretation.

---

## ✨ Features

- Flexible **CSV data import**
- **Automatic lipid classification** (with support for custom annotations)
- Multiple **normalization strategies**
- **Differential abundance analysis** (limma, edgeR)
- **Enrichment analysis** of lipid classes or pathways
- Rich **visualizations**:
  - PCA / PLS-DA (with confidence ellipses)
  - Heatmaps
  - Volcano plots
  - Barplots
- Interactive **Shiny interface**
- Reproducible, modular **R functions**
- Automated report generation via **R Markdown**

---

## 🚀 Installation

```r
remotes::install_github("fayrouzhammal/LIPIDIFy")
library(LIPIDIFy)
```

---

## 🖥️ Use Case 1 — Shiny App (no coding required)

Launch the interactive application:

```r
launch_lipidomics_app()
```

### Shiny workflow

1. **Data Upload**
   - Upload a CSV file
   - Or load example lipidomics data

2. **Lipid Classification**
   - Use automatic classification
   - Or upload a custom classification file

3. **Raw Data Visualization**
   - Inspect distributions and sample quality

4. **Normalization**
   - Compare normalization methods
   - Apply the selected pipeline

5. **Differential Analysis**
   - limma or edgeR
   - Flexible contrast definition
   - FDR correction

6. **Enrichment Analysis**
   - Identify enriched lipid classes or pathways

All plots and tables can be downloaded directly from the interface.

---

## 🧬 Use Case 2 — Programmatic R Workflow

### Load lipidomics data

```r
data <- load_lipidomics_data("your_data.csv")
```

### Normalize data

```r
normalized <- normalize_lipidomics_data(
  data$numeric_data,
  methods = c("TIC", "Log2")
)
```

### Differential analysis

```r
results <- perform_differential_analysis(
  normalized,
  data$metadata,
  method = "limma"
)
```

### Lipid classification

```r
classification <- get_lipid_classification(
  colnames(data$numeric_data)
)
```

### Enrichment analysis

```r
enrichment <- perform_enrichment_analysis(
  results,
  classification
)
```

---

## 📄 Input Data Format

The input file must be a **CSV** with:

- Metadata columns first
- Lipid abundances as numeric columns

### Example

| Sample Name | Sample Group | Tumour ID | Weight (mg) | PC 16:0_18:1 | PE 18:0_20:4 | TG 16:0_18:1_20:4 |
|------------|-------------|-----------|-------------|--------------|--------------|-------------------|
| Sample_1 | GroupA | 1 | 45.2 | 125000 | 89000 | 234000 |
| Sample_2 | GroupA | 1 | 48.7 | 118000 | 92000 | 221000 |
| Sample_3 | GroupB | 2 | 52.1 | 135000 | 95000 | 245000 |

**Requirements**
- `Sample Name` and `Sample Group` are required
- Lipid names must be column headers
- No other metadata supported
- Values must be numeric
- Comma-separated CSV format

---

## 🧾 Lipid Classification

### Automatic classification

LIPIDIFy parses lipid names and assigns:

- **LipidGroup** (e.g. Glycerophospholipids)
- **LipidType** (e.g. Phosphatidylcholine)
- **Saturation** (SFA, MUFA, PUFA)

---

### Custom classification (optional)

You can upload your own classification file.

**Minimum format**

| Lipid | Class1 |
|------|--------|
| PC 16:0_18:1 | Phospholipids |
| TG 16:0_18:1_20:4 | Neutral Lipids |

**Extended format**

| Lipid | MainClass | SubClass | FunctionalGroup | Saturation |
|------|-----------|----------|-----------------|------------|

Requirements:
- First column must be named `Lipid`
- Lipid names must match the dataset
- CSV format

---

## 📊 Analysis Workflow

```
Data Upload
 → Lipid Classification
 → Raw Data Visualization
 → Normalization
 → Differential Analysis
 → Visualization
 → Enrichment Analysis
```

---

## 📚 Documentation

- Function-level help:
  ```r
  ?function_name
  ```
- Package vignette:
  ```r
  browseVignettes("LIPIDIFy")
  ```

---

## 🧪 Citation

If you use **LIPIDIFy** in your research, please cite:

> Citation information will be added upon publication.

---

## 💬 Support

- Bug reports and feature requests: GitHub **Issues**
- Contributions are welcome via pull requests

---

## ✅ Get started

- **Biologists**:
  ```r
  launch_lipidomics_app()
  ```
- **Bioinformaticians**: start with `load_lipidomics_data()`
