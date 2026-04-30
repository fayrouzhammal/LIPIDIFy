# LIPIDIFy 0.99.0

## New features
- Added `normalize_mean()` and full pipeline builder supporting TIC, PQN,
  Quantile, VSN, Median, Mean, Log2, Log10, Sqrt, None
- Added `get_normalization_descriptions()` for UI help text
- Shiny app: Select All / Deselect All buttons on every checkbox group
- Shiny app: group-coloured normalization comparison plots
- Shiny app: full plot history tracked across the session for complete reports
- Shiny app: samples sorted by group in Lipid Expression plots
- Shiny app: descriptive, timestamped download filenames for all plots

## Bug fixes
- `create_pca_plot_with_ellipses()`: fixed duplicate legend keys when ellipses
  are drawn (fill + color legends now merged into one)
- `create_lipid_expression_barplot()`: samples now sorted by group, not by
  sample name
- Excel export: worksheet names truncated to Excel's 31-character limit
- Normalization help text modal and ellipse help modal now display correctly
- `perform_plsda()`: replaced `cat()` with `message()` (Bioconductor requirement)
- `perform_enrichment_analysis()`: replaced `cat()` with `message()`

## Notes
- Quantile normalization producing near-identical boxplots is expected and
  correct behaviour; inline documentation and UI now make this explicit
- Median and Mean normalization can look similar on symmetric (log-normal)
  lipidomics data; on skewed data they differ — this is also expected
