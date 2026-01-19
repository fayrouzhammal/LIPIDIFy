#' Launch Lipidomics Analyser Shiny App (FULLY CORRECTED VERSION)
#'
#' @param port Port number for the Shiny app
#' @return Launches the Shiny application
#' @export
launch_lipidomics_app <- function(port = NULL) {
  
  # Define UI with reorganized menu
  ui <- shinydashboard::dashboardPage(
    
    # Header
    shinydashboard::dashboardHeader(title = "LIPIDIFy"),
    
    # Sidebar with reorganized menu
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Welcome", tabName = "welcome", icon = shiny::icon("info-circle")),
        shinydashboard::menuItem("Data Upload", tabName = "upload", icon = shiny::icon("upload")),
        shinydashboard::menuItem("Lipid Classification", tabName = "classification", icon = shiny::icon("tags")),
        shinydashboard::menuItem("Raw Data Visualization", tabName = "raw_viz", icon = shiny::icon("chart-line")),
        shinydashboard::menuItem("Normalization", tabName = "normalization", icon = shiny::icon("balance-scale")),
        shinydashboard::menuItem("Normalized Data Viz", tabName = "norm_viz", icon = shiny::icon("chart-bar")),
        shinydashboard::menuItem("Lipid Expression", tabName = "lipid_expression", icon = shiny::icon("chart-bar")),
        shinydashboard::menuItem("Differential Analysis", tabName = "diff_analysis", icon = shiny::icon("calculator")),
        shinydashboard::menuItem("Results Visualization", tabName = "results_viz", icon = shiny::icon("chart-area")),
        shinydashboard::menuItem("Enrichment Analysis", tabName = "enrichment", icon = shiny::icon("search-plus")),
        shinydashboard::menuItem("Enrichment Visualization", tabName = "enrichment_viz", icon = shiny::icon("network-wired")),
        shinydashboard::menuItem("Generate Report", tabName = "report", icon = shiny::icon("file-alt"))
      )
    ),
    
    # Body
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        
        # Welcome Tab - IMPROVED with detailed explanation and example
        shinydashboard::tabItem(
          tabName = "welcome",
          shiny::fluidRow(
            shinydashboard::box(
              title = "Welcome to LIPIDIFy",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              shiny::HTML("
                <h3>A Comprehensive Tool for Lipidomics Data Analysis</h3>
                <p>LIPIDIFy is a user-friendly Shiny application designed for the complete analysis of lipidomics datasets. 
                It provides an end-to-end workflow from data upload to publication-ready visualizations and reports.</p>
                
                <hr>
                
                <h4><i class='fa fa-list-ol'></i> Analysis Workflow</h4>
                <ol>
                  <li><strong>Data Upload:</strong> Load your CSV file or use our example dataset to explore the tool</li>
                  <li><strong>Lipid Classification:</strong> Automatic classification of lipids by group, type, and saturation</li>
                  <li><strong>Raw Data Visualization:</strong> Explore data distribution before normalization</li>
                  <li><strong>Normalization:</strong> Apply and compare different normalization pipelines</li>
                  <li><strong>Lipid Expression:</strong> Visualize individual lipid abundance across samples or groups</li>
                  <li><strong>Differential Analysis:</strong> Identify significantly altered lipids between groups</li>
                  <li><strong>Enrichment Analysis:</strong> Discover enriched lipid classes and pathways</li>
                  <li><strong>Report Generation:</strong> Export your complete analysis as HTML or PDF</li>
                </ol>
                
                <hr>
                
                <h4><i class='fa fa-file-csv'></i> Input Data Format</h4>
                <p>Your CSV file should be organized with:</p>
                <ul>
                  <li><strong>Rows:</strong> Samples (one sample per row)</li>
                  <li><strong>Columns:</strong> Metadata columns followed by lipid abundance values</li>
                </ul>
                
                <h5>Example Input Format:</h5>
                <div style='overflow-x: auto;'>
                <table class='table table-bordered table-striped' style='font-size: 12px;'>
                  <thead style='background-color: #3c8dbc; color: white;'>
                    <tr>
                      <th>Sample Name</th>
                      <th>Sample Group</th>
                      <th>Tumour ID</th>
                      <th>Weight (mg)</th>
                      <th>PC 16:0_18:1</th>
                      <th>PC 18:0_20:4</th>
                      <th>PE 16:0_18:2</th>
                      <th>SM 18:1</th>
                      <th>...</th>
                    </tr>
                  </thead>
                  <tbody>
                    <tr><td>Sample_1</td><td>Control</td><td>T001</td><td>52.3</td><td>15420.5</td><td>8932.1</td><td>12045.8</td><td>5621.3</td><td>...</td></tr>
                    <tr><td>Sample_2</td><td>Control</td><td>T002</td><td>48.7</td><td>14856.2</td><td>9125.4</td><td>11876.3</td><td>5489.7</td><td>...</td></tr>
                    <tr><td>Sample_3</td><td>Treatment</td><td>T003</td><td>51.2</td><td>18932.1</td><td>7845.6</td><td>14532.9</td><td>6234.5</td><td>...</td></tr>
                    <tr><td>Sample_4</td><td>Treatment</td><td>T004</td><td>49.8</td><td>19245.8</td><td>7523.2</td><td>14876.1</td><td>6521.8</td><td>...</td></tr>
                    <tr><td>Sample_5</td><td>Resistant</td><td>T005</td><td>53.1</td><td>12456.3</td><td>11234.5</td><td>9876.4</td><td>4532.1</td><td>...</td></tr>
                  </tbody>
                </table>
                </div>
                
                <h5>Required Metadata Columns:</h5>
                <ul>
                  <li><strong>Sample Name:</strong> Unique identifier for each sample (e.g., Sample_1, Control_Rep1)</li>
                  <li><strong>Sample Group:</strong> Experimental condition or group (e.g., Control, Treatment, Resistant)</li>
                </ul>
                
                <h5>Optional Metadata Columns:</h5>
                <ul>
                  <li><strong>Tumour ID:</strong> For paired sample analysis</li>
                  <li><strong>Weight (mg):</strong> Sample weight for QC purposes</li>
                </ul>
                
                <h5>Lipid Naming Convention:</h5>
                <p>Lipids should be named following standard nomenclature for automatic classification:</p>
                <ul>
                  <li><code>PC 16:0_18:1</code> - Phosphatidylcholine with fatty acids 16:0 and 18:1</li>
                  <li><code>PE 18:0_20:4</code> - Phosphatidylethanolamine</li>
                  <li><code>TG 16:0_18:1_20:4</code> - Triacylglycerol with three fatty acids</li>
                  <li><code>SM 18:1</code> - Sphingomyelin</li>
                  <li><code>Cer 16:0</code> - Ceramide</li>
                  <li><code>LPC 18:2</code> - Lysophosphatidylcholine</li>
                </ul>
                
                <hr>
                
                <h4><i class='fa fa-tags'></i> Custom Classification File Format</h4>
                <p>You can upload a custom classification file to override automatic lipid classification. 
                The file must have a <code>Lipid</code> column followed by your classification columns.</p>
                
                <h5>Example Classification File:</h5>
                <div style='overflow-x: auto;'>
                <table class='table table-bordered table-striped' style='font-size: 12px;'>
                  <thead style='background-color: #00a65a; color: white;'>
                    <tr>
                      <th>Lipid</th>
                      <th>LipidClass</th>
                      <th>LipidType</th>
                      <th>Saturation</th>
                      <th>BiologicalRole</th>
                    </tr>
                  </thead>
                  <tbody>
                    <tr><td>PC 16:0_18:1</td><td>Glycerophospholipids</td><td>Phosphatidylcholine</td><td>MUFA</td><td>Membrane</td></tr>
                    <tr><td>PC 18:0_20:4</td><td>Glycerophospholipids</td><td>Phosphatidylcholine</td><td>PUFA</td><td>Membrane</td></tr>
                    <tr><td>PE 16:0_22:6</td><td>Glycerophospholipids</td><td>Phosphatidylethanolamine</td><td>PUFA</td><td>Membrane</td></tr>
                    <tr><td>Cer 16:0</td><td>Sphingolipids</td><td>Ceramide</td><td>SFA</td><td>Signaling</td></tr>
                    <tr><td>TG 16:0_18:1_18:1</td><td>Glycerolipids</td><td>Triacylglycerol</td><td>MUFA</td><td>Storage</td></tr>
                  </tbody>
                </table>
                </div>
                <p><small><strong>Note:</strong> You can add any number of classification columns (e.g., OmegaClass, Pathway, etc.)</small></p>
                
                <hr>
                
                <h4><i class='fa fa-project-diagram'></i> Custom Enrichment Sets File Format</h4>
                <p>Define custom lipid sets for enrichment analysis. Each lipid can belong to multiple sets.</p>
                
                <h5>Example Custom Sets File:</h5>
                <div style='overflow-x: auto;'>
                <table class='table table-bordered table-striped' style='font-size: 12px;'>
                  <thead style='background-color: #f39c12; color: white;'>
                    <tr>
                      <th>Lipid</th>
                      <th>Set_Name</th>
                    </tr>
                  </thead>
                  <tbody>
                    <tr><td>PC 18:0_20:4</td><td>Inflammatory_Signaling_AA</td></tr>
                    <tr><td>PE 18:0_20:4</td><td>Inflammatory_Signaling_AA</td></tr>
                    <tr><td>LPC 20:4</td><td>Inflammatory_Signaling_AA</td></tr>
                    <tr><td>PC 16:0_22:6</td><td>Omega3_DHA_Pathway</td></tr>
                    <tr><td>PE 16:0_22:6</td><td>Omega3_DHA_Pathway</td></tr>
                    <tr><td>Cer 16:0</td><td>Apoptosis_Ceramide</td></tr>
                    <tr><td>Cer 18:0</td><td>Apoptosis_Ceramide</td></tr>
                    <tr><td>SM 16:0</td><td>Membrane_Integrity</td></tr>
                    <tr><td>PC 16:0_18:1</td><td>Membrane_Integrity</td></tr>
                    <tr><td>TG 16:0_18:1_18:1</td><td>Energy_Storage</td></tr>
                  </tbody>
                </table>
                </div>
                <p><small><strong>Tip:</strong> A lipid can appear in multiple rows with different Set_Names to belong to multiple pathways.</small></p>
                
                <hr>
                
                <h4><i class='fa fa-cogs'></i> Key Features</h4>
                <div class='row'>
                  <div class='col-md-6'>
                    <h5>Normalization Methods:</h5>
                    <ul>
                      <li>TIC (Total Ion Current)</li>
                      <li>PQN (Probabilistic Quotient Normalization)</li>
                      <li>Quantile Normalization</li>
                      <li>VSN (Variance Stabilizing Normalization)</li>
                      <li>Log2/Log10 Transformation</li>
                    </ul>
                  </div>
                  <div class='col-md-6'>
                    <h5>Statistical Analysis:</h5>
                    <ul>
                      <li>limma (Linear Models for Microarray Data)</li>
                      <li>EdgeR (for count-like data)</li>
                      <li>Multiple contrast comparisons</li>
                      <li>FDR-corrected p-values</li>
                    </ul>
                  </div>
                </div>
                
                <hr>
                
                <h4><i class='fa fa-play-circle'></i> Getting Started</h4>
                <p>To begin your analysis:</p>
                <ol>
                  <li>Go to the <strong>'Data Upload'</strong> tab</li>
                  <li>Upload your CSV file OR click <strong>'Load Example Data'</strong> to explore the tool</li>
                  <li>Follow the workflow through each tab</li>
                </ol>
                
                <div class='alert alert-info'>
                  <i class='fa fa-lightbulb'></i> <strong>Tip:</strong> Use the example dataset first to familiarize yourself with the tool before analyzing your own data.
                </div>
              ")
            )
          )
        ),
        
        # Data Upload Tab
        shinydashboard::tabItem(tabName = "upload",
                                shiny::fluidRow(
                                  shinydashboard::box(
                                    title = "Upload Lipidomics Data", 
                                    status = "primary", 
                                    solidHeader = TRUE, 
                                    width = 6,
                                    shiny::fileInput("file", "Choose CSV File", accept = c(".csv")),
                                    shiny::checkboxGroupInput("metadata_cols", "Select Metadata Columns:",
                                                              choices = c("Sample Name", "Sample Group", "Tumour ID", "Weight (mg)"),
                                                              selected = c("Sample Name", "Sample Group")),
                                    shiny::actionButton("load_data", "Load Data", class = "btn-primary"),
                                    shiny::br(), shiny::br(),
                                    shiny::actionButton("load_example", "Load Example Data", class = "btn-info")
                                  ),
                                  shinydashboard::box(
                                    title = "Data Summary", 
                                    status = "info", 
                                    solidHeader = TRUE, 
                                    width = 6,
                                    shiny::verbatimTextOutput("data_summary"),
                                    DT::dataTableOutput("data_preview")
                                  )
                                )
        ),
        
        # Classification Tab
        shinydashboard::tabItem(tabName = "classification",
                                shiny::fluidRow(
                                  shinydashboard::box(
                                    title = "Lipid Classification",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = 6,
                                    shiny::h4("Automatic Classification"),
                                    shiny::p("Lipids are automatically classified based on their names."),
                                    shiny::actionLink("info_classification", "", icon = shiny::icon("info-circle")),
                                    shiny::downloadButton("download_classification", "Download Classification", class = "btn-success"),
                                    shiny::hr(),
                                    shiny::h4("Upload Custom Classification"),
                                    shiny::fileInput("custom_classification_file", "Choose CSV File",
                                                     accept = c(".csv")),
                                    shiny::helpText("File must have 'Lipid' column + classification columns"),
                                    shiny::actionButton("load_custom_classification", "Load Custom Classification", 
                                                        class = "btn-primary")
                                  ),
                                  shinydashboard::box(
                                    title = "Current Classification",
                                    status = "info",
                                    solidHeader = TRUE,
                                    width = 6,
                                    DT::dataTableOutput("classification_table")
                                  )
                                )
        ),
        
        # Raw Data Visualization Tab
        shinydashboard::tabItem(tabName = "raw_viz",
                                shiny::fluidRow(
                                  shinydashboard::box(
                                    title = "Raw Data Visualization Options", status = "primary", solidHeader = TRUE, width = 4,
                                    shiny::radioButtons("view_mode", "View Mode:",
                                                        choices = c("By Sample" = "sample", "By Lipid" = "lipid"),
                                                        selected = "sample"),
                                    shiny::selectInput("plot_type", "Plot Type:",
                                                       choices = c("Boxplot" = "boxplot","Violin"="violin", "Density" = "density", "Histogram" = "histogram")),
                                    shiny::conditionalPanel(
                                      condition = "input.view_mode == \"lipid\"",
                                      shiny::numericInput("top_n_lipids", "Top N Variable Lipids:", value = 30, min = 5, max = 100)
                                    ),
                                    shiny::actionButton("create_raw_plot", "Create Plot", class = "btn-primary"),
                                    shiny::br(), shiny::br(),
                                    shiny::selectInput("img_format_raw", "Image format:", c("PNG"="png","PDF"="pdf"), selected="png"),
                                    shiny::downloadButton("download_raw_plot", "Download Plot", class = "btn-success")
                                  ),
                                  shinydashboard::box(
                                    title = "Raw Data Visualization", status = "info", solidHeader = TRUE, width = 8,
                                    plotly::plotlyOutput("raw_plot", height = "500px")
                                  )
                                )
        ),
        
        # Normalization Tab - FIXED: Added format selector for pipeline comparison
        shinydashboard::tabItem(tabName = "normalization",
                                shiny::fluidRow(
                                  shinydashboard::box(
                                    title = "Normalization Pipeline Builder", status = "primary", solidHeader = TRUE, width = 4,
                                    shiny::h4("Pipeline 1:"),
                                    shiny::checkboxGroupInput("norm_methods_1", "Select Methods (in order):",
                                                              choices = get_normalization_methods(),
                                                              selected = c("TIC", "Log2")),
                                    shiny::h4("Pipeline 2 (for comparison):"),
                                    shiny::checkboxGroupInput("norm_methods_2", "Select Methods (in order):",
                                                              choices = get_normalization_methods(),
                                                              selected = c("PQN")),
                                    shiny::actionButton("compare_pipelines", "Compare Pipelines", class = "btn-info"),
                                    shiny::br(), shiny::br(),
                                    shiny::selectInput("chosen_pipeline", "Choose Pipeline to Apply:",
                                                       choices = c("Pipeline 1" = "1", "Pipeline 2" = "2")),
                                    shiny::actionButton("apply_normalization", "Apply Selected Pipeline", class = "btn-primary")
                                  ),
                                  shinydashboard::box(
                                    title = "Pipeline Comparison", status = "info", solidHeader = TRUE, width = 8,
                                    shiny::plotOutput("pipeline_comparison", height = "600px"),
                                    shiny::br(),
                                    shiny::selectInput("img_format_pipeline", "Image format:", 
                                                       c("PNG"="png","PDF"="pdf"), selected="png"),
                                    shiny::downloadButton("download_pipeline_comparison", "Download Comparison", class = "btn-success")
                                  )
                                )
        ),
        
        # Normalized Data Visualization Tab
        shinydashboard::tabItem(tabName = "norm_viz",
                                shiny::fluidRow(
                                  shinydashboard::box(
                                    title = "Normalized Data Visualization",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = 4,
                                    shiny::selectInput("norm_plot_type", "Plot Type:",
                                                       choices = c("Boxplot" = "boxplot", 
                                                                   "Violin" = "violin",
                                                                   "Density" = "density", 
                                                                   "PCA" = "pca", 
                                                                   "PLS-DA" = "plsda")),
                                    shiny::conditionalPanel(
                                      condition = "input.norm_plot_type == 'pca' || input.norm_plot_type == 'plsda'",
                                      shiny::selectInput("group_column", "Group Column:", choices = NULL),
                                      shiny::checkboxGroupInput("groups_included", "Include Groups:",
                                                                choices = NULL),
                                      shiny::radioButtons("ellipse_type", "Ellipse Type:",
                                                          choices = c("None" = "none",
                                                                      "Confidence Ellipse" = "confidence",
                                                                      "Visual Circle" = "visual"),
                                                          selected = "none"),
                                      shiny::actionLink("info_ellipses", "", icon = shiny::icon("info-circle"))
                                    ),
                                    shiny::actionButton("create_norm_plot", "Create Plot", 
                                                        class = "btn-primary"),
                                    shiny::br(), shiny::br(),
                                    shiny::selectInput("img_format_norm", "Image format:", 
                                                       c("PNG"="png","PDF"="pdf"), selected="png"),
                                    shiny::downloadButton("download_norm_plot", "Download Plot",
                                                          class = "btn-success")
                                  ),
                                  shinydashboard::box(
                                    title = "Normalized Data Plot",
                                    status = "info",
                                    solidHeader = TRUE,
                                    width = 8,
                                    plotly::plotlyOutput("norm_plot", height = "500px")
                                  )
                                )
        ),
        
        # Lipid Expression Tab
        shinydashboard::tabItem(tabName = "lipid_expression",
                                shiny::fluidRow(
                                  shinydashboard::box(
                                    title = "Lipid Expression Settings",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = 4,
                                    shiny::selectizeInput("selected_lipids", "Select Lipids:",
                                                          choices = NULL,
                                                          multiple = TRUE,
                                                          options = list(maxItems = 10)),
                                    shiny::radioButtons("expression_selection_mode", "Select by:",
                                                        choices = c("Samples" = "samples", "Groups" = "groups"),
                                                        selected = "samples"),
                                    shiny::conditionalPanel(
                                      condition = "input.expression_selection_mode == 'samples'",
                                      shiny::checkboxGroupInput("selected_samples", "Select Samples:",
                                                                choices = NULL)
                                    ),
                                    shiny::conditionalPanel(
                                      condition = "input.expression_selection_mode == 'groups'",
                                      shiny::checkboxGroupInput("selected_expression_groups", "Select Groups:",
                                                                choices = NULL)
                                    ),
                                    shiny::radioButtons("expression_data_type", "Data Type:",
                                                        choices = c("Raw" = "raw", "Normalized" = "normalized"),
                                                        selected = "normalized"),
                                    shiny::actionButton("create_expression_plot", "Create Plots", 
                                                        class = "btn-primary"),
                                    shiny::br(), shiny::br(),
                                    shiny::selectInput("img_format_expression", "Image format:", 
                                                       c("PNG"="png","PDF"="pdf"), selected="png"),
                                    shiny::downloadButton("download_expression_plots", "Download Plots",
                                                          class = "btn-success")
                                  ),
                                  shinydashboard::box(
                                    title = "Lipid Expression Plots",
                                    status = "info",
                                    solidHeader = TRUE,
                                    width = 8,
                                    shiny::uiOutput("expression_plots_ui")
                                  )
                                )
        ),
        
        # Differential Analysis Tab
        shinydashboard::tabItem(tabName = "diff_analysis",
                                shiny::fluidRow(
                                  shinydashboard::box(
                                    title = "Differential Analysis Settings",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = 4,
                                    shiny::selectInput("group_col_diff", "Group Column:",
                                                       choices = NULL),
                                    shiny::hr(),
                                    shiny::h4("Analysis Method"),
                                    shiny::radioButtons("diff_method", "Select Method:",
                                                        choices = c("limma" = "limma", "EdgeR" = "edger"),
                                                        selected = "limma"),
                                    shiny::actionLink("info_methods", "", icon = shiny::icon("info-circle")),
                                    shiny::hr(),
                                    shiny::h4("Select Contrasts"),
                                    shiny::actionLink("info_contrasts", "", icon = shiny::icon("info-circle")),
                                    shiny::uiOutput("contrast_selection_ui"),
                                    shiny::hr(),
                                    shiny::actionButton("run_diff_analysis", "Run Analysis", 
                                                        class = "btn-primary")
                                  ),
                                  shinydashboard::box(
                                    title = "Analysis Summary",
                                    status = "info",
                                    solidHeader = TRUE,
                                    width = 8,
                                    shiny::verbatimTextOutput("diff_summary"),
                                    shiny::hr(),
                                    shiny::selectInput("contrast_display", "Select Contrast to Display:",
                                                       choices = NULL),
                                    DT::dataTableOutput("diff_results_table"),
                                    shiny::br(),
                                    shiny::downloadButton("download_current_table", "Download Current Table", class = "btn-info"),
                                    shiny::downloadButton("download_results", "Download All Results (Excel)", class = "btn-success")
                                  )
                                )
        ),
        
        # Results Visualization Tab 
        shinydashboard::tabItem(tabName = "results_viz",
                                shiny::fluidRow(
                                  shinydashboard::box(
                                    title = "Visualization Options", status = "primary", solidHeader = TRUE, width = 4,
                                    shiny::selectInput("contrast_select", "Select Contrast:", choices = NULL),
                                    shiny::selectInput("viz_type", "Visualization Type:",
                                                       choices = c("Volcano Plot" = "volcano", "Heatmap" = "heatmap")),
                                    shiny::conditionalPanel(
                                      condition = "input.viz_type == \"volcano\"",
                                      shiny::numericInput("logfc_threshold", "LogFC Threshold:", value = 1, min = 0, step = 0.1),
                                      shiny::numericInput("pval_threshold", "P-value Threshold:", value = 0.05, min = 0, max = 1, step = 0.01),
                                      shiny::numericInput("top_labels", "Top N Labels:", value = 15, min = 0, max = 50),
                                      shiny::checkboxInput("color_by_class", "Color Significant by Classification", value = FALSE),
                                      shiny::conditionalPanel(
                                        condition = "input.color_by_class",
                                        shiny::selectInput("color_column", "Color by:",
                                                           choices = c("LipidGroup", "LipidType", "Saturation"))
                                      )
                                    ),
                                    shiny::conditionalPanel(
                                      condition = "input.viz_type == \"heatmap\"",
                                      shiny::numericInput("heatmap_top_n", "Top N Features:", value = 50, min = 10, max = 200)
                                    ),
                                    shiny::actionButton("create_viz", "Create Visualization", class = "btn-primary"),
                                    shiny::br(), shiny::br(),
                                    shiny::selectInput("img_format_results", "Image format:", c("PNG"="png","PDF"="pdf"), selected = "png"),
                                    shiny::downloadButton("download_viz", "Download Visualization", class = "btn-success")
                                  ),
                                  shinydashboard::box(
                                    title = "Results Visualization", status = "info", solidHeader = TRUE, width = 8,
                                    shiny::plotOutput("results_plot", height = "600px")
                                  )
                                )
        ),
        
        # Enrichment Analysis Tab 
        shinydashboard::tabItem(tabName = "enrichment",
                                shiny::fluidRow(
                                  shinydashboard::box(
                                    title = "Enrichment Analysis Settings",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = 4,
                                    shiny::selectInput("enrichment_contrast_select", "Select Contrast:",
                                                       choices = NULL),
                                    shiny::hr(),
                                    shiny::h4("Custom Enrichment Sets"),
                                    shiny::fileInput("custom_enrichment_file", "Upload Custom Sets CSV",
                                                     accept = c(".csv")),
                                    shiny::helpText("CSV with columns: Lipid, Set_Name"),
                                    shiny::actionLink("info_custom_sets", "", icon = shiny::icon("info-circle")),
                                    shiny::actionButton("load_custom_sets", "Load Custom Sets",
                                                        class = "btn-primary"),
                                    shiny::hr(),
                                    shiny::numericInput("min_set_size", "Min Set Size:", value = 5, min = 2),
                                    shiny::numericInput("max_set_size", "Max Set Size:", value = 500, min = 10),
                                    shiny::actionButton("run_enrichment", "Run Enrichment",
                                                        class = "btn-primary"),
                                    shiny::br(), shiny::br(),
                                    shiny::helpText("Note: If you see 'fgsea database corrupt' error, reinstall fgsea: install.packages('fgsea')")
                                  ),
                                  shinydashboard::box(
                                    title = "Enrichment Results",
                                    status = "info",
                                    solidHeader = TRUE,
                                    width = 8,
                                    shiny::selectInput("enrichment_contrast", "Display Contrast:", choices = NULL),
                                    shiny::selectInput("enrichment_type", "Enrichment Type:",
                                                       choices = c("Saturation" = "Saturation", 
                                                                   "Lipid Group" = "LipidGroup", 
                                                                   "Lipid Type" = "LipidType")),
                                    DT::dataTableOutput("enrichment_results_table"),
                                    shiny::downloadButton("download_enrichment", "Download Results",
                                                          class = "btn-success")
                                  )
                                )
        ),
        
        # Enrichment Visualization Tab
        shinydashboard::tabItem(tabName = "enrichment_viz",
                                shiny::fluidRow(
                                  shinydashboard::box(
                                    title = "Enrichment Visualization Options", status = "primary", solidHeader = TRUE, width = 4,
                                    shiny::selectInput("enrichment_viz_contrast", "Select Contrast:", choices = NULL),
                                    shiny::selectInput("enrichment_viz_type", "Enrichment Type:",
                                                       choices = c("Saturation" = "Saturation", 
                                                                   "Lipid Group" = "LipidGroup", 
                                                                   "Lipid Type" = "LipidType")),
                                    shiny::selectInput("enrichment_plot_type", "Plot Type:",
                                                       choices = c("Dot Plot" = "dotplot", "Bar Plot" = "barplot")),
                                    shiny::numericInput("max_pathways", "Max Pathways to Show:", value = 15, min = 5, max = 30),
                                    shiny::actionButton("create_enrichment_viz", "Create Visualization", class = "btn-primary"),
                                    shiny::br(), shiny::br(),
                                    shiny::selectInput("img_format_enrich", "Image format:", c("PNG"="png","PDF"="pdf"), selected = "png"),
                                    shiny::downloadButton("download_enrichment_viz", "Download Plot", class = "btn-success")
                                  ),
                                  shinydashboard::box(
                                    title = "Enrichment Visualization", status = "info", solidHeader = TRUE, width = 8,
                                    shiny::plotOutput("enrichment_plot", height = "600px")
                                  )
                                )
        ),
        
        # Report Generation Tab
        shinydashboard::tabItem(tabName = "report",
                                shiny::fluidRow(
                                  shinydashboard::box(
                                    title = "Generate Analysis Report",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = 6,
                                    shiny::h4("Report Options"),
                                    shiny::textInput("report_title", "Report Title:", 
                                                     value = "Lipidomics Analysis Report"),
                                    shiny::textInput("report_author", "Author:", value = ""),
                                    shiny::hr(),
                                    shiny::h4("Include Sections"),
                                    shiny::checkboxGroupInput("report_sections", "Select sections to include:",
                                                              choices = c("Data Summary" = "data_summary",
                                                                          "Normalization" = "normalization",
                                                                          "Differential Analysis" = "diff_analysis",
                                                                          "Enrichment Analysis" = "enrichment"),
                                                              selected = c("data_summary", "normalization", 
                                                                           "diff_analysis", "enrichment")),
                                    shiny::hr(),
                                    shiny::h4("Output Format"),
                                    shiny::radioButtons("report_format", "Choose format:",
                                                        choices = c("HTML" = "html", "PDF" = "pdf"),
                                                        selected = "html"),
                                    shiny::helpText("Note: PDF requires LaTeX. If PDF fails, use HTML format."),
                                    shiny::br(),
                                    shiny::downloadButton("download_report", "Generate & Download Report", class = "btn-primary btn-lg")
                                  ),
                                  shinydashboard::box(
                                    title = "Report Preview",
                                    status = "info",
                                    solidHeader = TRUE,
                                    width = 6,
                                    shiny::h4("Current Analysis Status"),
                                    shiny::verbatimTextOutput("report_status"),
                                    shiny::hr(),
                                    shiny::h4("Report Contents"),
                                    shiny::HTML("
                                      <p>The generated report will include:</p>
                                      <ul>
                                        <li><strong>Data Summary:</strong> Sample counts, feature counts, group distribution</li>
                                        <li><strong>Normalization:</strong> Methods applied and comparison</li>
                                        <li><strong>Differential Analysis:</strong> Summary statistics and significant features per contrast</li>
                                        <li><strong>Enrichment:</strong> Enriched lipid classes and pathways</li>
                                      </ul>
                                    ")
                                  )
                                )
        )
      )
    )
  )
  # Define Server with Enhanced Error Handling
  server <- function(input, output, session) {
    
    # Global error handling
    options(shiny.sanitize.errors = TRUE)
    
    # Reactive values to store data
    values <- shiny::reactiveValues(
      data = NULL,
      metadata = NULL,
      numeric_data = NULL,
      normalized_data = NULL,
      raw_data = NULL,
      
      # Classification
      classification = NULL,
      custom_classification = NULL,
      use_custom_classification = FALSE,
      classification_data = NULL,
      
      # Differential analysis
      diff_results = NULL,
      selected_contrasts = NULL,
      available_contrasts = NULL,
      
      # Custom enrichment
      custom_enrichment_sets = NULL,
      enrichment_results = NULL,
      
      # Visualization
      current_plot = NULL,
      current_raw_plot = NULL,
      current_norm_plot = NULL,
      current_results_plot = NULL,
      current_enrichment_plot = NULL,
      current_expression_plots = NULL,
      current_pipeline_plots = NULL
    )
    
    # Error handling function
    show_error <- function(e, context = "Operation") {
      error_msg <- paste(context, "failed:", e$message)
      shiny::showNotification(error_msg, type = "error", duration = 10)
    }
    
    # Load example data
    shiny::observeEvent(input$load_example, {
      tryCatch({
        example_data <- generate_example_data()
        values$raw_data <- load_lipidomics_data_from_df(example_data, input$metadata_cols)
        
        # Set rownames for numeric data
        if (!is.null(values$raw_data$metadata$`Sample Name`)) {
          rownames(values$raw_data$numeric_data) <- values$raw_data$metadata$`Sample Name`
          rownames(values$raw_data$metadata) <- values$raw_data$metadata$`Sample Name`
        }
        
        # Generate classification data
        lipid_names <- colnames(values$raw_data$numeric_data)
        values$classification <- classify_lipids(lipid_names)
        values$classification_data <- values$classification
        
        # Update UI choices
        group_choices <- names(values$raw_data$metadata)
        shiny::updateSelectInput(session, "group_column", choices = group_choices, selected = "Sample Group")
        shiny::updateSelectInput(session, "group_col_diff", choices = group_choices, selected = "Sample Group")
        
        # Update sample and group choices for expression plots
        if ("Sample Name" %in% names(values$raw_data$metadata)) {
          sample_choices <- values$raw_data$metadata$`Sample Name`
          shiny::updateCheckboxGroupInput(session, "selected_samples", 
                                          choices = sample_choices, selected = sample_choices)
        }
        if ("Sample Group" %in% names(values$raw_data$metadata)) {
          group_choices_expr <- unique(values$raw_data$metadata$`Sample Group`)
          shiny::updateCheckboxGroupInput(session, "selected_expression_groups", 
                                          choices = group_choices_expr, selected = group_choices_expr)
        }
        
        output$data_summary <- shiny::renderText({
          paste("Example data loaded successfully!\n",
                "Samples:", nrow(values$raw_data$data), "\n",
                "Features:", ncol(values$raw_data$numeric_data), "\n",
                "Groups found:", paste(unique(values$raw_data$metadata$`Sample Group`), collapse = ", "))
        })
        
        output$data_preview <- DT::renderDataTable({
          DT::datatable(head(values$raw_data$data), 
                        options = list(scrollX = TRUE, pageLength = 5))
        })
        
        shiny::showNotification("Example data loaded successfully!")
        
      }, error = function(e) {
        show_error(e, "Loading example data")
      })
    })
    
    # Data loading from file
    shiny::observeEvent(input$load_data, {
      shiny::req(input$file)
      
      tryCatch({
        file_path <- input$file$datapath
        loaded_data <- load_lipidomics_data(file_path, metadata_columns = input$metadata_cols)
        
        values$raw_data <- list(
          data = loaded_data$data,
          metadata = loaded_data$metadata,
          numeric_data = as.matrix(loaded_data$numeric_data)
        )
        
        # Set rownames
        if ("Sample Name" %in% names(values$raw_data$metadata)) {
          rownames(values$raw_data$numeric_data) <- values$raw_data$metadata$`Sample Name`
          rownames(values$raw_data$metadata) <- values$raw_data$metadata$`Sample Name`
        }
        
        # Auto-classify lipids
        lipid_names <- colnames(values$raw_data$numeric_data)
        values$classification <- classify_lipids(lipid_names)
        values$classification_data <- values$classification
        values$use_custom_classification <- FALSE
        
        # Update UI choices
        group_choices <- names(values$raw_data$metadata)
        shiny::updateSelectInput(session, "group_column", choices = group_choices, selected = group_choices[1])
        shiny::updateSelectInput(session, "group_col_diff", choices = group_choices, selected = group_choices[1])
        
        # Update sample and group choices for expression plots
        if ("Sample Name" %in% names(values$raw_data$metadata)) {
          sample_choices <- values$raw_data$metadata$`Sample Name`
          shiny::updateCheckboxGroupInput(session, "selected_samples", 
                                          choices = sample_choices, selected = sample_choices)
        }
        
        output$data_summary <- shiny::renderText({
          paste("Data loaded successfully!\n",
                "Samples:", nrow(values$raw_data$data), "\n",
                "Features:", ncol(values$raw_data$numeric_data), "\n",
                "Metadata columns:", paste(group_choices, collapse = ", "))
        })
        
        output$data_preview <- DT::renderDataTable({
          DT::datatable(head(values$raw_data$data), 
                        options = list(scrollX = TRUE, pageLength = 5))
        })
        
        shiny::showNotification("Data loaded successfully!", type = "message")
        
      }, error = function(e) {
        shiny::showNotification(paste("Error loading data:", e$message), type = "error")
      })
    })
    
    # Display classification table
    output$classification_table <- DT::renderDataTable({
      if (values$use_custom_classification && !is.null(values$custom_classification)) {
        return(DT::datatable(values$custom_classification))
      } else if (!is.null(values$classification)) {
        return(DT::datatable(values$classification))
      } else {
        return(NULL)
      }
    })
    
    # Download classification
    output$download_classification <- shiny::downloadHandler(
      filename = function() paste0("lipid_classification_", Sys.Date(), ".csv"),
      content = function(file) {
        classification_to_download <- if (values$use_custom_classification) {
          values$custom_classification
        } else {
          values$classification
        }
        write.csv(classification_to_download, file, row.names = FALSE)
      }
    )
    
    # Load custom classification
    shiny::observeEvent(input$load_custom_classification, {
      shiny::req(input$custom_classification_file)
      
      tryCatch({
        file_path <- input$custom_classification_file$datapath
        values$custom_classification <- load_custom_classification(file_path)
        values$classification_data <- values$custom_classification
        values$use_custom_classification <- TRUE
        shiny::showNotification("Custom classification loaded!", type = "message")
      }, error = function(e) {
        shiny::showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # Info modal for classification
    shiny::observeEvent(input$info_classification, {
      shiny::showModal(shiny::modalDialog(
        title = "Lipid Classification",
        shiny::HTML("
      <p><strong>Automatic Classification:</strong> Lipids are classified based on their names into:</p>
      <ul>
        <li><strong>LipidGroup:</strong> Main category (e.g., Glycerophospholipids, Sphingolipids)</li>
        <li><strong>LipidType:</strong> Specific class (e.g., Phosphatidylcholine, Ceramide)</li>
        <li><strong>Saturation:</strong> Fatty acid saturation (SFA, MUFA, PUFA)</li>
      </ul>
      <p><strong>Custom Classification:</strong> You can provide your own classification with flexible hierarchy.</p>
      <p>CSV format: First column = 'Lipid', followed by any number of classification columns.</p>
    "),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      ))
    })
    
    # Update lipid choices
    shiny::observe({
      shiny::req(values$raw_data)
      if (!is.null(values$raw_data$numeric_data)) {
        lipid_choices <- colnames(values$raw_data$numeric_data)
        shiny::updateSelectizeInput(session, "selected_lipids", choices = lipid_choices)
      }
    })
    
    # Update sample choices for expression plots
    shiny::observe({
      shiny::req(values$raw_data)
      if (!is.null(values$raw_data$metadata)) {
        if ("Sample Name" %in% names(values$raw_data$metadata)) {
          sample_choices <- values$raw_data$metadata$`Sample Name`
          shiny::updateCheckboxGroupInput(session, "selected_samples", 
                                          choices = sample_choices,
                                          selected = sample_choices)
        }
        # Update group choices for expression
        group_col <- if ("Sample Group" %in% names(values$raw_data$metadata)) {
          "Sample Group"
        } else {
          names(values$raw_data$metadata)[1]
        }
        if (!is.null(values$raw_data$metadata[[group_col]])) {
          group_choices <- unique(values$raw_data$metadata[[group_col]])
          shiny::updateCheckboxGroupInput(session, "selected_expression_groups", 
                                          choices = group_choices,
                                          selected = group_choices)
        }
      }
    })
    
    # Create expression plots
    shiny::observeEvent(input$create_expression_plot, {
      shiny::req(values$raw_data, input$selected_lipids)
      
      tryCatch({
        # Get data from correct source
        if (input$expression_data_type == "normalized" && !is.null(values$normalized_data)) {
          data_to_use <- values$normalized_data$numeric_data
          metadata_to_use <- values$normalized_data$metadata
        } else {
          data_to_use <- values$raw_data$numeric_data
          metadata_to_use <- values$raw_data$metadata
        }
        
        # Determine selection parameters
        selected_samples <- NULL
        selected_groups <- NULL
        
        if (input$expression_selection_mode == "samples") {
          selected_samples <- input$selected_samples
        } else {
          selected_groups <- input$selected_expression_groups
        }
        
        # Get group column
        group_col <- if ("Sample Group" %in% names(metadata_to_use)) {
          "Sample Group"
        } else {
          names(metadata_to_use)[1]
        }
        
        plots <- create_lipid_expression_barplot(
          data_matrix = data_to_use,
          metadata = metadata_to_use,
          selected_lipids = input$selected_lipids,
          selected_samples = selected_samples,
          selected_groups = selected_groups,
          group_column = group_col,
          data_type = input$expression_data_type
        )
        
        values$current_expression_plots <- plots
        shiny::showNotification("Plots created!", type = "message")
        
      }, error = function(e) {
        shiny::showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # Render expression plots
    output$expression_plots_ui <- shiny::renderUI({
      shiny::req(values$current_expression_plots)
      
      if (is.list(values$current_expression_plots) && !inherits(values$current_expression_plots, "ggplot")) {
        # Multiple plots
        plot_outputs <- lapply(names(values$current_expression_plots), function(lipid) {
          shinydashboard::box(
            title = lipid,
            width = 12,
            shiny::plotOutput(paste0("expr_plot_", make.names(lipid)), height = "400px")
          )
        })
        do.call(shiny::tagList, plot_outputs)
      } else {
        # Single plot
        shiny::plotOutput("expr_plot_single", height = "400px")
      }
    })
    
    # Render individual plots
    shiny::observe({
      if (is.list(values$current_expression_plots) && !inherits(values$current_expression_plots, "ggplot")) {
        lapply(names(values$current_expression_plots), function(lipid) {
          local({
            my_lipid <- lipid
            output_name <- paste0("expr_plot_", make.names(my_lipid))
            output[[output_name]] <- shiny::renderPlot({
              values$current_expression_plots[[my_lipid]]
            })
          })
        })
      } else if (!is.null(values$current_expression_plots)) {
        output$expr_plot_single <- shiny::renderPlot({
          values$current_expression_plots
        })
      }
    })
    
    # Observer to update group checkboxes when group column changes
    shiny::observe({
      req(values$raw_data, input$group_column)
      
      if (input$group_column %in% names(values$raw_data$metadata)) {
        unique_groups <- unique(values$raw_data$metadata[[input$group_column]])
        unique_groups <- unique_groups[!is.na(unique_groups)]
        
        shiny::updateCheckboxGroupInput(
          session, 
          "groups_included", 
          choices = unique_groups, 
          selected = unique_groups
        )
      }
    })
    
    # Raw data visualization
    shiny::observeEvent(input$create_raw_plot, {
      req(values$raw_data)
      
      tryCatch({
        top_n <- if (input$view_mode == "lipid") input$top_n_lipids else NULL
        
        plot <- visualize_raw_data_improved(
          values$raw_data, 
          input$plot_type, 
          input$view_mode, 
          top_n
        )
        
        values$current_plot <- plot
        
        output$raw_plot <- plotly::renderPlotly({
          plotly::ggplotly(plot)
        })
        
      }, error = function(e) {
        show_error(e, "Raw data visualization")
      })
    })
    
    # Pipeline comparison
    shiny::observeEvent(input$compare_pipelines, {
      req(values$raw_data)
      
      tryCatch({
        # Apply both pipelines
        pipeline1_data <- apply_normalizations(values$raw_data$numeric_data, input$norm_methods_1)
        pipeline2_data <- apply_normalizations(values$raw_data$numeric_data, input$norm_methods_2)
        
        # Create comparison plots using fixed function
        p1 <- create_pipeline_plot(pipeline1_data, paste(input$norm_methods_1, collapse = " -> "))
        p2 <- create_pipeline_plot(pipeline2_data, paste(input$norm_methods_2, collapse = " -> "))
        
        # Store for download
        values$current_pipeline_plots <- list(p1 = p1, p2 = p2)
        
        output$pipeline_comparison <- shiny::renderPlot({
          gridExtra::grid.arrange(p1, p2, ncol = 1)
        })
        
        shiny::showNotification("Pipeline comparison complete!", type = "message")
        
      }, error = function(e) {
        show_error(e, "Pipeline comparison")
      })
    })
    
    # Apply normalization
    shiny::observeEvent(input$apply_normalization, {
      req(values$raw_data, input$chosen_pipeline)
      
      tryCatch({
        methods <- if (input$chosen_pipeline == "1") input$norm_methods_1 else input$norm_methods_2
        
        normalized_data <- apply_normalizations(values$raw_data$numeric_data, methods)
        
        # Preserve rownames
        if (!is.null(rownames(values$raw_data$numeric_data))) {
          rownames(normalized_data) <- rownames(values$raw_data$numeric_data)
        }
        
        values$normalized_data <- list(
          data = cbind(values$raw_data$metadata, normalized_data),
          metadata = values$raw_data$metadata,
          numeric_data = normalized_data
        )
        
        shiny::showNotification("Normalization applied successfully!")
        
      }, error = function(e) {
        show_error(e, "Applying normalization")
      })
    })
    
    # Normalized data visualization
    shiny::observeEvent(input$create_norm_plot, {
      req(values$normalized_data)
      
      tryCatch({
        # Filter by groups if specified
        if (!is.null(input$groups_included) && length(input$groups_included) > 0) {
          group_col <- input$group_column
          
          if (is.null(rownames(values$normalized_data$metadata))) {
            rownames(values$normalized_data$metadata) <- rownames(values$normalized_data$numeric_data)
          }
          
          selected_samples <- values$normalized_data$metadata[[group_col]] %in% input$groups_included
          
          filtered_data <- values$normalized_data$numeric_data[selected_samples, , drop = FALSE]
          filtered_metadata <- values$normalized_data$metadata[selected_samples, , drop = FALSE]
          
        } else {
          filtered_data <- values$normalized_data$numeric_data
          filtered_metadata <- values$normalized_data$metadata
        }
        
        if (input$norm_plot_type %in% c("boxplot", "violin", "density")) {
          data_list <- list(numeric_data = filtered_data, metadata = filtered_metadata)
          plot <- visualize_raw_data_improved(data_list, input$norm_plot_type, "sample")
          
          values$current_norm_plot <- plot
          
          output$norm_plot <- plotly::renderPlotly({
            plotly::ggplotly(plot)
          })
          
        } else if (input$norm_plot_type == "pca") {
          req(input$group_column)
          
          pca_results <- perform_pca(filtered_data, filtered_metadata, input$group_column)
          
          plot <- create_pca_plot_with_ellipses(
            pca_data = pca_results$pca_data,
            variance_explained = pca_results$variance_explained,
            ellipse_type = input$ellipse_type
          )
          
          values$current_norm_plot <- plot
          
          output$norm_plot <- plotly::renderPlotly({
            plotly::ggplotly(plot)
          })
          
        } else if (input$norm_plot_type == "plsda") {
          req(input$group_column)
          
          data_matrix <- as.matrix(filtered_data)
          
          plsda_results <- perform_plsda(data_matrix, filtered_metadata, input$group_column)
          
          plot <- create_plsda_plot_with_ellipses(
            plsda_data = plsda_results$scores_data,
            ellipse_type = input$ellipse_type
          )
          
          values$current_norm_plot <- plot
          
          output$norm_plot <- plotly::renderPlotly({
            plotly::ggplotly(plot)
          })
        }
        
      }, error = function(e) {
        show_error(e, "Creating normalized data plot")
      })
    })
    
    # Generate available contrasts
    shiny::observe({
      metadata_to_use <- if (!is.null(values$normalized_data)) {
        values$normalized_data$metadata
      } else if (!is.null(values$raw_data)) {
        values$raw_data$metadata
      } else {
        NULL
      }
      
      shiny::req(metadata_to_use, input$group_col_diff)
      
      if (input$group_col_diff %in% names(metadata_to_use)) {
        groups <- levels(factor(metadata_to_use[[input$group_col_diff]]))
        contrasts <- create_default_contrasts(groups)
        values$available_contrasts <- contrasts
      }
    })
    
    # Render contrast selection UI
    output$contrast_selection_ui <- shiny::renderUI({
      shiny::req(values$available_contrasts)
      shiny::checkboxGroupInput("selected_contrasts", "Choose Contrasts:",
                                choices = values$available_contrasts,
                                selected = values$available_contrasts)
    })
    
    # Info modal for methods
    shiny::observeEvent(input$info_methods, {
      shiny::showModal(shiny::modalDialog(
        title = "Differential Analysis Methods",
        shiny::HTML("
      <h4>limma</h4>
      <p>Linear Models for Microarray and Omics Data. Widely used, fast, robust for continuous data.</p>
      <p><strong>Best for:</strong> Normalized lipidomics data, moderate sample sizes.</p>
      
      <h4>EdgeR</h4>
      <p>Originally designed for RNA-seq, but adaptable to other omics data. Uses negative binomial models.</p>
      <p><strong>Best for:</strong> Count-like data, when you want different statistical assumptions.</p>
      
      <p><strong>Recommendation:</strong> Start with limma for lipidomics. Try EdgeR if you want to compare methods.</p>
    "),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      ))
    })
    
    # Info modal for contrasts
    shiny::observeEvent(input$info_contrasts, {
      shiny::showModal(shiny::modalDialog(
        title = "Contrast Selection",
        shiny::HTML("
      <p><strong>Contrasts</strong> define which groups you want to compare.</p>
      <p>Format: <code>GroupA - GroupB</code> tests for differences between these groups.</p>
      <p><strong>Tip:</strong> Select only the contrasts relevant to your research question to reduce multiple testing burden.</p>
    "),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      ))
    })
    # Run differential analysis - FIXED with proper summary output
    shiny::observeEvent(input$run_diff_analysis, {
      shiny::req(values$normalized_data, input$selected_contrasts)
      
      tryCatch({
        metadata_to_use <- if (!is.null(values$normalized_data$metadata)) {
          values$normalized_data$metadata
        } else {
          values$raw_data$metadata
        }
        
        results <- perform_differential_analysis(
          data_matrix = values$normalized_data$numeric_data,
          metadata = metadata_to_use,
          group_column = input$group_col_diff,
          contrasts_list = input$selected_contrasts,
          method = input$diff_method
        )
        
        values$diff_results <- results
        
        # Update contrast display selector
        contrast_choices <- names(results$results)
        shiny::updateSelectInput(session, "contrast_display", 
                                 choices = contrast_choices,
                                 selected = contrast_choices[1])
        shiny::updateSelectInput(session, "contrast_select", 
                                 choices = contrast_choices,
                                 selected = contrast_choices[1])
        shiny::updateSelectInput(session, "enrichment_contrast_select", 
                                 choices = contrast_choices,
                                 selected = contrast_choices[1])
        shiny::updateSelectInput(session, "enrichment_contrast", 
                                 choices = contrast_choices,
                                 selected = contrast_choices[1])
        shiny::updateSelectInput(session, "enrichment_viz_contrast", 
                                 choices = contrast_choices,
                                 selected = contrast_choices[1])
        
        shiny::showNotification(
          paste("Differential analysis completed using", input$diff_method), 
          type = "message"
        )
        
      }, error = function(e) {
        shiny::showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # Differential analysis summary output - FIXED terminology
    output$diff_summary <- shiny::renderText({
      if (is.null(values$diff_results)) {
        return("No differential analysis results available.\nPlease run the analysis first.")
      }
      
      # Create summary with FIXED terminology
      summary_text <- paste0(
        "=== Differential Analysis Summary ===\n\n",
        "Method: ", values$diff_results$method, "\n",
        "Number of contrasts: ", length(values$diff_results$results), "\n\n"
      )
      
      # Add summary for each contrast - FIXED: Increased/Decreased abundance
      for (contrast_name in names(values$diff_results$results)) {
        res <- values$diff_results$results[[contrast_name]]
        n_sig <- sum(res$adj.P.Val < 0.05, na.rm = TRUE)
        n_up <- sum(res$adj.P.Val < 0.05 & res$logFC > 0, na.rm = TRUE)
        n_down <- sum(res$adj.P.Val < 0.05 & res$logFC < 0, na.rm = TRUE)
        
        summary_text <- paste0(
          summary_text,
          "--- ", contrast_name, " ---\n",
          "Total features: ", nrow(res), "\n",
          "Significant (adj.P < 0.05): ", n_sig, "\n",
          "  Increased abundance: ", n_up, "\n",
          "  Decreased abundance: ", n_down, "\n\n"
        )
      }
      
      return(summary_text)
    })
    
    # Update diff results table when contrast changes
    output$diff_results_table <- DT::renderDataTable({
      req(values$diff_results, input$contrast_display)
      
      tryCatch({
        selected_results <- values$diff_results$results[[input$contrast_display]]
        
        if (is.null(selected_results)) {
          return(DT::datatable(data.frame(Message = "No results for selected contrast")))
        }
        
        # Ensure it's a data.frame
        if (!is.data.frame(selected_results)) {
          selected_results <- as.data.frame(selected_results)
        }
        
        # Add lipid names as first column if necessary
        if (!"Lipid" %in% colnames(selected_results)) {
          selected_results <- data.frame(
            Lipid = rownames(selected_results),
            selected_results,
            stringsAsFactors = FALSE
          )
        }
        
        # Round numeric values for display
        numeric_cols <- sapply(selected_results, is.numeric)
        selected_results[numeric_cols] <- lapply(selected_results[numeric_cols], round, 4)
        
        DT::datatable(
          selected_results,
          options = list(
            pageLength = 15,
            lengthMenu = c(10, 15, 25, 50, 100),
            scrollX = TRUE,
            scrollY = "400px",
            deferRender = TRUE,
            paging = TRUE,
            searching = TRUE
          ),
          rownames = FALSE,
          filter = 'top',
          selection = 'multiple'
        )
      }, error = function(e) {
        shiny::showNotification(paste("Table display error:", e$message), type = "error")
        DT::datatable(data.frame(Error = "Unable to display results"))
      })
    }, server = TRUE)
    
    # Results visualization - FIXED volcano plot coloring
    shiny::observeEvent(input$create_viz, {
      req(values$diff_results, input$contrast_select)
      
      tryCatch({
        selected_results <- values$diff_results$results[[input$contrast_select]]
        
        if (input$viz_type == "volcano") {
          classification <- if (input$color_by_class) values$classification_data else NULL
          color_by <- if (input$color_by_class) input$color_column else NULL
          
          # FIXED: Pass thresholds to color only significant lipids
          plot <- create_volcano_plot_labeled(
            selected_results,
            title = paste("Volcano Plot:", input$contrast_select),
            logfc_threshold = input$logfc_threshold,
            pval_threshold = input$pval_threshold,
            top_labels = input$top_labels,
            classification_data = classification,
            color_by = color_by
          )
          
          values$current_results_plot <- plot
          
          output$results_plot <- shiny::renderPlot({
            plot
          })
          
        } else if (input$viz_type == "heatmap") {
          # Get significant features
          sig_indices <- which(
            selected_results$adj.P.Val < 0.05 & 
              abs(selected_results$logFC) > 1
          )
          
          if (length(sig_indices) == 0) {
            output$results_plot <- shiny::renderPlot({
              ggplot2::ggplot() + 
                ggplot2::annotate("text", x = 0.5, y = 0.5, 
                                  label = "No significant features found\nfor heatmap", 
                                  size = 6) +
                ggplot2::theme_void()
            })
          } else {
            # Limit to requested number of features
            n_features <- min(length(sig_indices), input$heatmap_top_n)
            selected_indices <- sig_indices[1:n_features]
            sig_feature_names <- rownames(selected_results)[selected_indices]
            
            # Check features exist in data
            available_features <- intersect(sig_feature_names, colnames(values$normalized_data$numeric_data))
            
            if (length(available_features) == 0) {
              output$results_plot <- shiny::renderPlot({
                ggplot2::ggplot() + 
                  ggplot2::annotate("text", x = 0.5, y = 0.5, 
                                    label = "No significant features\navailable in data", 
                                    size = 6) +
                  ggplot2::theme_void()
              })
            } else {
              # Extract groups from the current contrast
              contrast_name <- input$contrast_select
              contrast_parts <- unlist(strsplit(contrast_name, " - "))
              
              if (length(contrast_parts) == 2) {
                group1 <- trimws(contrast_parts[1])
                group2 <- trimws(contrast_parts[2])
                groups_to_include <- c(group1, group2)
                
                group_column <- input$group_col_diff
                all_metadata <- values$normalized_data$metadata
                all_data <- values$normalized_data$numeric_data
                
                samples_in_groups <- all_metadata[[group_column]] %in% groups_to_include
                
                filtered_data <- all_data[samples_in_groups, available_features, drop = FALSE]
                filtered_metadata <- all_metadata[samples_in_groups, , drop = FALSE]
                
              } else {
                filtered_data <- values$normalized_data$numeric_data[, available_features, drop = FALSE]
                filtered_metadata <- values$normalized_data$metadata
              }
              
              # Create the heatmap
              heatmap <- create_heatmap_robust(
                t(filtered_data),
                filtered_metadata,
                input$group_col_diff,
                top_n = length(available_features),
                classification_data = values$classification_data,
                title = paste("Heatmap:", contrast_name)
              )
              
              values$current_results_plot <- heatmap
              
              output$results_plot <- shiny::renderPlot({
                if (inherits(heatmap, "pheatmap")) {
                  grid::grid.draw(heatmap$gtable)
                } else {
                  heatmap
                }
              })
            }
          }
        }
      }, error = function(e) {
        show_error(e, "Creating visualization")
      })
    })
    
    # Load custom enrichment sets
    shiny::observeEvent(input$load_custom_sets, {
      shiny::req(input$custom_enrichment_file)
      
      tryCatch({
        file_path <- input$custom_enrichment_file$datapath
        values$custom_enrichment_sets <- load_custom_enrichment_sets(file_path)
        shiny::showNotification("Custom enrichment sets loaded!", type = "message")
      }, error = function(e) {
        shiny::showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # Info modal for custom sets
    shiny::observeEvent(input$info_custom_sets, {
      shiny::showModal(shiny::modalDialog(
        title = "Custom Enrichment Sets",
        shiny::HTML("
          <p><strong>Format:</strong> CSV file with two columns:</p>
          <ul>
            <li><code>Lipid</code>: Lipid name (must match your data)</li>
            <li><code>Set_Name</code>: Name of the functional set/category</li>
          </ul>
          <p><strong>Example:</strong></p>
          <pre>
Lipid,Set_Name
PC 16:0_18:1,Membrane_Lipids
PE 18:0_20:4,Membrane_Lipids
TG 16:0_18:1_20:4,Storage_Lipids
          </pre>
          <p>Lipids can belong to multiple sets by appearing in multiple rows with different Set_Names.</p>
        "),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      ))
    })
    
    # Enrichment analysis with error handling for fgsea
    shiny::observeEvent(input$run_enrichment, {
      shiny::req(values$diff_results, values$classification)
      
      tryCatch({
        classification_to_use <- if (values$use_custom_classification) {
          values$custom_classification
        } else {
          values$classification
        }
        
        enrichment_results <- perform_enrichment_analysis(
          results_list = values$diff_results$results,
          classification_data = classification_to_use,
          min_set_size = input$min_set_size,
          max_set_size = input$max_set_size,
          custom_sets = values$custom_enrichment_sets
        )
        
        values$enrichment_results <- enrichment_results
        shiny::showNotification("Enrichment analysis completed!", type = "message")
        
      }, error = function(e) {
        # Check if it's the fgsea corrupt database error
        if (grepl("corrupt", e$message, ignore.case = TRUE) || grepl("fgsea", e$message, ignore.case = TRUE)) {
          shiny::showNotification(
            paste("fgsea package error. Try reinstalling: install.packages('fgsea')\n", e$message), 
            type = "error", duration = 15
          )
        } else {
          shiny::showNotification(paste("Error:", e$message), type = "error")
        }
      })
    })
    
    # Display enrichment results
    output$enrichment_results_table <- DT::renderDataTable({
      req(values$enrichment_results, input$enrichment_contrast, input$enrichment_type)
      
      tryCatch({
        if (input$enrichment_contrast %in% names(values$enrichment_results)) {
          contrast_results <- values$enrichment_results[[input$enrichment_contrast]]
          
          if (input$enrichment_type %in% names(contrast_results)) {
            enrichment_data <- contrast_results[[input$enrichment_type]]
            
            if (nrow(enrichment_data) > 0) {
              DT::datatable(
                enrichment_data,
                options = list(
                  pageLength = 10,
                  scrollX = TRUE,
                  searching = TRUE
                )
              )
            } else {
              DT::datatable(data.frame(Message = "No enrichment results for this type"))
            }
          } else {
            DT::datatable(data.frame(Message = "Enrichment type not available"))
          }
        } else {
          DT::datatable(data.frame(Message = "Contrast not found in results"))
        }
      }, error = function(e) {
        DT::datatable(data.frame(Error = paste("Display error:", e$message)))
      })
    })
    
    # Enrichment visualization
    shiny::observeEvent(input$create_enrichment_viz, {
      req(values$enrichment_results, input$enrichment_viz_contrast, input$enrichment_viz_type)
      
      tryCatch({
        if (input$enrichment_viz_contrast %in% names(values$enrichment_results)) {
          contrast_results <- values$enrichment_results[[input$enrichment_viz_contrast]]
          
          if (input$enrichment_viz_type %in% names(contrast_results)) {
            enrichment_data <- contrast_results[[input$enrichment_viz_type]]
            
            if (nrow(enrichment_data) == 0) {
              plot <- ggplot2::ggplot() + 
                ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No enrichment results available") +
                ggplot2::theme_void() +
                ggplot2::labs(title = paste("Enrichment Analysis:", input$enrichment_viz_contrast))
            } else {
              if (input$enrichment_plot_type == "dotplot") {
                plot <- create_enrichment_dotplot(
                  enrichment_data, 
                  title = paste("Enrichment Analysis:", input$enrichment_viz_contrast, "-", input$enrichment_viz_type),
                  max_pathways = input$max_pathways
                )
              } else {
                plot <- create_enrichment_barplot(
                  enrichment_data, 
                  title = paste("Enrichment Analysis:", input$enrichment_viz_contrast, "-", input$enrichment_viz_type),
                  max_pathways = input$max_pathways
                )
              }
            }
            
            values$current_enrichment_plot <- plot
            
            output$enrichment_plot <- shiny::renderPlot({
              plot
            })
          } else {
            output$enrichment_plot <- shiny::renderPlot({
              ggplot2::ggplot() + 
                ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Enrichment type not available") +
                ggplot2::theme_void()
            })
          }
        }
        
      }, error = function(e) {
        show_error(e, "Creating enrichment visualization")
      })
    })
    
    # Report status output
    output$report_status <- shiny::renderText({
      status_text <- "=== Analysis Status ===\n\n"
      
      # Data status
      if (!is.null(values$raw_data)) {
        status_text <- paste0(status_text, "Data loaded: ", 
                              nrow(values$raw_data$data), " samples, ",
                              ncol(values$raw_data$numeric_data), " features\n")
      } else {
        status_text <- paste0(status_text, "Data not loaded\n")
      }
      
      # Normalization status
      if (!is.null(values$normalized_data)) {
        status_text <- paste0(status_text, "Normalization applied\n")
      } else {
        status_text <- paste0(status_text, "Normalization not applied\n")
      }
      
      # Differential analysis status
      if (!is.null(values$diff_results)) {
        status_text <- paste0(status_text, "Differential analysis completed (",
                              length(values$diff_results$results), " contrasts)\n")
      } else {
        status_text <- paste0(status_text, "Differential analysis not run\n")
      }
      
      # Enrichment status
      if (!is.null(values$enrichment_results)) {
        status_text <- paste0(status_text, "Enrichment analysis completed\n")
      } else {
        status_text <- paste0(status_text, "Enrichment analysis not run\n")
      }
      
      return(status_text)
    })
    # ========== DOWNLOAD HANDLERS ==========
    
    output$download_data_preview <- shiny::downloadHandler(
      filename = function() paste0("data_preview_", Sys.Date(), ".csv"),
      content = function(file) {
        req(values$raw_data)
        write.csv(values$raw_data$data, file, row.names = FALSE)
      }
    )
    
    output$download_raw_plot <- shiny::downloadHandler(
      filename = function() {
        ext <- if (identical(input$img_format_raw, "pdf")) "pdf" else "png"
        paste0("raw_data_plot_", Sys.Date(), ".", ext)
      },
      content = function(file) {
        req(values$current_plot)
        tryCatch({
          if (identical(input$img_format_raw, "pdf")) {
            grDevices::pdf(file, width = 12, height = 8, useDingbats = FALSE)
            print(values$current_plot)
            grDevices::dev.off()
          } else {
            ggplot2::ggsave(
              filename = file,
              plot = values$current_plot,
              width = 12, height = 8, dpi = 300, units = "in"
            )
          }
        }, error = function(e) {
          shiny::showNotification(paste("Download error:", e$message), type = "error")
          if (names(grDevices::dev.cur()) %in% c("pdf", "cairo_pdf")) {
            try(grDevices::dev.off(), silent = TRUE)
          }
        })
      }
    )
    
    output$download_norm_plot <- shiny::downloadHandler(
      filename = function() {
        ext <- if (identical(input$img_format_norm, "pdf")) "pdf" else "png"
        paste0("normalized_plot_", Sys.Date(), ".", ext)
      },
      content = function(file) {
        tryCatch({
          if (is.null(values$current_norm_plot)) {
            stop("No normalized plot available. Click 'Create Plot' first.")
          }
          p <- values$current_norm_plot
          is_pdf <- identical(input$img_format_norm, "pdf")
          
          if (is_pdf) {
            grDevices::pdf(file, width = 12, height = 8, useDingbats = FALSE)
          } else {
            grDevices::png(file, width = 12, height = 8, units = "in", res = 300)
          }
          on.exit({
            if (grDevices::dev.cur() > 1) try(grDevices::dev.off(), silent = TRUE)
          }, add = TRUE)
          
          if (inherits(p, "grob") || inherits(p, "gtable")) {
            grid::grid.newpage()
            grid::grid.draw(p)
          } else {
            print(p)
          }
        }, error = function(e) {
          shiny::showNotification(paste("Normalized plot download error:", e$message), type = "error")
        })
      }
    )
    
    # FIXED: Pipeline comparison download with PDF/PNG choice
    output$download_pipeline_comparison <- shiny::downloadHandler(
      filename = function() {
        ext <- if (identical(input$img_format_pipeline, "pdf")) "pdf" else "png"
        paste0("pipeline_comparison_", Sys.Date(), ".", ext)
      },
      content = function(file) {
        req(input$norm_methods_1, input$norm_methods_2, values$raw_data)
        tryCatch({
          pipeline1_data <- apply_normalizations(values$raw_data$numeric_data, input$norm_methods_1)
          pipeline2_data <- apply_normalizations(values$raw_data$numeric_data, input$norm_methods_2)
          
          p1 <- create_pipeline_plot(pipeline1_data, paste(input$norm_methods_1, collapse = " -> "))
          p2 <- create_pipeline_plot(pipeline2_data, paste(input$norm_methods_2, collapse = " -> "))
          
          combined_plot <- gridExtra::arrangeGrob(p1, p2, ncol = 1)
          
          is_pdf <- identical(input$img_format_pipeline, "pdf")
          
          if (is_pdf) {
            ggplot2::ggsave(
              filename = file,
              plot = combined_plot,
              width = 12, height = 10, units = "in",
              device = "pdf"
            )
          } else {
            ggplot2::ggsave(
              filename = file,
              plot = combined_plot,
              width = 12, height = 10, dpi = 300, units = "in"
            )
          }
        }, error = function(e) {
          shiny::showNotification(paste("Download error:", e$message), type = "error")
        })
      }
    )
    
    output$download_viz <- shiny::downloadHandler(
      filename = function() {
        ext <- if (identical(input$img_format_results, "pdf")) "pdf" else "png"
        paste0("results_viz_", input$contrast_select, "_", Sys.Date(), ".", ext)
      },
      content = function(file) {
        req(values$current_results_plot)
        tryCatch({
          is_pdf <- identical(input$img_format_results, "pdf")
          if (inherits(values$current_results_plot, "pheatmap")) {
            if (is_pdf) {
              grDevices::pdf(file, width = 12, height = 10, useDingbats = FALSE)
              grid::grid.draw(values$current_results_plot$gtable)
              grDevices::dev.off()
            } else {
              grDevices::png(file, width = 12, height = 10, units = "in", res = 300)
              grid::grid.draw(values$current_results_plot$gtable)
              grDevices::dev.off()
            }
          } else {
            if (is_pdf) {
              ggplot2::ggsave(filename = file, plot = values$current_results_plot,
                              width = 12, height = 10, units = "in", device = "pdf")
            } else {
              ggplot2::ggsave(filename = file, plot = values$current_results_plot,
                              width = 12, height = 10, units = "in", dpi = 300)
            }
          }
        }, error = function(e) {
          shiny::showNotification(paste("Download error:", e$message), type = "error")
        })
      }
    )
    
    output$download_current_table <- shiny::downloadHandler(
      filename = function() {
        paste0("differential_results_", input$contrast_display, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(values$diff_results, input$contrast_display)
        tryCatch({
          selected_results <- values$diff_results$results[[input$contrast_display]]
          
          if (!"Lipid" %in% colnames(selected_results)) {
            selected_results <- data.frame(
              Lipid = rownames(selected_results),
              selected_results,
              stringsAsFactors = FALSE
            )
          }
          
          write.csv(selected_results, file, row.names = FALSE)
        }, error = function(e) {
          shiny::showNotification(paste("Table download error:", e$message), type = "error")
        })
      }
    )
    
    output$download_results <- shiny::downloadHandler(
      filename = function() paste0("differential_results_all_", Sys.Date(), ".xlsx"),
      content = function(file) {
        req(values$diff_results)
        tryCatch({
          wb <- openxlsx::createWorkbook()
          for (contrast_name in names(values$diff_results$results)) {
            contrast_clean <- gsub("[^A-Za-z0-9_]", "_", contrast_name)
            openxlsx::addWorksheet(wb, contrast_clean)
            
            results_data <- values$diff_results$results[[contrast_name]]
            if (!"Lipid" %in% colnames(results_data)) {
              results_data <- data.frame(
                Lipid = rownames(results_data),
                results_data,
                stringsAsFactors = FALSE
              )
            }
            
            openxlsx::writeData(wb, contrast_clean, results_data, rowNames = FALSE)
          }
          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
        }, error = function(e) {
          shiny::showNotification(paste("Excel download error:", e$message), type = "error")
        })
      }
    )
    
    output$download_enrichment <- shiny::downloadHandler(
      filename = function() {
        paste0("enrichment_results_", input$enrichment_contrast, "_", input$enrichment_type, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(values$enrichment_results, input$enrichment_contrast, input$enrichment_type)
        tryCatch({
          if (input$enrichment_contrast %in% names(values$enrichment_results)) {
            enrichment_data <- values$enrichment_results[[input$enrichment_contrast]][[input$enrichment_type]]
            write.csv(enrichment_data, file, row.names = FALSE)
          }
        }, error = function(e) {
          shiny::showNotification(paste("Enrichment download error:", e$message), type = "error")
        })
      }
    )
    
    output$download_enrichment_viz <- shiny::downloadHandler(
      filename = function() {
        ext <- if (identical(input$img_format_enrich, "pdf")) "pdf" else "png"
        paste0("enrichment_plot_", input$enrichment_viz_contrast, "_",
               input$enrichment_viz_type, "_", Sys.Date(), ".", ext)
      },
      content = function(file) {
        req(values$current_enrichment_plot)
        tryCatch({
          if (identical(input$img_format_enrich, "pdf")) {
            ggplot2::ggsave(filename = file, plot = values$current_enrichment_plot,
                            width = 12, height = 8, units = "in", device = "pdf")
          } else {
            ggplot2::ggsave(filename = file, plot = values$current_enrichment_plot,
                            width = 12, height = 8, units = "in", dpi = 300)
          }
        }, error = function(e) {
          shiny::showNotification(paste("Enrichment plot download error:", e$message), type = "error")
        })
      }
    )
    
    output$download_expression_plots <- shiny::downloadHandler(
      filename = function() {
        ext <- if (identical(input$img_format_expression, "pdf")) "pdf" else "png"
        paste0("lipid_expression_plots_", Sys.Date(), ".", ext)
      },
      content = function(file) {
        req(values$current_expression_plots)
        tryCatch({
          is_pdf <- identical(input$img_format_expression, "pdf")
          
          if (is.list(values$current_expression_plots) && !inherits(values$current_expression_plots, "ggplot")) {
            # Multiple plots - combine them
            combined <- gridExtra::arrangeGrob(grobs = values$current_expression_plots, ncol = 1)
            
            if (is_pdf) {
              ggplot2::ggsave(filename = file, plot = combined,
                              width = 12, height = 6 * length(values$current_expression_plots),
                              units = "in", device = "pdf")
            } else {
              ggplot2::ggsave(filename = file, plot = combined,
                              width = 12, height = 6 * length(values$current_expression_plots),
                              units = "in", dpi = 300)
            }
          } else {
            # Single plot
            if (is_pdf) {
              ggplot2::ggsave(filename = file, plot = values$current_expression_plots,
                              width = 12, height = 8, units = "in", device = "pdf")
            } else {
              ggplot2::ggsave(filename = file, plot = values$current_expression_plots,
                              width = 12, height = 8, units = "in", dpi = 300)
            }
          }
        }, error = function(e) {
          shiny::showNotification(paste("Expression plot download error:", e$message), type = "error")
        })
      }
    )
    
    # ========== FIXED REPORT GENERATION WITH PLOTS ==========
    
    output$download_report <- shiny::downloadHandler(
      filename = function() {
        ext <- if (input$report_format == "pdf") "pdf" else "html"
        paste0("lipidomics_report_", Sys.Date(), ".", ext)
      },
      content = function(file) {
        # Show progress notification
        progress_id <- shiny::showNotification("Generating report with figures... Please wait.", duration = NULL, type = "message")
        
        tryCatch({
          # Create temporary directory for report generation
          temp_dir <- tempdir()
          report_dir <- file.path(temp_dir, paste0("report_", format(Sys.time(), "%Y%m%d%H%M%S")))
          dir.create(report_dir, showWarnings = FALSE, recursive = TRUE)
          
          temp_rmd <- file.path(report_dir, "report.Rmd")
          
          # Save plots as PNG files
          plot_files <- list()
          
          # Save raw data plot if available
          if (!is.null(values$current_plot)) {
            raw_plot_file <- file.path(report_dir, "raw_data_plot.png")
            tryCatch({
              ggplot2::ggsave(raw_plot_file, values$current_plot, width = 10, height = 6, dpi = 150)
              plot_files$raw_plot <- "raw_data_plot.png"
            }, error = function(e) NULL)
          }
          
          # Save normalized data plot if available
          if (!is.null(values$current_norm_plot)) {
            norm_plot_file <- file.path(report_dir, "normalized_plot.png")
            tryCatch({
              if (inherits(values$current_norm_plot, "ggplot")) {
                ggplot2::ggsave(norm_plot_file, values$current_norm_plot, width = 10, height = 6, dpi = 150)
                plot_files$norm_plot <- "normalized_plot.png"
              }
            }, error = function(e) NULL)
          }
          
          # Save results plot (volcano/heatmap) if available
          if (!is.null(values$current_results_plot)) {
            results_plot_file <- file.path(report_dir, "results_plot.png")
            tryCatch({
              if (inherits(values$current_results_plot, "pheatmap")) {
                grDevices::png(results_plot_file, width = 10, height = 8, units = "in", res = 150)
                grid::grid.draw(values$current_results_plot$gtable)
                grDevices::dev.off()
              } else if (inherits(values$current_results_plot, "ggplot")) {
                ggplot2::ggsave(results_plot_file, values$current_results_plot, width = 10, height = 8, dpi = 150)
              }
              plot_files$results_plot <- "results_plot.png"
            }, error = function(e) {
              if (grDevices::dev.cur() > 1) try(grDevices::dev.off(), silent = TRUE)
            })
          }
          
          # Save enrichment plot if available
          if (!is.null(values$current_enrichment_plot)) {
            enrichment_plot_file <- file.path(report_dir, "enrichment_plot.png")
            tryCatch({
              ggplot2::ggsave(enrichment_plot_file, values$current_enrichment_plot, width = 10, height = 6, dpi = 150)
              plot_files$enrichment_plot <- "enrichment_plot.png"
            }, error = function(e) NULL)
          }
          
          # Save pipeline comparison plots if available
          if (!is.null(values$current_pipeline_plots)) {
            pipeline_plot_file <- file.path(report_dir, "pipeline_comparison.png")
            tryCatch({
              combined <- gridExtra::arrangeGrob(
                values$current_pipeline_plots$p1, 
                values$current_pipeline_plots$p2, 
                ncol = 1
              )
              ggplot2::ggsave(pipeline_plot_file, combined, width = 10, height = 10, dpi = 150)
              plot_files$pipeline_plot <- "pipeline_comparison.png"
            }, error = function(e) NULL)
          }
          
          # Build report content with plots
          report_content <- build_report_rmd_with_plots(
            title = input$report_title,
            author = input$report_author,
            sections = input$report_sections,
            raw_data = values$raw_data,
            normalized_data = values$normalized_data,
            diff_results = values$diff_results,
            enrichment_results = values$enrichment_results,
            output_format = input$report_format,
            plot_files = plot_files
          )
          
          # Write Rmd file
          con <- file(temp_rmd, "w", encoding = "UTF-8")
          writeLines(report_content, con)
          close(con)
          
          # Set output format
          if (input$report_format == "pdf") {
            # Try PDF, fall back to HTML if it fails
            tryCatch({
              out_format <- rmarkdown::pdf_document(toc = TRUE)
              output_file <- rmarkdown::render(
                input = temp_rmd,
                output_format = out_format,
                output_dir = report_dir,
                quiet = TRUE
              )
              file.copy(output_file, file, overwrite = TRUE)
              shiny::removeNotification(progress_id)
              shiny::showNotification("PDF report generated successfully!", type = "message")
            }, error = function(pdf_err) {
              shiny::showNotification(
                "PDF generation failed. Generating HTML instead. For PDF, install tinytex::install_tinytex()",
                type = "warning", duration = 10
              )
              # Fallback to HTML
              out_format <- rmarkdown::html_document(toc = TRUE, toc_float = TRUE, theme = "flatly")
              output_file <- rmarkdown::render(
                input = temp_rmd,
                output_format = out_format,
                output_dir = report_dir,
                quiet = TRUE
              )
              file.copy(output_file, file, overwrite = TRUE)
              shiny::removeNotification(progress_id)
            })
          } else {
            # HTML format
            out_format <- rmarkdown::html_document(toc = TRUE, toc_float = TRUE, theme = "flatly")
            output_file <- rmarkdown::render(
              input = temp_rmd,
              output_format = out_format,
              output_dir = report_dir,
              quiet = TRUE
            )
            file.copy(output_file, file, overwrite = TRUE)
            shiny::removeNotification(progress_id)
            shiny::showNotification("HTML report generated successfully!", type = "message")
          }
          
        }, error = function(e) {
          shiny::removeNotification(progress_id)
          shiny::showNotification(
            paste("Report generation failed:", e$message), 
            type = "error", duration = 15
          )
          
          # Create a simple text fallback
          fallback_content <- paste0(
            "# Lipidomics Analysis Report\n\n",
            "Report generation encountered an error:\n",
            e$message, "\n\n",
            "## Troubleshooting:\n",
            "1. Try HTML format instead of PDF\n",
            "2. For PDF support, install tinytex: tinytex::install_tinytex()\n",
            "3. Ensure rmarkdown package is installed: install.packages('rmarkdown')\n"
          )
          writeLines(fallback_content, file)
        })
      }
    )
  }
  
  # Run the app
  shiny::shinyApp(ui = ui, server = server, options = list(port = port))
}

#' Build Report Rmd Content with Plots
#'
#' @param title Report title
#' @param author Report author
#' @param sections Sections to include
#' @param raw_data Raw data object
#' @param normalized_data Normalized data object
#' @param diff_results Differential analysis results
#' @param enrichment_results Enrichment analysis results
#' @param output_format Output format (html or pdf)
#' @param plot_files Named list of plot file paths
#' @return Character string with Rmd content
build_report_rmd_with_plots <- function(title, author, sections, raw_data, normalized_data, 
                              diff_results, enrichment_results, output_format = "html",
                              plot_files = list()) {
  
  # YAML header based on format
  if (output_format == "pdf") {
    yaml_header <- paste0(
      '---\n',
      'title: "', title, '"\n',
      'author: "', author, '"\n',
      'date: "', format(Sys.Date(), "%B %d, %Y"), '"\n',
      'output:\n',
      '  pdf_document:\n',
      '    toc: true\n',
      '    toc_depth: 2\n',
      '---\n\n'
    )
  } else {
    yaml_header <- paste0(
      '---\n',
      'title: "', title, '"\n',
      'author: "', author, '"\n',
      'date: "', format(Sys.Date(), "%B %d, %Y"), '"\n',
      'output:\n',
      '  html_document:\n',
      '    toc: true\n',
      '    toc_float: true\n',
      '    theme: flatly\n',
      '    highlight: tango\n',
      '---\n\n'
    )
  }
  
  # Setup chunk
  setup_chunk <- paste0(
    '```{r setup, include=FALSE}\n',
    'knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.width = 10, fig.height = 6)\n',
    '```\n\n'
  )
  
  # Build content sections
  content <- ""
  
  # Data Summary Section
  if ("data_summary" %in% sections && !is.null(raw_data)) {
    n_samples <- nrow(raw_data$data)
    n_features <- ncol(raw_data$numeric_data)
    
    content <- paste0(content,
      '# Data Summary\n\n',
      '## Dataset Overview\n\n',
      '| Metric | Value |\n',
      '|--------|-------|\n',
      '| Number of samples | ', n_samples, ' |\n',
      '| Number of lipid features | ', n_features, ' |\n\n'
    )
    
    if (!is.null(raw_data$metadata) && "Sample Group" %in% names(raw_data$metadata)) {
      group_table <- table(raw_data$metadata$`Sample Group`)
      content <- paste0(content,
        '## Sample Distribution by Group\n\n',
        '| Group | Count |\n',
        '|-------|-------|\n'
      )
      for (grp in names(group_table)) {
        content <- paste0(content, '| ', grp, ' | ', group_table[grp], ' |\n')
      }
      content <- paste0(content, '\n')
    }
    
    # Add raw data plot if available
    if (!is.null(plot_files$raw_plot)) {
      content <- paste0(content,
        '## Raw Data Distribution\n\n',
        '![Raw Data Distribution](', plot_files$raw_plot, ')\n\n'
      )
    }
  }
  
  # Normalization Section
  if ("normalization" %in% sections) {
    content <- paste0(content,
      '# Normalization\n\n',
      'Data normalization was performed to reduce technical variation and enable ',
      'meaningful comparison between samples.\n\n'
    )
    
    if (!is.null(normalized_data)) {
      content <- paste0(content,
        '**Status:** Normalization has been applied to the dataset.\n\n'
      )
      
      # Add pipeline comparison plot if available
      if (!is.null(plot_files$pipeline_plot)) {
        content <- paste0(content,
          '## Normalization Pipeline Comparison\n\n',
          '![Pipeline Comparison](', plot_files$pipeline_plot, ')\n\n'
        )
      }
      
      # Add normalized data plot if available
      if (!is.null(plot_files$norm_plot)) {
        content <- paste0(content,
          '## Normalized Data Distribution\n\n',
          '![Normalized Data](', plot_files$norm_plot, ')\n\n'
        )
      }
    } else {
      content <- paste0(content,
        '**Status:** Normalization has not yet been applied.\n\n'
      )
    }
  }
  
  # Differential Analysis Section - FIXED terminology
  if ("diff_analysis" %in% sections && !is.null(diff_results)) {
    content <- paste0(content,
      '# Differential Analysis\n\n',
      '## Analysis Parameters\n\n',
      '- **Method:** ', diff_results$method, '\n',
      '- **Number of contrasts tested:** ', length(diff_results$results), '\n\n',
      '## Results Summary\n\n'
    )
    
    for (contrast_name in names(diff_results$results)) {
      res <- diff_results$results[[contrast_name]]
      n_total <- nrow(res)
      n_sig <- sum(res$adj.P.Val < 0.05, na.rm = TRUE)
      n_up <- sum(res$adj.P.Val < 0.05 & res$logFC > 0, na.rm = TRUE)
      n_down <- sum(res$adj.P.Val < 0.05 & res$logFC < 0, na.rm = TRUE)
      
      # FIXED: Use "Increased/Decreased abundance" instead of "Up/Down regulated"
      content <- paste0(content,
        '### ', contrast_name, '\n\n',
        '| Metric | Count |\n',
        '|--------|-------|\n',
        '| Total features tested | ', n_total, ' |\n',
        '| Significant (adj.P < 0.05) | ', n_sig, ' |\n',
        '| Increased abundance | ', n_up, ' |\n',
        '| Decreased abundance | ', n_down, ' |\n\n'
      )
    }
    
    # Add results plot (volcano or heatmap) if available
    if (!is.null(plot_files$results_plot)) {
      content <- paste0(content,
        '## Results Visualization\n\n',
        '![Differential Analysis Results](', plot_files$results_plot, ')\n\n'
      )
    }
    
    # Add top significant features table
    if (length(diff_results$results) > 0) {
      first_contrast <- names(diff_results$results)[1]
      first_res <- diff_results$results[[first_contrast]]
      sig_features <- first_res[first_res$adj.P.Val < 0.05, ]
      
      if (nrow(sig_features) > 0) {
        sig_features <- sig_features[order(sig_features$adj.P.Val), ]
        top_features <- head(sig_features, 10)
        
        content <- paste0(content,
          '## Top Significant Features (', first_contrast, ')\n\n',
          '| Lipid | LogFC | adj.P.Val | Direction |\n',
          '|-------|-------|-----------|----------|\n'
        )
        
        for (i in 1:nrow(top_features)) {
          direction <- if (top_features$logFC[i] > 0) "Increased" else "Decreased"
          content <- paste0(content,
            '| ', rownames(top_features)[i], 
            ' | ', round(top_features$logFC[i], 3),
            ' | ', format(top_features$adj.P.Val[i], scientific = TRUE, digits = 3),
            ' | ', direction, ' |\n'
          )
        }
        content <- paste0(content, '\n')
      }
    }
  }
  
  # Enrichment Section
  if ("enrichment" %in% sections && !is.null(enrichment_results)) {
    content <- paste0(content,
      '# Enrichment Analysis\n\n',
      'Enrichment analysis was performed to identify lipid classes and categories ',
      'that show coordinated changes between conditions.\n\n',
      '**Number of contrasts analyzed:** ', length(enrichment_results), '\n\n'
    )
    
    # Add enrichment plot if available
    if (!is.null(plot_files$enrichment_plot)) {
      content <- paste0(content,
        '## Enrichment Visualization\n\n',
        '![Enrichment Analysis](', plot_files$enrichment_plot, ')\n\n'
      )
    }
    
    # Add enrichment results summary
    for (contrast_name in names(enrichment_results)) {
      contrast_results <- enrichment_results[[contrast_name]]
      
      content <- paste0(content, '### ', contrast_name, '\n\n')
      
      for (enrich_type in names(contrast_results)) {
        enrich_data <- contrast_results[[enrich_type]]
        if (nrow(enrich_data) > 0) {
          sig_pathways <- enrich_data[enrich_data$padj < 0.25, ]
          
          if (nrow(sig_pathways) > 0) {
            content <- paste0(content,
              '**', enrich_type, '** - Significant pathways (padj < 0.25):\n\n',
              '| Pathway | NES | p-value | padj |\n',
              '|---------|-----|---------|------|\n'
            )
            
            top_pathways <- head(sig_pathways[order(sig_pathways$pval), ], 5)
            for (j in 1:nrow(top_pathways)) {
              content <- paste0(content,
                '| ', top_pathways$pathway[j],
                ' | ', round(top_pathways$NES[j], 3),
                ' | ', format(top_pathways$pval[j], scientific = TRUE, digits = 2),
                ' | ', format(top_pathways$padj[j], scientific = TRUE, digits = 2), ' |\n'
              )
            }
            content <- paste0(content, '\n')
          }
        }
      }
    }
  }
  
  # Session Information
  content <- paste0(content,
    '# Session Information\n\n',
    '- **Report generated:** ', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '\n',
    '- **R version:** ', R.version.string, '\n\n'
  )
  
  return(paste0(yaml_header, setup_chunk, content))
}
