library(shiny)
library(shinydashboard)
library(bslib)
library(DT)
library(plotly)
library(reactable)
library(shinyjs)
library(shinyjqui)

ui <- dashboardPage(
  dashboardHeader(title = "📊 Data Science Dashboard", titleWidth = 300),
  
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("Load Data", tabName = "load_data", icon = icon("file-upload", lib = "font-awesome")),
      menuItem("PreProcessing", tabName = "preprocessing", icon = icon("cogs")),
      menuItem("Analyse Data", tabName = "analyse_data", icon = icon("chart-line")),
      menuItem("ML Models", tabName = "ml_models", icon = icon("robot")),
      menuItem("Results", tabName = "results", icon = icon("chart-pie")),
      menuItem("Implements", tabName = "implements", icon = icon("play-circle")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Customizing the Dashboard */
        .skin-blue .main-header .logo {
          background-color: #1f77b4; /* Header background */
          color: white; /* Header text color */
          font-size: 20px; /* Header font size */
        }
        .skin-blue .main-sidebar {
          background-color: #343a40; /* Sidebar background */
          color: #f8f9fa; /* Sidebar text color */
        }
        .skin-blue .sidebar-menu>li.active>a {
          background-color: #17a2b8; /* Active menu item background */
          color: white;
        }
        .skin-blue .sidebar-menu>li>a:hover {
          background-color: #007bff; /* Hovered menu item background */
          color: white;
        }
      "))
    ),
    
    tabItems(
      tabItem(
        tabName = "load_data",
        fluidRow(
          box(
            title = "Loading Data",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            fileInput("fileInput", "Browse File", accept = c(".csv", ".xlsx")),
            helpText("Upload a CSV or Excel file.")
          ),
          box(
            title = "File Details",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("fileDetails")
          )
        ),
        fluidRow(
          box(
            title = "Data Exploration",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            tabsetPanel(
              tabPanel(
                title = "Show Data", status = "primary", solidHeader = TRUE,
                style = "overflow-x: auto;",
                DTOutput("table"),
                width = 9
              ),
              tabPanel(
                title = "Data Summary",
                DTOutput("dataSummary")  # Change from verbatimTextOutput
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Drop Feature",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            uiOutput("drop_feature_ui"),  # Use dynamic UI
            actionButton("apply_drop", "Apply")
          )
        ),
        fluidRow(
          box(
            title = "Drag & Drop: Switch Variable Type",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            uiOutput("drag_drop_ui"),
            actionButton("apply_switch", "Apply Switch")
          )
        )
      ),
      
      
      tabItem(
        tabName = "preprocessing",
        fluidRow(
          # Handling Missing Values Box
          box(
            title = "Handle Missing Values", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 6,
            # Select variable from dataset
            uiOutput("missing_var_ui"),
            # Display missing percentage
            textOutput("missing_percent"),
            # Select method to handle missing values
            uiOutput("missing_method_ui"),
            actionButton("apply_missing", "Apply")
          ),
          # Handling Outliers Box
          box(
            title = "Handle Outliers", 
            status = "warning", 
            solidHeader = TRUE, 
            width = 6,
            # Select variable for outlier handling
            uiOutput("outlier_var_ui"),
            # Select method to handle outliers
            selectInput(
              inputId = "outlier_method", 
              label = "Select Outlier Handling Method:", 
              choices = c("Remove Outliers", "Replace with Median", "Replace with Mean"), 
              selected = "Remove Outliers"
            ),
            actionButton("apply_outliers", "Apply")
          )
        ),
        fluidRow(
          # Data Transformation Box
          box(
            title = "Data Transformation",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            # Select numerical variables for transformation (no default selection)
            uiOutput("transform_var_ui"),
            # Transformation method selection
            selectInput(
              inputId = "transformation_method",
              label = "Select Transformation Method:",
              choices = c("Min-Max Scaling", "Z-Score Normalization", "Log Transformation"),
              selected = "Min-Max Scaling"
            ),
            actionButton("apply_transformation", "Apply")
          ),
          # Encoding Data Box
          box(
            title = "Encoding Data",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            # Select categorical variables for encoding (no default selection)
            uiOutput("encoding_var_ui"),
            # Encoding method selection
            selectInput(
              inputId = "encoding_method",
              label = "Select Encoding Method:",
              choices = c("Label Encoding", "One-Hot Encoding"),
              selected = "Label Encoding"
            ),
            actionButton("apply_encoding", "Apply")
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              style = "text-align: center; margin-top: 20px; margin-bottom: 30px;",  # Added margin-bottom
              actionButton("submit_data", "Submit", 
                           style = "padding: 10px 20px; font-size: 18px; background-color: #007BFF; color: white;"),
              downloadButton("save_data", "Save", 
                             class = "btn btn-secondary shiny-disabled",
                             style = "padding: 10px 20px; font-size: 18px; margin-left: 20px;")
            )
          )
         
        ),
        fluidRow(
          # Display Submitted Data Table
          box(
            title = "Data Table",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            dataTableOutput("submitted_table")
          )
        )
      ),
      
      
      tabItem(
        tabName = "analyse_data",
        
        # First row: Variable selection
        fluidRow(
          # Box for single variable selection
          box(
            title = "Univariate Analysis Variable Selection", status = "primary", solidHeader = TRUE, width = 6,
            selectInput("x_variable", "Variable:", choices = NULL)
          ),
          # Box for two-variable selection
          box(
            title = "Bivariate Analysis Variable Selection", status = "primary", solidHeader = TRUE, width = 6,
            selectInput("x_variable_bi", "X Variable:", choices = NULL),
            selectInput("y_variable", "Y Variable:", choices = NULL)
          )
        ),
        
        # Second row: Unidimensional and Bidimensional analysis
        fluidRow(
          # Unidimensional analysis box
          box(
            title = "Unidimensional Analysis", status = "primary", solidHeader = TRUE, width = 6,
            tabsetPanel(
              tabPanel(
                "Histogramme", 
                numericInput("binwidth_input", "Binwidth:", value = 0.2, min = 0.01, step = 0.01),
                plotlyOutput("histogram")
              ),
              tabPanel("Box Plot", plotlyOutput("boxplot")),
              tabPanel("Extra", verbatimTextOutput("univariate_analysis")),
              # Add a Pie Chart tab, dynamically controlled
              tabPanel(
                "Pie Chart",
                conditionalPanel(
                  condition = "output.is_categorical === true", # Show only if variable is categorical
                  plotOutput("pie_chart", height = 500, width = 600)
                )
              )
            )
          ),
          # Bidimensional analysis box
          box(
            title = "Bidimensional Analysis", status = "primary", solidHeader = TRUE, width = 6,
            tabsetPanel(
              tabPanel(
                "Correlation Plot",
                plotlyOutput("bivariate_analysis"),
                conditionalPanel(
                  condition = "output.show_correlation === true",  # Show only for numeric variables
                  h4(textOutput("correlation_coefficient"), style = "color: red; margin-top: 10px;")
                )
              ),
              tabPanel("Correlation Matrix", plotOutput("correlation_matrix_plot")),
              tabPanel(
                "Box Plot",
                plotOutput("boxplot_parallel"),
                conditionalPanel(
                  condition = "output.show_correlation_ratio === true",  # Show only for quantitative vs qualitative
                  h4(textOutput("correlation_ratio"), style = "color: blue; margin-top: 10px;")
                )
              ),
              tabPanel("Bar Plot (Profils-Colonnes)", plotOutput("bar_plot_profiles")),
              tabPanel(
                "Contingency Table & Cramér's V",
                tableOutput("contingency_table"),
                h4(textOutput("cramers_v"), style = "color: blue; margin-top: 10px;")
              )
            )
          )
        )
      ),
      
      tabItem(
        tabName = "ml_models",
        fluidRow(
          useShinyjs(),
          box(
            title = "Select Target Variable",
            status = "success",
            solidHeader = TRUE,
            width = 4,
            selectInput("target_variable", "Select the Target Variable", choices = NULL),
            actionButton("validate_target", "Validate Target Variable")
          ),
          
          # Box for displaying histogram of the target variable
          box(
            title = "Target Variable Histogram",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            plotOutput("target_histogram")  # Render histogram dynamically
          ),
          
          # Box for selecting resampling technique
          box(
            title = "Handle Imbalanced Data",
            status = "warning",
            solidHeader = TRUE,
            width = 4,
            selectInput("resampling_technique", "Choose Resampling Technique", 
                        choices = c("undersampling", "oversampling"), 
                        selected = "oversampling"),
            actionButton("apply_resampling", "Apply Resampling")
          ),
          box(
            title = "Split Data", status = "primary", solidHeader = TRUE, width = 12,
            selectInput(
              "split_method",
              "Choose a Data Split Method:",
              choices = c("Holdout"),
              selected = "Holdout"
            ),
            conditionalPanel(
              condition = "input.split_method == 'Holdout'",
              sliderInput(
                "train_percentage",
                "Training Data (%)",
                min = 50,
                max = 90,
                value = 70,
                step = 5
              ),
              sliderInput(
                "test_percentage",
                "Testing Data (%)",
                min = 10,
                max = 50,
                value = 30,
                step = 5
              )
            ),
            conditionalPanel(
              condition = "input.split_method == 'Cross-validation'",
              numericInput(
                "k_folds",
                "Number of Folds (k):",
                value = 5,
                min = 2,
                step = 1
              )
            ),
            actionButton("split_data", "Split Data", class = "btn-primary"),
            verbatimTextOutput("split_message")  # Display split information
          ),
          
          box(
            title = "Model Selection", status = "primary", solidHeader = TRUE, width = 12,
            selectInput(
              "model_choice",
              "Select Model:",
              choices = NULL,
              selected = NULL
            )
          ),
          box(
            title = "Model Parameters", status = "primary", solidHeader = TRUE, width = 12,
            # SVM Parameters
            conditionalPanel(
              condition = "input.model_choice == 'SVM'",
              numericInput("svm_C", "Parameter C:", value = 0.01, min = 0.001, step = 0.001),
              selectInput("svm_kernel", "Kernel Type:", choices = c("linear", "polynomial", "rbf"), selected = "linear")
            ),
            # Random Forest Parameters
            conditionalPanel(
              condition = "input.model_choice == 'Random Forest'",
              numericInput("rf_trees", "Number of Trees:", value = 100, min = 1, step = 1)
            ),
            # Linear Regression - Empty Placeholder
            conditionalPanel(
              condition = "input.model_choice == 'Linear Regression'",
              tags$p("No parameters to configure for Linear Regression.")
            ),
            # Decision Tree Parameters
            conditionalPanel(
              condition = "input.model_choice == 'Decision Tree'",
              numericInput("dt_max_depth", "Maximum Depth:", value = 5, min = 1, step = 1),
              selectInput(
                "dt_criterion",
                "Split Criterion:",
                choices = c("gini", "entropy"),
                selected = "gini"
              )
            )
          ),
          
          box(
            title = "Train and Save Model", status = "primary", solidHeader = TRUE, width = 12,
            actionButton("train_model", "Train Model", class = "btn-primary"),
            uiOutput("save_model_ui") # Cet élément sera rendu dynamiquement après l'entraînement
          ),
          verbatimTextOutput("model_message")
        )
      ),
       
      tabItem(
        tabName = "results",
        fluidRow(
          # First row of boxes
          box(
            title = "Model Metrics", status = "primary", solidHeader = TRUE, width = 4,
            actionButton("show_results", "Show Results"),  # Button to trigger results
            tableOutput("model_metrics")  # Table to display metrics
          ),
          box(
            title = "Confusion Matrix", status = "primary", solidHeader = TRUE, width = 4,
            plotOutput("conf_matrix_plot")  # Plot to display confusion matrix
          ),
          box(
            title = "ROC Curve", status = "primary", solidHeader = TRUE, width = 4,
            plotOutput("roc_curve"),
            h4("AUC Score:"),
            textOutput("auc_score")
          )
        ),
        # New fluidRow with box for graph
        fluidRow(
          box(
            title = "Feature Importance", status = "primary", solidHeader = TRUE, width = 12,
            plotOutput("feature_importance_plot")  # Graph to display feature importance
          )
        )
      )
      
)
)
)
