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
        ),
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
        fluidRow(
          # Sélection des variables
          box(
            title = "Variable Selection", status = "primary", solidHeader = TRUE, width = 2,
            h3("Variable Selection"),
            selectInput("x_variable", "X Variable:", choices = NULL),
            selectInput("y_variable", "Y Variable:", choices = NULL)
          ),
          # Statistiques descriptives unidimensionnelles
          box(
            title = "Statistiques descriptives unidimensionnelle", status = "primary", solidHeader = TRUE,
            tabsetPanel(
              tabPanel("Histogramme", numericInput("binwidth_input", "Binwidth:", value = 0.2, min = 0.01, step = 0.01), plotlyOutput("histogram")),
              tabPanel("Box Plot", plotlyOutput("boxplot")),
              tabPanel("Extra", verbatimTextOutput("univariate_analysis")),
              tabPanel("Pie Chart", plotOutput("pie_chart", height = 500, width = 600))
            ),
            width = 5
          ),
          # Analyse bidimensionnelle
          box(
            title = "Analyse bidimensionnelle", status = "primary", solidHeader = TRUE,
            tabsetPanel(
              tabPanel("correlation plot", plotlyOutput("bivariate_analysis")),
              tabPanel("Correlation Matrix", plotOutput("correlation_matrix_plot")),
              tabPanel("Box Plot",plotOutput("boxplot_parallel"))
            ),
            width = 5
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
          )
        ),
      ),
      
      tabItem(
        tabName = "results",
        fluidRow(
          box(
            title = "Model Metrics", status = "primary", solidHeader = TRUE, width = 4,
            tableOutput("model_metrics")
          ),
          box(
            title = "Confusion Matrix", status = "primary", solidHeader = TRUE, width = 4,
            plotOutput("conf_matrix_plot")
          ),
          box(
            title = "ROC Curve", status = "primary", solidHeader = TRUE, width = 4,
            plotOutput("roc_curve"),
            h4("AUC Score:"),
            textOutput("auc_score")
          )
        )
      )
      
      
    )
  )
)