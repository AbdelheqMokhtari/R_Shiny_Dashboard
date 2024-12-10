library(shiny)
library(shinydashboard)
library(bslib)
library(readxl)  
library(DT)
library(plotly)
ui <- dashboardPage(
  dashboardHeader(title = "\U0001F504 Data Science Dashboard"),
  dashboardSidebar(

    sidebarMenu(
      menuItem("Load Data & EDA", tabName = "load_data", icon = icon("file-upload")),

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
    tabItems(
      tabItem(
        tabName = "load_data",
        fluidRow(
          box(
            title = "Upload Data", status = "primary", solidHeader = TRUE, width = 3,  
            fileInput("file", "Choose a File", 
                      accept = c(".csv", ".xlsx", ".xls", ".data"), 
                      multiple = FALSE),
            conditionalPanel(
              condition = "input.file && input.file[0].name.endsWith('.data')",
              fileInput("names_file", "Upload .names File", accept = c(".names"))
            ),

            DTOutput("data_preview")
          )
        )
      ),
      tabItem(
        tabName = "analyse_data",
        fluidRow(

          ),
          
          # Second box: Aperçu des données

          box(
            title = "Aperçu des données", status = "primary", solidHeader = TRUE,
            style = "overflow-x: auto;",
            DTOutput("table"),
            width = 9  
          ),

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
          box(
            title = "Analyse bidimensionnelle", status = "primary", solidHeader = TRUE,
            tabsetPanel(
              tabPanel("correlation plot", plotlyOutput("bivariate_analysis")),
              tabPanel("Correlation Matrix", plotOutput("correlation_matrix_plot")),
              tabPanel("Box Plot", plotOutput("boxplot_parallel"))
            ),
            width = 5
          )
        )
      ),
      tabItem(
        tabName = "ml_models",
        fluidRow(
          box(
            title = "Split Data", status = "primary", solidHeader = TRUE, width = 12,
            selectInput(
              "split_method",
              "Choose a Data Split Method:",
              choices = c("Holdout", "Cross-validation"),
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
            )
          ),
          box(
            title = "Model Selection", status = "primary", solidHeader = TRUE, width = 12,
            selectInput(
              "target_variable",
              "Select Target Variable:",
              choices = NULL,
              selected = NULL
            ),
            selectInput(
              "model_choice",
              "Select Model:",
              choices = NULL,
              selected = NULL
            )
          ),
          box(
            title = "Model Parameters", status = "primary", solidHeader = TRUE, width = 12,
            conditionalPanel(
              condition = "input.model_choice == 'SVM'",
              numericInput("svm_C", "Parameter C:", value = 0.01, min = 0.001, step = 0.001),
              selectInput("svm_kernel", "Kernel Type:", choices = c("linear", "polynomial", "rbf"), selected = "linear")
            ),
            conditionalPanel(
              condition = "input.model_choice == 'Random Forest'",
              numericInput("rf_trees", "Number of Trees:", value = 100, min = 1, step = 1)
            ),
            conditionalPanel(
              condition = "input.model_choice == 'Logistic Regression'",
              checkboxInput("log_reg_penalty", "Include Penalty (Regularization)?", value = TRUE),
              selectInput(
                "log_reg_penalty_type",
                "Penalty Type:",
                choices = c("L1 (Lasso)", "L2 (Ridge)"),
                selected = "L2 (Ridge)"
              ),
              conditionalPanel(
                condition = "input.log_reg_penalty == true",
                numericInput("log_reg_penalty_value", "Penalty Strength (λ):", value = 0.01, min = 0.001, step = 0.001)
              )
            ),
            conditionalPanel(
              condition = "input.model_choice == 'Linear Regression'",
              checkboxInput("lin_reg_include_intercept", "Include Intercept?", value = TRUE)
            ),
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
            title = "Train & Save Model", status = "primary", solidHeader = TRUE, width = 12,
            actionButton("train_model", "Train Model", icon = icon("play-circle")),
            conditionalPanel(
              condition = "output.model_trained",
              downloadButton("save_model", "Save Model", icon = icon("download"))
            )
          )
        )
      ),
      tabItem(
        tabName = "results",
        fluidRow(
          box(
            title = "Model Evaluation", status = "success", solidHeader = TRUE, width = 12,
            p("Evaluate the performance of your trained model on the test dataset."),
            
            # Metrics Table
            box(
              title = "Evaluation Metrics", status = "primary", solidHeader = TRUE, width = 4,
              verbatimTextOutput("evaluation_metrics")
            ),
            
            # Confusion Matrix
            box(
              title = "Confusion Matrix", status = "primary", solidHeader = TRUE, width = 4,
              plotOutput("confusion_matrix_plot")
            ),
            
            # ROC Curve and AUC
            box(
              title = "ROC Curve", status = "primary", solidHeader = TRUE, width = 4,
              plotOutput("roc_curve"),
              verbatimTextOutput("auc_value")
            )
          )
        )
      )
      
    )
  )
)
