library(shiny)
library(shinydashboard)
library(bslib)
library(readxl)  
library(DT)
library(plotly)
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

    /* Ensuring equal height for Missing Values and Outliers boxes */
    .equal-height-container {
      display: flex;
      justify-content: space-between;
      gap: 10px;  /* Optional: adds space between columns */
    }

    .equal-height-box {
      display: flex;
      flex-direction: column;
      justify-content: space-between;
      height: 100%;   /* Ensures both boxes fill the available space equally */
    }

    /* Optional: You can set a minimum height if desired */
    .box {
      min-height: 300px;  /* Optional: Ensures boxes are large enough */
    }
  "))
    ),
    
    
    
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
      tabName = "Preprocessing",
      fluidRow(
        # Box for Handling Missing Values
        column(
          width = 6,
          div(class = "equal-height-box box",  # Apply equal height box styling
              box(
                title = "Handle Missing Values",
                width = 12, 
                solidHeader = TRUE, 
                status = "primary",
                selectInput(
                  inputId = "missing_feature",
                  label = "Select Feature:",
                  choices = names(uploaded_data()),
                  selected = names(uploaded_data())[1]
                ),
                textOutput("missing_percentage"),
                conditionalPanel(
                  condition = "output.missingPercentage !== '0%'",
                  selectInput(
                    inputId = "missing_action",
                    label = "Choose Action:",
                    choices = c(
                      "Remove Rows" = "remove",
                      "Replace with Mean" = "mean",
                      "Replace with Median" = "median",
                      "Replace with Mode" = "mode"
                    )
                  ),
                  actionButton(inputId = "apply_missing", label = "Apply")
                )
              )
          )
        ),
        
        # Box for Handling Outliers
        column(
          width = 6,
          div(class = "equal-height-box box",  # Apply equal height box styling
              box(
                title = "Handle Outliers",
                width = 12, 
                solidHeader = TRUE, 
                status = "warning",
                selectInput(
                  inputId = "outlier_feature",
                  label = "Select Feature (Numerical Only):",
                  choices = names(uploaded_data())[sapply(uploaded_data(), is.numeric)],
                  selected = names(uploaded_data())[sapply(uploaded_data(), is.numeric)][1]
                ),
                selectInput(
                  inputId = "outlier_method",
                  label = "Select Method:",
                  choices = c(
                    "Remove Outliers" = "remove",
                    "Replace with Mean" = "mean",
                    "Replace with Median" = "median"
                  )
                ),
                actionButton(inputId = "apply_outliers", label = "Apply")
              )
          )
        )
      ),
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
          )
        )
      ),
      
      tabItem(
        tabName = "results",
        fluidRow(
          box(
            title = "Results", status = "warning", solidHeader = TRUE, width = 12,
            p("This section will display model performance results."),
            icon("chart-bar", lib = "font-awesome")
          )
        )
      )
    )
  )
)