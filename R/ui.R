library(shiny)
library(shinydashboard)
library(bslib)
library(readxl)  
library(DT)
library(plotly)
ui <- dashboardPage(
  dashboardHeader(title = "📊 Data Science Dashboard", titleWidth = 300),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Load Data & EDA", tabName = "load_data", icon = icon("file-upload", lib = "font-awesome")),
      menuItem("Analyse Data", tabName = "analyse_data", icon = icon("chart-line")),
      menuItem("ML Models", tabName = "ml_models", icon = icon("robot")),
      menuItem("Results", tabName = "results", icon = icon("chart-pie"))
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
            title = "Upload Data", status = "primary", solidHeader = TRUE, width = 12,
            fileInput("file", "Choose a File", 
                      accept = c(".csv", ".xlsx", ".xls", ".data"), 
                      multiple = FALSE),
            conditionalPanel(
              condition = "input.file && input.file[0].name.endsWith('.data')",
              fileInput("names_file", "Upload .names File", accept = c(".names"))
            ),
          )
        )
      ),
      
      tabItem(
        tabName = "analyse_data",
        fluidRow(
          # Aperçu des données
          box(
            title = "Aperçu des données", status = "primary", solidHeader = TRUE,
            style = "overflow-x: auto;",
            DTOutput("table"),
            width = 12
          ),
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
              tabPanel("Histogramme", numericInput("binwidth_input", "Binwidth:", value = 0.2, min = 0.01, step = 0.01), plotOutput("histogram")),
              tabPanel("Box Plot", plotOutput("boxplot")),
              tabPanel("Extra", verbatimTextOutput("univariate_analysis")),
              tabPanel("Pie Chart", plotOutput("pie_chart", height = 500, width = 600))
            ),
            width = 5
          ),
          # Analyse bidimensionnelle
          box(
            title = "Analyse bidimensionnelle", status = "primary", solidHeader = TRUE,
            tabsetPanel(
              tabPanel("correlation plot", plotOutput("bivariate_analysis")),
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
          # Box pour Split Data
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