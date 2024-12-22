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
      menuItem("Study case", tabName = "study_case", icon = icon("play-circle")),
      menuItem("Load Data", tabName = "load_data", icon = icon("file-upload", lib = "font-awesome")),
      menuItem("PreProcessing", tabName = "preprocessing", icon = icon("cogs")),
      menuItem("Analyse Data", tabName = "analyse_data", icon = icon("chart-line")),
      menuItem("ML Models", tabName = "ml_models", icon = icon("robot")),
      menuItem("Results", tabName = "results", icon = icon("chart-pie")),
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
    useShinyjs(),  # Enable shinyjs
    tabItems(
      tabItem(
        tabName = "study_case",
        fluidRow(
          column(
            width = 12,
            tabsetPanel(
              tabPanel(
                title = "Housing Prices in California: Study Case",
                tags$h3("Study Case Overview"),
                tags$p("This study aims to predict house prices in California using a dataset containing 10 features. The target variable is the `median_house_value`. A linear regression model is employed to predict house prices."),
                
                tags$h3("Dataset Description"),
                tags$ul(
                  tags$li(tags$b("Features:"), " The dataset includes 10 features: 1 categorical and 9 numerical."),
                  tags$li(tags$b("Target Variable:"), " `median_house_value` is a continuous numeric variable representing the house price."),
                  tags$li(tags$b("Missing Data:"), " The feature `total_bedrooms` contains 1% missing values, which are handled during preprocessing."),
                  tags$li(tags$b("Dataset Dimensions:"), " The dataset contains 20,641 rows.")
                ),
                
                tags$h3("Preprocessing"),
                tags$ul(
                  tags$li(tags$b("Missing Data Handling:"), " Missing values in `total_bedrooms` are addressed using a chosen imputation method."),
                  tags$li(tags$b("Feature Scaling:"), " Numerical features are scaled to ensure consistent model performance."),
                  tags$li(tags$b("Categorical Encoding:"), " The categorical feature is encoded for compatibility with the regression model.")
                ),
                
                tags$h3("Modeling"),
                tags$ul(
                  tags$li(tags$b("Model:"), " Linear regression is used to predict the `median_house_value`."),
                  tags$li(tags$b("Data Split:"), " The dataset is split into 70% training and 30% testing subsets."),
                  tags$li(tags$b("Training Process:"), " The model is trained on the training dataset and evaluated on the test dataset.")
                ),
                
                tags$h3("Model Evaluation"),
                tags$ul(
                  tags$li(tags$b("Mean Squared Error (MSE):"), " 4,722,634,625.73"),
                  tags$li(tags$b("Root Mean Squared Error (RMSE):"), " 68,721.43"),
                  tags$li(tags$b("R-squared:"), " 0.64"),
                  tags$li(tags$p("The model demonstrates a reasonable fit with an R-squared value of 0.64, indicating that 64% of the variance in house prices can be explained by the features."))
                ),
                
                tags$h3("Insights and Observations"),
                tags$ul(
                  tags$li("The high RMSE suggests significant variability in house prices, possibly due to outliers or unmodeled factors."),
                  tags$li("The moderate R-squared value highlights the importance of additional features or complex models to capture remaining variance."),
                  tags$li("Imputation of missing values and feature scaling significantly improved model performance.")
                ),
                
                tags$h3("Visualizations"),
                
                tags$h4("Model Predictions vs Actual Values"),
                tags$img(src = "actual_vs_predictions.png", height = "300px", width = "500px"),
                
                tags$h4("Residuals Distribution"),
                tags$img(src = "residuals_vs_predicted.png", height = "300px", width = "500px")
                ),
              tabPanel(
                title = "Housing Prices in California: Study Case",
                tags$h3("Study Case Overview"),
                tags$p("This study aims to predict house prices in California using a dataset containing 10 features. The target variable is the `median_house_value`. A linear regression model is employed to predict house prices."),
                
                tags$h3("Dataset Description"),
                tags$ul(
                  tags$li(tags$b("Features:"), " The dataset includes 10 features: 1 categorical and 9 numerical."),
                  tags$li(tags$b("Target Variable:"), " `median_house_value` is a continuous numeric variable representing the house price."),
                  tags$li(tags$b("Missing Data:"), " The feature `total_bedrooms` contains 1% missing values, which are handled during preprocessing."),
                  tags$li(tags$b("Dataset Dimensions:"), " The dataset contains 20,641 rows.")
                ),
                
                tags$h3("Preprocessing"),
                tags$ul(
                  tags$li(tags$b("Missing Data Handling:"), " Missing values in `total_bedrooms` are addressed using a chosen imputation method."),
                  tags$li(tags$b("Feature Scaling:"), " Numerical features are scaled to ensure consistent model performance."),
                  tags$li(tags$b("Categorical Encoding:"), " The categorical feature is encoded for compatibility with the regression model.")
                ),
                
                tags$h3("Modeling"),
                tags$ul(
                  tags$li(tags$b("Model:"), " Linear regression is used to predict the `median_house_value`."),
                  tags$li(tags$b("Data Split:"), " The dataset is split into 70% training and 30% testing subsets."),
                  tags$li(tags$b("Training Process:"), " The model is trained on the training dataset and evaluated on the test dataset.")
                ),
                
                tags$h3("Model Evaluation"),
                tags$ul(
                  tags$li(tags$b("Mean Squared Error (MSE):"), " 4,722,634,625.73"),
                  tags$li(tags$b("Root Mean Squared Error (RMSE):"), " 68,721.43"),
                  tags$li(tags$b("R-squared:"), " 0.64"),
                  tags$li(tags$p("The model demonstrates a reasonable fit with an R-squared value of 0.64, indicating that 64% of the variance in house prices can be explained by the features."))
                ),
                
                tags$h3("Insights and Observations"),
                tags$ul(
                  tags$li("The high RMSE suggests significant variability in house prices, possibly due to outliers or unmodeled factors."),
                  tags$li("The moderate R-squared value highlights the importance of additional features or complex models to capture remaining variance."),
                  tags$li("Imputation of missing values and feature scaling significantly improved model performance.")
                ),
                
                tags$h3("Visualizations"),
                
                tags$h4("Model Predictions vs Actual Values"),
                tags$img(src = "actual_vs_predictions.png", height = "300px", width = "500px"),
                
                tags$h4("Residuals Distribution"),
                tags$img(src = "residuals_vs_predicted.png", height = "300px", width = "500px")
              )
              )
              
            )
          )
        ),
     
      
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
                title = "Show Data",
                style = "overflow-x: auto;",
                DTOutput("table"),
                width = 9
              ),
              tabPanel(
                title = "Data Summary",
                DTOutput("dataSummary")
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
            uiOutput("drop_feature_ui"),
            actionButton("apply_drop", "Apply")
          )
        ),
        fluidRow(
          # Box for Numerical to Categorical Conversion
          box(
            title = "Convert Numerical to Categorical",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            selectInput(
              inputId = "num_to_cat",
              label = "Select Numerical Variables:",
              choices = NULL,  # Dynamically populated
              multiple = TRUE   # Allow multiple selections
            ),
            actionButton("apply_num_to_cat", "Convert to Categorical")
          ),
          
          # Box for Categorical to Numerical Conversion
          box(
            title = "Convert Categorical to Numerical",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            selectInput(
              inputId = "cat_to_num",
              label = "Select Categorical Variables:",
              choices = NULL,  # Dynamically populated
              multiple = TRUE   # Allow multiple selections
            ),
            actionButton("apply_cat_to_num", "Convert to Numerical")
          )
        )
      ),
      
      tabItem(
        tabName = "preprocessing",
        fluidRow(
          box(
            title = "Handle Missing Values", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 6,
            uiOutput("missing_var_ui"),
            textOutput("missing_percent"),
            uiOutput("missing_method_ui"),
            actionButton("apply_missing", "Apply")
          ),
          box(
            title = "Handle Outliers", 
            status = "warning", 
            solidHeader = TRUE, 
            width = 6,
            uiOutput("outlier_var_ui"),
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
          box(
            title = "Data Transformation",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            uiOutput("transform_var_ui"),
            selectInput(
              inputId = "transformation_method",
              label = "Select Transformation Method:",
              choices = c("Min-Max Scaling", "Z-Score Normalization", "Log Transformation"),
              selected = "Min-Max Scaling"
            ),
            div(
              style = "display: flex; align-items: center; gap: 10px;",
              actionButton("apply_transformation", "Apply"),
              tags$span(
                "⚠️ Be cautious when applying transformations to the target variable; only Log Transformation allows recovery of original values.",
                style = "color: orange; font-size: 12px;"
              )
            )
          ),
          box(
            title = "Encoding Data",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            uiOutput("encoding_var_ui"),
            selectInput(
              inputId = "encoding_method",
              label = "Select Encoding Method:",
              choices = c("Label Encoding", "One-Hot Encoding"),
              selected = "Label Encoding"
            ),
            div(
              style = "display: flex; align-items: center;",  # Align button and message in the same line
              actionButton("apply_encoding", "Apply"),
              tags$span(
                "❗ Avoid applying One-Hot Encoding to the target variable.",
                style = "color: red; margin-left: 10px;"  # Add spacing and red color
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              style = "text-align: center; margin-top: 20px; margin-bottom: 30px;",
              actionButton("submit_data", "Submit", 
                           style = "padding: 10px 20px; font-size: 18px; background-color: #007BFF; color: white;"),
              downloadButton("save_data", "Save", 
                             class = "btn btn-secondary shiny-disabled",
                             style = "padding: 10px 20px; font-size: 18px; margin-left: 20px;")
            )
          )
        ),
        fluidRow(
          box(
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            title = "Training Data",
            style = "overflow-x: auto;",
            DTOutput("table_training")
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
          # Select Target Variable Box
          box(
            title = "Select Target Variable",
            status = "success",
            solidHeader = TRUE,
            width = 4, 
            height = "400px", # Fix component height
            selectInput("target_variable", "Select the Target Variable", choices = NULL),
            actionButton("validate_target", "Validate Target Variable")
          ),
          
          # Target Variable Summary/Histogram Box
          box(
            id = "target_histogram_box",
            title = textOutput("target_box_title"),  # Dynamic title
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            height = "400px", # Fix component height
            uiOutput("target_summary")  # Dynamic content
          ),
          
          # Handle Imbalanced Data Box
          box(
            id = "resampling_box",
            title = "Handle Imbalanced Data",
            status = "warning",
            solidHeader = TRUE,
            width = 4,
            height = "400px", # Fix component height
            uiOutput("resampling_ui")
          )
        ),
        
        fluidRow(
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
        ),
        fluidRow(
          box(
            title = "Model Selection", status = "primary", solidHeader = TRUE, width = 12,
            selectInput(
              "model_choice",
              "Select Model:",
              choices = NULL,
              selected = NULL
            )
          ),
        ),
        fluidRow(
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
        ),
        fluidRow(
          box(
            title = "Train and Save Model", status = "primary", solidHeader = TRUE, width = 12,
            actionButton("train_model", "Train Model", class = "btn-primary"),
            uiOutput("save_model_ui") # Cet élément sera rendu dynamiquement après l'entraînement
          ),
          verbatimTextOutput("model_message"),
        )
      ),
      tabItem(
        tabName = "results",
        fluidRow(
          box(
            title = "Model Metrics",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            actionButton("show_results", "Show Results"),  # Button to trigger results
            tableOutput("model_metrics")  # Table to display metrics
          ),
          box(
            title = "Confusion Matrix",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            plotOutput("conf_matrix_plot")  # Plot to display confusion matrix
          ),
          box(
            title = "ROC Curve", status = "primary", solidHeader = TRUE, width = 4,
            plotOutput("roc_curve"),
            h4("AUC Score:"),
            textOutput("auc_score")
          )
        ),
        fluidRow(
          box(
            title="Feauture Importance",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotOutput("feature_importance_plot")
            
          )
        )
      ),
      tabItem("About", tabName = "about", icon = icon("info-circle"),
               tabPanel("About",
                        fluidPage(
                          h2("Data Science Dashboard: Project README"),
                          
                          h3("Overview"),
                          p("This Shiny application provides a comprehensive platform for data analysis, preprocessing, and machine learning model evaluation. The app is designed to handle end-to-end workflows, from loading datasets to training and evaluating machine learning models."),
                          
                          h3("Features"),
                          tags$ul(
                            tags$li("Data Loading: Upload datasets in CSV or Excel formats."),
                            tags$li("Preprocessing: Handle missing values, scale numerical features, encode categorical variables, and manage outliers."),
                            tags$li("Data Exploration: Perform univariate and bivariate analyses, including histograms, box plots, correlation plots, and advanced metrics."),
                            tags$li("Machine Learning Models: Train, evaluate, and compare models like Linear Regression, Random Forest, Support Vector Machines (SVM), and Decision Trees."),
                            tags$li("Results: Visualize model metrics, confusion matrices, ROC curves, and feature importance.")
                          ),
                          
                          h3("Application Structure"),
                          tags$ul(
                            tags$li("Sidebar Menu"),
                            tags$li("Study Case: Provides an overview of a case study (e.g., predicting California housing prices) with detailed preprocessing and model evaluation steps."),
                            tags$li("Load Data: Allows users to upload datasets and explore their structure."),
                            tags$li("Preprocessing: Offers tools for handling missing values, outliers, and feature transformations."),
                            tags$li("Analyse Data: Enables univariate and bivariate analyses using various visualizations."),
                            tags$li("ML Models: Facilitates model selection, parameter tuning, and training."),
                            tags$li("Results: Displays metrics and visualizations for trained models."),
                            tags$li("About: Contains project details and acknowledgments.")
                          ),
                          
                          h3("Detailed Features"),
                          h4("1. Study Case"),
                          tags$ul(
                            tags$li("Overview: Introduces the problem and dataset."),
                            tags$li("Dataset Description: Details the features, target variable, and missing data."),
                            tags$li("Preprocessing: Summarizes methods like scaling and encoding."),
                            tags$li("Modeling: Describes the model training process and evaluation metrics."),
                            tags$li("Insights: Highlights observations and model performance insights.")
                          ),
                          
                          h4("2. Load Data"),
                          p("The Load Data section allows users to upload and explore datasets. The key functionalities include:"),
                          tags$ul(
                            tags$li(HTML("File Upload: Supports CSV and Excel files. <br> For Excel files, the app checks for a specific sheet named 'Data.' <br> Displays error notifications for unsupported file types or missing sheets.")),
                            tags$li("File Details:Displays metadata such as file name, format, number of instances (rows), features (columns), and counts of categorical and numerical features."),
                            tags$li("Dataset Exploration: Provides a scrollable and searchable table view of the uploaded dataset.Displays a summary of each column, including type (numerical or categorical), missing values, and statistical measures like mean, median, and standard deviation."),
                            tags$li("Feature Management: Drop Feautures: Dynamically select and remove specific features (columns) from the dataset."),
                            tags$li("Convert Features: Converts selected numerical variables to categorical or vice versa.")
                          ),
                          
                          h4("3. Preprocessing"),
                          p("The Preprocessing section provides advanced tools for cleaning and preparing the dataset:"),
                          tags$ul(
                            tags$li("Handling Missing Values: Options include suppression, replacing with mode, median, or mean."),
                            tags$li("Managing Outliers: Detects outliers using the IQR method, with options to remove or replace outliers."),
                            tags$li("Data Transformation: Offers methods like Min-Max Scaling, Z-Score Normalization, and Log Transformation."),
                            tags$li("Data Encoding: Options for Label Encoding and One-Hot Encoding for categorical variables.")
                          ),
                          
                          h4("4. Analyse Data"),
                          p("The Analyse Data section provides tools for univariate and bivariate analysis:"),
                          tags$ul(
                            tags$li("Univariate Analysis: Includes histograms, box plots, pie charts, and summary statistics."),
                            tags$li("Bivariate Analysis: Includes correlation plots, correlation matrices, and parallel box plots."),
                            tags$li("Contingency Tables & Cramér's V: Summarize relationships between categorical variables.")
                          ),
                          
                          h4("5. ML Models"),
                          p("The ML Models section offers extensive capabilities for training and evaluating machine learning models:"),
                          tags$ul(
                            tags$li("Target Variable Selection: Dynamically select the target variable and handle missing values."),
                            tags$li("Resampling Techniques: Includes undersampling and oversampling methods."),
                            tags$li("Model Selection: Offers models based on the target variable type, with configurable parameters."),
                            tags$li("Model Training: Supports multiple models, including SVM, Random Forest, Linear Regression, and Decision Trees.")
                          ),
                          
                          h4("6. Results"),
                          p("The Results section shows model metrics and visualizations:"),
                          tags$ul(
                            tags$li("Model Metrics: Includes accuracy, precision, recall, F1 score for classification models, and MSE, RMSE for regression models."),
                            tags$li("Confusion Matrix: Visualizes predictions vs. actual values."),
                            tags$li("ROC Curve: Plots ROC curves with AUC scores."),
                            tags$li("Feature Importance: Displays the most important features contributing to model predictions.")
                          ),
                          
                          h3("How to Use"),
                          tags$ul(
                            tags$li("Start the App: Run the application using `shiny::runApp('path_to_app')`."),
                            tags$li("Upload Data: Navigate to 'Load Data' to upload your dataset."),
                            tags$li("Preprocess Data: Use 'Preprocessing' to clean and prepare the data."),
                            tags$li("Analyze Data: Explore patterns and relationships under 'Analyse Data.'"),
                            tags$li("Train Models: In 'ML Models,' select and train a model."),
                            tags$li("View Results: Check 'Results' for metrics and visualizations.")
                          ),
                          
                          h3("Technical Details"),
                          p("Libraries Used: shiny, shinydashboard, bslib, shinyjs, DT, plotly, ggplot2, randomForest, caret, e1071, rpart."),
                          
                          h3("System Requirements"),
                          p("R Version: 4.0 or higher. Ensure all required libraries are installed."),
                          
                          h3("Acknowledgments"),
                          p("This project is designed for educational purposes to demonstrate the application of data preprocessing, exploratory data analysis, and machine learning workflows using R Shiny.")
                        )
               )
      )
      
    )
  )
)