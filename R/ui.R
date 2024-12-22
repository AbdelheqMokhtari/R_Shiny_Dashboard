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
                title = "Housing Prices",
                htmlOutput("study_case_content")
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
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = "📊 Data Science Dashboard: Project README",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            div(style = "overflow-y: auto; max-height: 80vh;", # Enables scrolling
                HTML('
<h2>Overview</h2>
<p>This Shiny application provides a comprehensive platform for data analysis, preprocessing, and machine learning model evaluation. The app is designed to handle end-to-end workflows, from loading datasets to training and evaluating machine learning models.</p>

<h2>Features</h2>
<ul>
  <li>Data Loading: Upload datasets in CSV or Excel formats.</li>
  <li>Preprocessing: Handle missing values, scale numerical features, encode categorical variables, and manage outliers.</li>
  <li>Data Exploration: Perform univariate and bivariate analyses, including histograms, box plots, correlation plots, and advanced metrics.</li>
  <li>Machine Learning Models: Train, evaluate, and compare models like Linear Regression, Random Forest, Support Vector Machines (SVM), and Decision Trees.</li>
  <li>Results: Visualize model metrics, confusion matrices, ROC curves, and feature importance.</li>
</ul>

<h2>Application Structure</h2>
<h3>Sidebar Menu</h3>
<ul>
  <li>Study Case: Provides an overview of a case study (e.g., predicting California housing prices) with detailed preprocessing and model evaluation steps.</li>
  <li>Load Data: Allows users to upload datasets and explore their structure.</li>
  <li>Preprocessing: Offers tools for handling missing values, outliers, and feature transformations.</li>
  <li>Analyse Data: Enables univariate and bivariate analyses using various visualizations.</li>
  <li>ML Models: Facilitates model selection, parameter tuning, and training.</li>
  <li>Results: Displays metrics and visualizations for trained models.</li>
  <li>About: Contains project details and acknowledgments.</li>
</ul>

<h2>Detailed Features</h2>
<h3>1. Study Case</h3>
<ul>
  <li>Overview: Introduces the problem and dataset.</li>
  <li>Dataset Description: Details the features, target variable, and missing data.</li>
  <li>Preprocessing: Summarizes methods like scaling and encoding.</li>
  <li>Modeling: Describes the model training process and evaluation metrics.</li>
  <li>Insights: Highlights observations and model performance insights.</li>
</ul>

<h3>2. Load Data</h3>
<p>The Load Data section allows users to upload and explore datasets. The key functionalities include:</p>
<ul>
  <li>**File Upload:**
    <ul>
      <li>Supports CSV and Excel files.</li>
      <li>For Excel files, the app checks for a specific sheet named "Data."</li>
      <li>Displays error notifications for unsupported file types or missing sheets.</li>
    </ul>
  </li>
  <li>**File Details:**
    <ul>
      <li>Displays metadata such as file name, format, number of instances (rows), features (columns), and counts of categorical and numerical features.</li>
    </ul>
  </li>
  <li>**Dataset Exploration:**
    <ul>
      <li>Provides a scrollable and searchable table view of the uploaded dataset.</li>
      <li>Displays a summary of each column, including type (numerical or categorical), missing values, and statistical measures like mean, median, and standard deviation.</li>
    </ul>
  </li>
  <li>**Feature Management:**
    <ul>
      <li>Drop Features: Dynamically select and remove specific features (columns) from the dataset. Feedback is provided to confirm dropped features.</li>
      <li>Convert Features:
        <ul>
          <li>Numerical to Categorical: Converts selected numerical variables to categorical by changing their data type to factors.</li>
          <li>Categorical to Numerical: Converts selected categorical variables to numerical, restoring original values if available or attempting numeric conversion for compatible data.</li>
        </ul>
        <li>Displays confirmation dialogs for successful conversions and error dialogs for variables that could not be converted.</li>
      </li>
    </ul>
  </li>
</ul>

<h3>3. Preprocessing</h3>
<p>The Preprocessing section provides advanced tools for cleaning and preparing the dataset:</p>
<ul>
  <li>**Handling Missing Values**
    <ul>
      <li>Variable Selection: Dynamically select a variable to manage its missing values.</li>
      <li>Methods:
        <ul>
          <li>Suppression: Removes rows containing missing values for the selected variable.</li>
          <li>Replace with Mode: Fills missing values with the most frequent value.</li>
          <li>Replace with Median: Fills missing values with the median of the variable.</li>
          <li>Replace with Mean: Fills missing values with the mean of the variable.</li>
        </ul>
      </li>
      <li>Feedback: The app provides detailed notifications and modal dialogs summarizing the changes made, including the number of missing values handled and the chosen method.</li>
    </ul>
  </li>
  <li>**Managing Outliers**
    <ul>
      <li>Variable Selection: Supports numerical variables for outlier handling.</li>
      <li>Outlier Detection: Uses the Interquartile Range (IQR) method to identify outliers.</li>
      <li>Methods:
        <ul>
          <li>Remove Outliers: Deletes rows containing outliers.</li>
          <li>Replace with Median: Replaces outliers with the median value of the variable.</li>
          <li>Replace with Mean: Replaces outliers with the mean value of the variable.</li>
        </ul>
      </li>
      <li>Feedback: Provides a summary of the number of outliers detected and handled, along with the selected method, through modal dialogs and notifications.</li>
    </ul>
  </li>
  <li>**Data Transformation**
    <ul>
      <li>Variable Selection: Dynamically select one or more numerical variables for transformation.</li>
      <li>Transformation Methods:
        <ul>
          <li>Min-Max Scaling: Scales values to a range of [0, 1].</li>
          <li>Z-Score Normalization: Standardizes values by centering around the mean and scaling to unit variance.</li>
          <li>Log Transformation: Applies log transformation to handle skewed distributions (adds 1 to avoid log of zero).</li>
        </ul>
      </li>
      <li>Feedback: Summarizes the transformations applied, specifying the variables and methods, in a detailed modal dialog.</li>
    </ul>
  </li>
  <li>**Data Encoding**
    <ul>
      <li>Variable Selection: Dynamically identify categorical variables for encoding.</li>
      <li>Encoding Methods:
        <ul>
          <li>Label Encoding: Converts categorical values to integer labels.</li>
          <li>One-Hot Encoding: Expands categorical variables into multiple binary columns.</li>
        </ul>
      </li>
      <li>Feedback: Confirms successful encoding through notifications and modal dialogs.</li>
    </ul>
  </li>
</ul>

<h3>4. Analyse Data</h3>
<p>The Analyse Data section provides comprehensive tools for univariate and bivariate analysis:</p>
<ul>
  <li>**Univariate Analysis**
    <ul>
      <li>Histograms: Visualize the distribution of a selected numerical variable. The bin width is customizable.</li>
      <li>Box Plots: Summarize the spread, median, and outliers of a numerical variable.</li>
      <li>Pie Charts: Display the proportions of categories for categorical variables or numerical variables with few unique values (treated as categories).</li>
      <li>Summary Statistics: View summary statistics, including mean, median, min, max, and standard deviation, for a selected variable.</li>
    </ul>
  </li>
  <li>**Bivariate Analysis**
    <ul>
      <li>Correlation Plots: Scatter plots for visualizing relationships between two variables, with optional regression lines for numeric pairs.</li>
      <li>Correlation Coefficients: Calculate and display the correlation coefficient (r) for two numeric variables.</li>
      <li>Correlation Matrices: Generate a heatmap of correlations between all numeric variables in the dataset.</li>
      <li>Parallel Box Plots: Display box plots of a numeric variable grouped by a categorical variable.</li>
      <li>Bar Plots: Show column profiles, visualizing proportions of one categorical variable within levels of another.</li>
      <li>Contingency Tables: Summarize the joint distribution of two categorical variables.</li>
      <li>Cramérs V: Quantify the association between two categorical variables (including label-encoded).</li>
      <li> Correlation Ratios: Measure the strength of the relationship between a categorical and a numerical variable.</li>
      </ul>
      </li>
      <h3>5. ML Models</h3>
<p>The ML Models section offers extensive capabilities for training and evaluating machine learning models:</p>

<h4>Target Variable Selection</h4>
<ul>
  <li>Dynamically select the target variable from the dataset.</li>
  <li>Handle missing values in the target variable automatically.</li>
  <li>Apply label encoding to prepare categorical target variables.</li>
  <li>Automatically remove the target variable from the feature set after selection.</li>
  <li>Feedback includes notifications and visualizations of target distributions.</li>
</ul>

<h4>Resampling Techniques</h4>
<ul>
  <li>Available Techniques:</li>
    <ul>
      <li>Undersampling: Balances the dataset by reducing instances of overrepresented classes.</li>
      <li>Oversampling: Increases instances of underrepresented classes.</li>
    </ul>
  <li>Confirmation dialogs ensure resampling is performed intentionally.</li>
  <li>Automatically updates the training dataset and target variable after resampling.</li>
</ul>

<h4>Data Splitting</h4>
<ul>
  <li>Supports Holdout split:</li>
    <ul>
      <li>Configurable train-test split percentages.</li>
      <li>Automatically generates training and testing datasets.</li>
      <li>Provides a detailed post-split analysis of target distribution.</li>
      <li>Validates that split percentages sum to 100%.</li>
    </ul>
</ul>

<h4>Model Selection</h4>
<ul>
  <li>Models are dynamically filtered based on the target variable type:</li>
    <ul>
      <li>Categorical Targets: Support for SVM, Logistic Regression, Decision Trees, and Random Forest.</li>
      <li>Numerical Targets: Support for Linear Regression, Decision Trees, and Random Forest.</li>
    </ul>
  <li>Further refinement of options based on variable properties (e.g., ordinal vs. continuous).</li>
</ul>

<h4>Model Training</h4>
<ul>
  <li>Supports multiple models with customizable parameters:</li>
    <ul>
      <li>SVM: Configurable kernel type and cost parameter.</li>
      <li>Random Forest: Adjustable number of trees.</li>
      <li>Linear Regression: Trains using numeric predictors.</li>
      <li>Decision Trees: Configurable depth and splitting criterion.</li>
    </ul>
  <li>Automatically evaluates and saves trained models.</li>
  <li>Includes UI for downloading trained models as .rds files.</li>
</ul>

<h3>6. Results</h3>
<p>The Results section provides a detailed evaluation of model performance:</p>

<h4>Classification Models (e.g., SVM, Random Forest)</h4>
<ul>
  <li>Confusion Matrix:</li>
    <ul>
      <li>Displays the actual vs. predicted classifications.</li>
      <li>Automatically calculates metrics like accuracy, precision, recall, and F1 score.</li>
    </ul>
  <li>Metrics Summary:</li>
    <ul>
      <li>Displays key performance metrics in a tabular format for easier interpretation.</li>
    </ul>
  <li>ROC Curve and AUC:</li>
    <ul>
      <li>Plots the Receiver Operating Characteristic (ROC) curve.</li>
      <li>Computes the Area Under the Curve (AUC) for binary classification models.</li>
    </ul>
  <li>Confusion Matrix Visualization:</li>
    <ul>
      <li>Heatmap representation for easy visualization of prediction errors.</li>
    </ul>
</ul>

<h4>Regression Models (e.g., Linear Regression, Decision Tree)</h4>
<ul>
  <li>Metrics Summary:</li>
    <ul>
      <li>Displays Mean Squared Error (MSE), Root Mean Squared Error (RMSE), and R-squared metrics.</li>
    </ul>
  <li>Prediction vs. Actual Plot:</li>
    <ul>
      <li>Scatter plot comparing predicted values with actual target values.</li>
      <li>Includes a reference line to evaluate prediction accuracy.</li>
    </ul>
  <li>Residuals Analysis:</li>
    <ul>
      <li>Residuals vs. Predicted plot to assess model fit.</li>
      <li>Highlights trends and potential issues like heteroscedasticity.</li>
    </ul>
  <li>Feature Importance:</li>
    <ul>
      <li>Model-Specific Visualization:</li>
        <ul>
          <li>Random Forest: Displays feature importance using mean decrease in impurity.</li>
          <li>Decision Tree: Highlights the most influential variables in splits.</li>
          <li>Linear Regression: Shows absolute values of coefficients as a proxy for importance.</li>
          <li>SVM (Linear Kernel): Uses the magnitude of coefficients to rank features.</li>
        </ul>
    </ul>
</ul>

<h3>How to Use</h3>
<p>Start the App: Run the application in RStudio or using the Shiny Server.</p>

<pre><code>shiny::runApp("path_to_app")</code></pre>

<p>Upload Data: Navigate to the "Load Data" tab and upload your dataset.</p>
<p>Preprocess Data: Use the "Preprocessing" tab to clean and prepare the data.</p>
<p>Analyze Data: Explore patterns and relationships using the "Analyse Data" tab.</p>
<p>Train Models: Go to "ML Models," select a model, configure parameters, and train it.</p>
<p>View Results: Check the "Results" tab for metrics and visualizations.</p>

<h3>Technical Details</h3>
<h4>Libraries Used</h4>
<ul>
  <li>UI Libraries: shiny, shinydashboard, bslib, shinyjs</li>
  <li>Data Handling: DT, reactable</li>
  <li>Visualization: plotly, ggplot2, corrplot</li>
  <li>Machine Learning: randomForest, caret, e1071, rpart, pROC</li>
</ul>

<h4>System Requirements</h4>
<ul>
  <li>R Version: 4.0 or higher.</li>
  <li>Additional Packages: Ensure all required libraries are installed.</li>
</ul>

<h3>Acknowledgments</h3>
<p>This project is designed for educational purposes to demonstrate the application of data preprocessing, exploratory data analysis, and machine learning workflows using R Shiny.</p>

          ')
            )
          )
        )
      )
      
      
    )
  )
)