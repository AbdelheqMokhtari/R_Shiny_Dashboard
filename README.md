## Overview
This Shiny application provides a comprehensive platform for data analysis, preprocessing, and machine learning model evaluation. The app is designed to handle end-to-end workflows, from loading datasets to training and evaluating machine learning models.

## Features
- **Data Loading**: Upload datasets in CSV or Excel formats.
- **Preprocessing**: Handle missing values, scale numerical features, encode categorical variables, and manage outliers.
- **Data Exploration**: Perform univariate and bivariate analyses, including histograms, box plots, correlation plots, and advanced metrics.
- **Machine Learning Models**: Train, evaluate, and compare models like Linear Regression, Random Forest, Support Vector Machines (SVM), and Decision Trees.
- **Results**: Visualize model metrics, confusion matrices, ROC curves, and feature importance.

## Application Structure
### Sidebar Menu
- **Study Case**: Provides an overview of a case study (e.g., predicting California housing prices) with detailed preprocessing and model evaluation steps.
- **Load Data**: Allows users to upload datasets and explore their structure.
- **Preprocessing**: Offers tools for handling missing values, outliers, and feature transformations.
- **Analyse Data**: Enables univariate and bivariate analyses using various visualizations.
- **ML Models**: Facilitates model selection, parameter tuning, and training.
- **Results**: Displays metrics and visualizations for trained models.
- **About**: Contains project details and acknowledgments.

## Detailed Features
### 1. Study Case
- **Overview**: Introduces the problem and dataset.
- **Dataset Description**: Details the features, target variable, and missing data.
- **Preprocessing**: Summarizes methods like scaling and encoding.
- **Modeling**: Describes the model training process and evaluation metrics.
- **Insights**: Highlights observations and model performance insights.

### 2. Load Data
The Load Data section allows users to upload and explore datasets. The key functionalities include:
- **File Upload**:
  - Supports CSV and Excel files.
  - For Excel files, the app checks for a specific sheet named "Data."
  - Displays error notifications for unsupported file types or missing sheets.
- **File Details**:
  - Displays metadata such as file name, format, number of instances (rows), features (columns), and counts of categorical and numerical features.
- **Dataset Exploration**:
  - Provides a scrollable and searchable table view of the uploaded dataset.
  - Displays a summary of each column, including type (numerical or categorical), missing values, and statistical measures like mean, median, and standard deviation.
- **Feature Management**:
  - **Drop Features**: Dynamically select and remove specific features (columns) from the dataset. Feedback is provided to confirm dropped features.
  - **Convert Features**:
    - Numerical to Categorical: Converts selected numerical variables to categorical by changing their data type to factors.
    - Categorical to Numerical: Converts selected categorical variables to numerical, restoring original values if available or attempting numeric conversion for compatible data.

### 3. Preprocessing
The Preprocessing section provides advanced tools for cleaning and preparing the dataset:
- **Handling Missing Values**
  - Variable Selection: Dynamically select a variable to manage its missing values.
  - Methods:
    - Suppression: Removes rows containing missing values for the selected variable.
    - Replace with Mode: Fills missing values with the most frequent value.
    - Replace with Median: Fills missing values with the median of the variable.
    - Replace with Mean: Fills missing values with the mean of the variable.
- **Managing Outliers**
  - Variable Selection: Supports numerical variables for outlier handling.
  - Outlier Detection: Uses the Interquartile Range (IQR) method to identify outliers.
  - Methods:
    - Remove Outliers: Deletes rows containing outliers.
    - Replace with Median: Replaces outliers with the median value of the variable.
    - Replace with Mean: Replaces outliers with the mean value of the variable.
- **Data Transformation**
  - Variable Selection: Dynamically select one or more numerical variables for transformation.
  - Transformation Methods:
    - Min-Max Scaling: Scales values to a range of [0, 1].
    - Z-Score Normalization: Standardizes values by centering around the mean and scaling to unit variance.
    - Log Transformation: Applies log transformation to handle skewed distributions (adds 1 to avoid log of zero).
- **Data Encoding**
  - Variable Selection: Dynamically identify categorical variables for encoding.
  - Encoding Methods:
    - Label Encoding: Converts categorical values to integer labels.
    - One-Hot Encoding: Expands categorical variables into multiple binary columns.

### 4. Analyse Data
The Analyse Data section provides comprehensive tools for univariate and bivariate analysis:
- **Univariate Analysis**
  - Histograms, Box Plots, Pie Charts, and Summary Statistics.
- **Bivariate Analysis**
  - Correlation Plots, Correlation Coefficients, Heatmaps, Parallel Box Plots, and more.

### 5. ML Models
The ML Models section offers extensive capabilities for training and evaluating machine learning models:
- **Target Variable Selection**: Dynamically select the target variable, handle missing values, and apply label encoding.
- **Resampling Techniques**: Balance datasets using undersampling or oversampling.
- **Data Splitting**: Supports holdout split with configurable percentages.
- **Model Selection and Training**: Train models like SVM, Random Forest, Linear Regression, and Decision Trees with customizable parameters.

### 6. Results
The Results section provides a detailed evaluation of model performance:
- **Classification Models**: Confusion Matrix, Metrics Summary, ROC Curve, and AUC.
- **Regression Models**: Metrics Summary, Prediction vs. Actual Plot, and Residuals Analysis.

## How to Use
1. **Start the App**: Run the application in RStudio or using the Shiny Server.
   ```
   shiny::runApp("path_to_app")
   ```
2. **Upload Data**: Navigate to the "Load Data" tab and upload your dataset.
3. **Preprocess Data**: Use the "Preprocessing" tab to clean and prepare the data.
4. **Analyze Data**: Explore patterns and relationships using the "Analyse Data" tab.
5. **Train Models**: Go to "ML Models," select a model, configure parameters, and train it.
6. **View Results**: Check the "Results" tab for metrics and visualizations.

## Technical Details
### Libraries Used
- **UI Libraries**: shiny, shinydashboard, bslib, shinyjs
- **Data Handling**: DT, reactable
- **Visualization**: plotly, ggplot2, corrplot
- **Machine Learning**: randomForest, caret, e1071, rpart, pROC

### System Requirements
- **R Version**: 4.0 or higher.
- **Additional Packages**: Ensure all required libraries are installed.

## Application URL
https://9229dp-abdelheq-mokhtari.shinyapps.io/Rshiny-DB-DataScience/

## Acknowledgments
This project is designed for educational purposes to demonstrate the application of data preprocessing, exploratory data analysis, and machine learning workflows using R Shiny.

## Contributors
- Abdelheq Mokhtari
- Mohamed Raouf Razi
- Akram Chaabnia
