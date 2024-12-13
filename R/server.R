library(shiny)
library(shinydashboard)
library(bslib)
library(readxl)  
library(DT)
library(plotly)
library(caret)
library(pROC)

server <- function(input, output, session) {
  
  # Reactive data storage
  uploaded_data <- reactiveVal(NULL)
  column_types <- reactiveVal(NULL)
  derived_columns <- reactiveVal(character(0))  # New reactive value to track derived columns
  
  # Reactive value to store original column names
  original_columns <- reactiveVal(NULL)
  
  # Main observer for file input
  observeEvent(input$fileInput, {
    req(input$fileInput)
    
    data <- load_file_data(input$fileInput)  # Load the file data
    if (is.null(data)) return(NULL)  # Stop if data couldn't be loaded
    
    uploaded_data(data)  # Store the loaded data in a reactive value
    column_types(sapply(data, class))  # Store column types
    original_columns(names(data))  # Store original column names
    update_ui_after_file_upload(data, input$fileInput)  # Update the UI components
  })
  
  # Function to load data based on file type
  load_file_data <- function(fileInput) {
    file_ext <- tools::file_ext(fileInput$name)
    
    if (file_ext == "csv") {
      return(read.csv(fileInput$datapath))
    } else if (file_ext %in% c("xlsx", "xls")) {
      sheet_names <- excel_sheets(fileInput$datapath)
      if ("Data" %in% sheet_names) {
        return(read_excel(fileInput$datapath, sheet = "Data"))
      } else {
        showNotification("Sheet 'Data' not found in the file.", type = "error")
        return(NULL)
      }
    } else {
      showNotification("Unsupported file type", type = "error")
      return(NULL)
    }
  }
  
  # Function to render Drop Feature UI
  render_drop_feature_ui <- function() {
    renderUI({
      req(uploaded_data())
      selectInput(
        "drop_feature",
        "Select Feature to Drop:",
        choices = original_columns(),  # Use original columns
        selected = NULL
      )
    })
  }
  
  # Reactive values to track target variable and its state
  target_variable <- reactiveVal(NULL)
  target_validated <- reactiveVal(FALSE)  # To track if target is validated
  
  # Dynamic rendering of options for transformations
  output$target_variable_options <- renderUI({
    req(uploaded_data())
    req(input$target_variable)
    
    # Get column type
    variable_type <- column_types()[[input$target_variable]]
    
    if (variable_type %in% c("factor", "character")) {
      checkboxInput("apply_label_encoding", "Apply Label Encoding?", value = FALSE)
    } else if (variable_type %in% c("numeric", "integer")) {
      checkboxInput("apply_log_transformation", "Apply Log Transformation?", value = FALSE)
    } else {
      NULL  # No transformation options for other types
    }
  })
  
  # Observe when the validate button is clicked
  observeEvent(input$validate_target, {
    req(input$target_variable)
    req(!target_validated())  # Ensure it hasn't already been validated
    
    # Mark the target as validated
    target_variable(input$target_variable)
    target_validated(TRUE)
    
    # Disable inputs to prevent further changes
    shinyjs::disable("target_variable")
    shinyjs::disable("apply_label_encoding")
    shinyjs::disable("apply_log_transformation")
    shinyjs::disable("validate_target")
    
    # Extract target variable and remove it from the dataset
    data <- uploaded_data()
    Y <- data[[input$target_variable]]
    assign("Y", Y, envir = .GlobalEnv)  # Store target variable (can be improved)
    data <- data[, !names(data) %in% input$target_variable, drop = FALSE]
    uploaded_data(data)
    
    showNotification("Target variable validated and stored as 'Y'.", type = "message")
  })
  
  
  
  # Function to render uploaded file name
  render_file_name <- function(fileInput) {
    renderText({
      paste("Uploaded file:", fileInput$name)
    })
  }
  
  # Function to render file details
  render_file_details <- function(fileInput) {
    renderText({
      req(uploaded_data())
      
      num_features <- ncol(uploaded_data())
      num_instances <- nrow(uploaded_data())
      num_categorical <- sum(sapply(uploaded_data(), function(x) is.factor(x) || is.character(x)))
      file_ext <- tools::file_ext(fileInput$name)
      
      details <- paste(
        "File Name: ", fileInput$name, "\n",
        "File format: ", file_ext, "\n",
        "Number of features (columns): ", num_features, "\n",
        "Number of instances (rows): ", num_instances, "\n",
        "Number of categorical variables: ", num_categorical
      )
      
      return(details)
    })
  }
  
  
  # Function to update UI components after file upload
  update_ui_after_file_upload <- function(data, fileInput) {
    output$drop_feature_ui <- render_drop_feature_ui()
    output$fileName <- render_file_name(fileInput)
    output$fileDetails <- render_file_details(fileInput) 
  }
  
  
  
  # Helper function to drop the selected feature
  drop_selected_feature <- function(selected_feature) {
    updated_data <- uploaded_data()[, !(names(uploaded_data()) %in% selected_feature)]
    uploaded_data(updated_data)  # Update the dataset
    
    # Remove dropped features from derived columns
    derived_columns(setdiff(derived_columns(), selected_feature))
    
    update_ui_components()  # Update UI components
  }
  
  # Data Table Output: Display data preview
  output$dataHead <- renderDT({
    req(uploaded_data())
    head_rows <- as.numeric(input$n_rows)
    datatable(head(uploaded_data(), n = head_rows), options = list(pageLength = head_rows))
  })
  
  # Data Summary Output: Display the summary of the data
  output$dataSummary <- renderPrint({
    req(uploaded_data())
    str(uploaded_data())
  })
  
  # Observe feature drop button click
  observeEvent(input$apply_drop, {
    req(uploaded_data())
    selected_feature <- input$drop_feature
    
    if (!is.null(selected_feature) && selected_feature != "") {
      data <- uploaded_data()
      data <- data[, !(names(data) %in% selected_feature)]  # Drop the selected feature
      uploaded_data(data)  # Update the reactive dataset
      
      # Update original and derived columns
      original_columns(setdiff(original_columns(), selected_feature))
      derived_columns(setdiff(derived_columns(), selected_feature))
      
      update_ui_components()
    } else {
      showNotification("Please select a valid feature to drop.", type = "warning")
    }
  })
  
  
  # Function to update UI components dynamically
  update_ui_components <- function() {
    # Update drop feature UI
    output$drop_feature_ui <- renderUI({
      req(uploaded_data())
      selectInput("drop_feature", "Select Feature to Drop:", 
                  choices = names(uploaded_data()), 
                  selected = NULL)
    })
    
    # Update target variable UI
    output$target_variable <- renderUI({
      req(uploaded_data())
      selectInput("target_variable", "Select the Target Variable:", 
                  choices = names(uploaded_data()), 
                  selected = NULL)
    })
  }
  
  # Display updated data table
  output$table <- renderDT({
    req(uploaded_data())
    datatable(uploaded_data(), options = list(pageLength = 5))
  })
  
  # Display the uploaded file name in the first box
  output$fileName <- renderText({
    paste("Uploaded file:", input$fileInput$name)  # Display file name
  })
  
  # Display the details of the file 
  output$fileDetails <- renderText({
    req(uploaded_data())  # Ensure data is available
    
    # Get the file format and details
    num_features <- ncol(uploaded_data())
    num_instances <- nrow(uploaded_data())
    num_categorical <- sum(sapply(uploaded_data(), function(x) is.factor(x) || is.character(x)))  # Count categorical columns
    file_ext <- tools::file_ext(input$fileInput$name)  # Get file extension from the uploaded file
    
    # Format the details to display
    details <- paste(
      "File Name: ", input$fileInput$name, "\n",  # Access the file name here
      "File format: ", file_ext, "\n",
      "Number of features (columns): ", num_features, "\n",
      "Number of instances (rows): ", num_instances, "\n",
      "Number of categorical variables: ", num_categorical
    )
    
    return(details)
  })
  
  
  # Data Table Output: Display data preview
  output$dataHead <- renderDT({
    req(uploaded_data())
    head_rows <- as.numeric(input$n_rows)
    datatable(head(uploaded_data(), n = head_rows), options = list(pageLength = head_rows))
  })
  
  # Data Summary Output: Display the summary of the data
  output$dataSummary <- renderPrint({
    req(uploaded_data())
    str(uploaded_data())
  })
  
  # Render the variable classification table
  output$var_classification_table <- renderDT({
    req(uploaded_data())  # Ensure the data is available
    
    # Separate numeric and categorical variables
    numeric_vars <- names(uploaded_data())[sapply(uploaded_data(), is.numeric)]
    categorical_vars <- names(uploaded_data())[sapply(uploaded_data(), function(x) is.factor(x) || is.character(x))]
    
    # Create a data frame to show the variables in two columns
    classification_df <- data.frame(
      `Numeric Variables` = ifelse(length(numeric_vars) > 0, paste(numeric_vars, collapse = ", "), "None"),
      `Categorical Variables` = ifelse(length(categorical_vars) > 0, paste(categorical_vars, collapse = ", "), "None")
    )
    
    # Display the classification table
    datatable(classification_df, options = list(pageLength = 5))
  })
  
  output$missing_var_ui <- renderUI({
    req(uploaded_data()) # Ensure data is loaded
    selectInput(
      inputId = "missing_var", 
      label = "Select Variable:", 
      choices = names(uploaded_data()), # Dynamically get column names
      selected = NULL
    )
  })
  
  
  # Dynamic Selection of missing values
  output$missing_percent <- renderText({
    req(input$missing_var, uploaded_data())
    var <- uploaded_data()[[input$missing_var]]
    percent <- sum(is.na(var)) / length(var) * 100
    paste("Missing Percent:", round(percent, 2), "%")
  })
  
  current_missing_var <- reactiveVal(NULL)
  
  # Dynamic Selection for missing values 
  
  output$missing_var_ui <- renderUI({
    req(uploaded_data())
    
    data <- uploaded_data()
    variables <- setdiff(names(data), derived_columns())  # Exclude derived columns
    
    selectInput(
      inputId = "missing_var", 
      label = "Select Variable to Handle Missing Values:", 
      choices = variables, 
      selected = NULL
    )
  })
  
  # Dynamic selection for outliers 
  
  output$outlier_var_ui <- renderUI({
    req(uploaded_data())
    numeric_vars <- setdiff(names(uploaded_data())[sapply(uploaded_data(), is.numeric)], derived_columns())
    
    if (length(numeric_vars) > 0) {
      selectInput(
        inputId = "outlier_var", 
        label = "Select Numerical Variable:", 
        choices = numeric_vars, 
        selected = NULL
      )
    } else {
      tags$p("No numerical variables available for outlier handling.", style = "color: red;")
    }
  })
  
  
  # Dynamically selecting Data Transformation
  output$transform_var_ui <- renderUI({
    req(uploaded_data())
    
    data <- uploaded_data()
    # Dynamically identify numerical columns excluding derived columns
    numeric_vars <- setdiff(names(data)[sapply(data, is.numeric)], derived_columns())
    
    selectizeInput(
      inputId = "transform_var",
      label = "Select Numerical Variables for Transformation:",
      choices = numeric_vars,
      selected = NULL, # No variable selected by default
      multiple = TRUE
    )
  })
  
  # Data Encoding Variable Selection
  output$encoding_var_ui <- renderUI({
    req(uploaded_data())
    
    data <- uploaded_data()
    # Dynamically identify categorical columns in the current dataset
    categorical_vars <- names(data)[sapply(data, function(col) is.factor(col) || is.character(col))]
    
    selectizeInput(
      inputId = "encoding_var",
      label = "Select Categorical Variables for Encoding:",
      choices = categorical_vars,
      selected = NULL, # No variable selected by default
      multiple = TRUE
    )
  })
  
  
  # Apply Logic of Pre-processing 
  
  # Observe the selected variable and update the reactive value
  observeEvent(input$missing_var, {
    current_missing_var(input$missing_var)
  })
  
  # Missing value handling logic
  observeEvent(input$apply_missing, {
    req(input$missing_var, input$missing_method, uploaded_data())
    
    data <- uploaded_data()
    var <- input$missing_var
    missing_count <- sum(is.na(data[[var]])) # Count missing values
    
    if (missing_count > 0) {
      if (input$missing_method == "Suppression") {
        # Remove rows with missing values
        data <- data[!is.na(data[[var]]), ]
      } else if (input$missing_method == "Replace with Mode") {
        mode_val <- as.numeric(names(sort(table(data[[var]]), decreasing = TRUE)[1]))
        data[[var]][is.na(data[[var]])] <- mode_val
      } else if (input$missing_method == "Replace with Median") {
        median_val <- median(data[[var]], na.rm = TRUE)
        data[[var]][is.na(data[[var]])] <- median_val
      } else if (input$missing_method == "Replace with Mean") {
        mean_val <- mean(data[[var]], na.rm = TRUE)
        data[[var]][is.na(data[[var]])] <- mean_val
      }
      
      # Update the reactive storage
      uploaded_data(data)
      
      # Show popup message with rows affected
      showModal(
        modalDialog(
          title = "Missing Values Handled",
          paste("The variable", var, "had", missing_count, "missing values."),
          if (input$missing_method == "Suppression") {
            paste(missing_count, "rows were removed.")
          } else {
            "The missing values have been replaced."
          },
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    } else {
      showNotification("No missing values in the selected variable.", type = "warning")
    }
  })
  
  # handling outliers logic 
  
  observeEvent(input$apply_outliers, {
    req(input$outlier_var, input$outlier_method, uploaded_data())
    
    data <- uploaded_data()
    var <- input$outlier_var
    
    if (is.numeric(data[[var]])) {
      # Detect outliers using the IQR method
      iqr <- IQR(data[[var]], na.rm = TRUE)
      q1 <- quantile(data[[var]], 0.25, na.rm = TRUE)
      q3 <- quantile(data[[var]], 0.75, na.rm = TRUE)
      lower_bound <- q1 - 1.5 * iqr
      upper_bound <- q3 + 1.5 * iqr
      
      outliers <- which(data[[var]] < lower_bound | data[[var]] > upper_bound)
      outlier_count <- length(outliers)
      
      if (outlier_count > 0) {
        if (input$outlier_method == "Remove Outliers") {
          # Remove outliers
          data <- data[-outliers, ]
        } else if (input$outlier_method == "Replace with Median") {
          # Replace outliers with median
          median_val <- median(data[[var]], na.rm = TRUE)
          data[[var]][outliers] <- median_val
        } else if (input$outlier_method == "Replace with Mean") {
          # Replace outliers with mean
          mean_val <- mean(data[[var]], na.rm = TRUE)
          data[[var]][outliers] <- mean_val
        }
        
        # Update the reactive storage
        uploaded_data(data)
        
        # Show popup message with the number of outliers detected
        showModal(
          modalDialog(
            title = "Outliers Handled",
            paste("The variable", var, "had", outlier_count, "outliers detected."),
            if (input$outlier_method == "Remove Outliers") {
              paste(outlier_count, "rows were removed.")
            } else {
              "The outliers have been replaced."
            },
            easyClose = TRUE,
            footer = modalButton("Close")
          )
        )
      } else {
        showNotification("No outliers detected in the selected variable.", type = "warning")
      }
    } else {
      showNotification("Selected variable is not numeric.", type = "error")
    }
  })
  
  
  # Handling Data Transformation Logic 
  
  observeEvent(input$apply_transformation, {
    req(uploaded_data())
    req(input$transform_var)
    req(input$transformation_method)
    
    data <- uploaded_data()
    selected_vars <- input$transform_var
    
    for (var in selected_vars) {
      if (input$transformation_method == "Min-Max Scaling") {
        data[[var]] <- (data[[var]] - min(data[[var]], na.rm = TRUE)) / 
          (max(data[[var]], na.rm = TRUE) - min(data[[var]], na.rm = TRUE))
      } else if (input$transformation_method == "Z-Score Normalization") {
        data[[var]] <- scale(data[[var]], center = TRUE, scale = TRUE)
      } else if (input$transformation_method == "Log Transformation") {
        data[[var]] <- log(data[[var]] + 1) # Adding 1 to handle zero values
      }
    }
    
    uploaded_data(data) # Update the dataset
    showNotification("Transformation applied successfully!", type = "message")
  })
  
  
  # Handling Data Encoding Logic
  
  # Observer for applying encoding
  observeEvent(input$apply_encoding, {
    req(uploaded_data())
    req(input$encoding_var)
    
    data <- uploaded_data()
    selected_vars <- input$encoding_var
    new_columns <- character(0)  # Initialize new_columns
    
    if (input$encoding_method == "Label Encoding") {
      for (var in selected_vars) {
        data[[var]] <- as.numeric(as.factor(data[[var]]))
      }
    } else if (input$encoding_method == "One-Hot Encoding") {
      one_hot <- model.matrix(~ . - 1, data[selected_vars, drop = FALSE])
      new_columns <- colnames(one_hot)  # Capture names of new columns
      data <- cbind(data[, !(names(data) %in% selected_vars)], one_hot)
    }
    
    # Track derived columns
    derived_columns(union(derived_columns(), new_columns))
    
    uploaded_data(data)  # Update the reactive dataset
    showNotification("Encoding applied successfully!", type = "message")
  })
  
  # Mise à jour des choix pour les variables
  observe({
    req(uploaded_data())
    data <- uploaded_data()
    updateSelectInput(session, "x_variable", choices = names(data))
    updateSelectInput(session, "y_variable", choices = names(data))
  })
  
  # Histogramme
  output$histogram <- renderPlotly({
    req(uploaded_data(), input$x_variable)
    data <- uploaded_data()
    plot_ly(data, x = ~get(input$x_variable), type = "histogram", autobinx = FALSE, 
            xbins = list(size = input$binwidth_input))
  })
  
  # Boxplot
  output$boxplot <- renderPlotly({
    req(uploaded_data(), input$x_variable)
    data <- uploaded_data()
    plot_ly(data, y = ~get(input$x_variable), type = "box")
  })
  
  # Analyse univariée
  output$univariate_analysis <- renderPrint({
    req(uploaded_data(), input$x_variable)
    data <- uploaded_data()
    summary(data[[input$x_variable]])
  })
  
  # Pie Chart
  output$pie_chart <- renderPlot({
    req(uploaded_data(), input$x_variable)
    data <- uploaded_data()
    variable <- data[[input$x_variable]]
    if (is.factor(variable) || is.character(variable)) {
      pie(table(variable), main = "Pie Chart", col = rainbow(length(unique(variable))))
    } else {
      showNotification("Pie Chart is only available for categorical variables.", type = "error")
    }
  })
  
  # Analyse bidimensionnelle : Correlation plot
  output$bivariate_analysis <- renderPlotly({
    req(uploaded_data(), input$x_variable, input$y_variable)
    data <- uploaded_data()
    plot_ly(data, x = ~get(input$x_variable), y = ~get(input$y_variable), type = "scatter", mode = "markers")
  })
  
  # Quantitative vs qualitative : Boxplots
  output$quant_vs_qual_table <- renderTable({
    req(input$var_x, input$var_y, processed_data())
    processed_data() %>%
      group_by(!!sym(input$var_x)) %>%
      summarise(
        Moyenne = mean(!!sym(input$var_y), na.rm = TRUE),
        Ecart_type = sd(!!sym(input$var_y), na.rm = TRUE)
      )
  })
  
  output$boxplot_parallel <- renderPlot({
    req(uploaded_data(), input$x_variable, input$y_variable)
    
    data <- uploaded_data()
    x <- data[[input$x_variable]]  # Variable catégorielle
    y <- data[[input$y_variable]]  # Variable numérique
    
    validate(
      need(is.numeric(y), "Y Variable must be numeric."),
      need(is.factor(x) || is.character(x), "X Variable must be categorical.")
    )
    
    ggplot(data, aes(x = as.factor(x), y = y)) +
      geom_boxplot(fill = "orange", alpha = 0.7) +
      theme_minimal() +
      labs(title = "Parallel Boxplots", x = input$x_variable, y = input$y_variable)
  })
  
  
  # Correlation Matrix
  output$correlation_matrix_plot <- renderPlot({
    req(uploaded_data())
    numeric_data <- uploaded_data()[, sapply(uploaded_data(), is.numeric)]
    corr <- cor(numeric_data, use = "complete.obs")
    corrplot::corrplot(corr, method = "color", type = "upper")
  })
  
  # Observer pour synchroniser les sliders
  observeEvent(input$train_percentage, {
    updateSliderInput(
      session,
      "test_percentage",
      value = 100 - input$train_percentage
    )
  })
  
  observeEvent(input$test_percentage, {
    updateSliderInput(
      session,
      "train_percentage",
      value = 100 - input$test_percentage
    )
  })
  data_split <- reactive({
    req(uploaded_data())
    data <- uploaded_data()
    
    if (input$split_method == "Holdout") {
      train_ratio <- input$train_percentage / 100
      set.seed(123) # Pour la reproductibilité
      train_indices <- sample(seq_len(nrow(data)), size = floor(train_ratio * nrow(data)))
      list(
        train = data[train_indices, ],
        test = data[-train_indices, ]
      )
    } else if (input$split_method == "Cross-validation") {
      folds <- input$k_folds
      set.seed(123) # Pour la reproductibilité
      cv_indices <- sample(rep(1:folds, length.out = nrow(data)))
      split_list <- lapply(1:folds, function(k) {
        list(
          train = data[cv_indices != k, ],
          test = data[cv_indices == k, ]
        )
      })
      split_list
    } else {
      NULL
    }
  })
  
  # Visualisation des données découpées
  output$data_split_summary <- renderPrint({
    split <- data_split()
    if (input$split_method == "Holdout") {
      cat("Holdout Split:\n")
      cat("Training Set Size:", nrow(split$train), "\n")
      cat("Testing Set Size:", nrow(split$test), "\n")
    } else if (input$split_method == "Cross-validation") {
      cat("Cross-validation (k =", input$k_folds, "):\n")
      lapply(seq_along(split), function(k) {
        cat("Fold", k, ": Training Size =", nrow(split[[k]]$train), ", Testing Size =", nrow(split[[k]]$test), "\n")
      })
    }
  })
  observe({
    req(uploaded_data())
    data <- uploaded_data()
    updateSelectInput(session, "target_variable", choices = names(data))
  })
  
  # Observer pour définir les modèles disponibles en fonction de la variable cible
  observeEvent(input$target_variable, {
    req(uploaded_data(), input$target_variable)
    data <- uploaded_data()
    target_var <- data[[input$target_variable]]
    
    if (is.numeric(target_var)) {
      updateSelectInput(
        session,
        "model_choice",
        choices = c("Linear Regression", "Decision Tree"),
        selected = "Linear Regression"
      )
    } else if (is.factor(target_var) || is.character(target_var)) {
      updateSelectInput(
        session,
        "model_choice",
        choices = c("SVM", "Random Forest", "Logistic Regression"),
        selected = "Logistic Regression"
      )
    } else {
      showNotification("Unsupported data type for Target Variable.", type = "error")
    }
  })
  observe({
    req(uploaded_data())
    data <- uploaded_data()
    updateSelectInput(session, "target_variable", choices = names(data))
  })
  
  # Mise à jour des modèles en fonction de la variable cible
  observeEvent(input$target_variable, {
    req(uploaded_data(), input$target_variable)
    data <- uploaded_data()
    target_var <- data[[input$target_variable]]
    
    if (is.numeric(target_var)) {
      updateSelectInput(
        session,
        "model_choice",
        choices = c("Linear Regression", "Decision Tree"),
        selected = "Linear Regression"
      )
    } else if (is.factor(target_var) || is.character(target_var)) {
      updateSelectInput(
        session,
        "model_choice",
        choices = c("SVM", "Random Forest", "Logistic Regression"),
        selected = "Logistic Regression"
      )
    } else {
      showNotification("Unsupported data type for Target Variable.", type = "error")
    }
  })
 
  
  # Variable réactive pour suivre l'état d'entraînement du modèle
  model_trained <- reactiveVal(FALSE)
  trained_model <- reactiveVal(NULL)
  predictions <- reactiveVal(NULL)
  actuals <- reactiveVal(NULL)
  
  observeEvent(input$train_model, {
    req(uploaded_data(), input$target_variable, input$model_choice)
    
    # Charger les données et préparer les ensembles de train/test
    data <- uploaded_data()
    target_var <- input$target_variable
    split_method <- input$split_method
    
    # Vérifier et imputer les valeurs manquantes
    data <- na.omit(data)
    
    # Appliquer automatiquement Label Encoding pour les variables catégoriques
    predictors <- setdiff(names(data), target_var)  # Variables prédictives
    for (var in predictors) {
      if (is.character(data[[var]]) || is.factor(data[[var]])) {
        data[[var]] <- as.numeric(as.factor(data[[var]]))
      }
    }
    
    # Vérifier que la variable cible est bien un facteur pour les algorithmes de classification
    if (input$model_choice %in% c("SVM", "Random Forest", "Logistic Regression")) {
      if (!is.factor(data[[target_var]])) {
        data[[target_var]] <- as.factor(data[[target_var]])
      }
    }
    
    # Diviser les données (Holdout ou Cross-validation)
    model <- NULL  # Initialisation du modèle pour qu'il soit accessible après la boucle
    if (split_method == "Holdout") {
      train_index <- sample(1:nrow(data), size = floor(input$train_percentage / 100 * nrow(data)))
      train_data <- data[train_index, ]
      test_data <- data[-train_index, ]
      
      # Appliquer automatiquement Label Encoding pour les variables catégoriques sur les données de test aussi
      for (var in predictors) {
        if (is.character(train_data[[var]]) || is.factor(train_data[[var]])) {
          train_data[[var]] <- as.numeric(as.factor(train_data[[var]]))
          test_data[[var]] <- as.numeric(as.factor(test_data[[var]]))  # appliquer l'encodage sur les données de test
        }
      }
      
      # Entraîner le modèle sur les données d'entraînement
      if (input$model_choice == "SVM") {
        library(e1071)
        model <- svm(
          as.formula(paste(target_var, "~ .")),
          data = train_data,
          kernel = input$svm_kernel,
          cost = input$svm_C
        )
      } else if (input$model_choice == "Random Forest") {
        library(randomForest)
        model <- randomForest(
          as.formula(paste(target_var, "~ .")),
          data = train_data,
          ntree = input$rf_trees
        )
      } else if (input$model_choice == "Logistic Regression") {
        library(glmnet)
        
        # Préparer les données pour glmnet
        x <- model.matrix(as.formula(paste(target_var, "~ .")), data = train_data)[, -1]
        y <- as.numeric(as.factor(train_data[[target_var]])) - 1  # Reindexer les classes à partir de 0
        
        # Déterminer le type de régression logistique (binaire ou multinomiale)
        if (length(unique(train_data[[target_var]])) == 2) {
          family_type <- "binomial"
        } else {
          family_type <- "multinomial"
        }
        
        # Appliquer glmnet avec ou sans pénalité
        if (input$log_reg_penalty) {
          penalty_type <- ifelse(input$log_reg_penalty_type == "L1 (Lasso)", 1, 0)
          model <- glmnet(
            x,
            y,
            alpha = penalty_type,
            lambda = input$log_reg_penalty_value,
            family = family_type
          )
        } else {
          # Modèle sans pénalité (régression classique)
          model <- glm(
            as.formula(paste(target_var, "~ .")),
            data = train_data,
            family = family_type
          )
        }
      }
    } else if (split_method == "Cross-validation") {
      folds <- input$k_folds
      # Mélanger les données avant la cross-validation
      set.seed(123)  # Pour la reproductibilité
      data <- data[sample(nrow(data)), ]  # Mélanger les données
      
      # Diviser les données en plis (folds)
      fold_size <- floor(nrow(data) / folds)
      cv_results <- lapply(1:folds, function(k) {
        # Définir les indices de test pour chaque pli
        test_index <- ((k - 1) * fold_size + 1):(k * fold_size)
        test_data <- data[test_index, ]
        train_data <- data[-test_index, ]
        
        # Appliquer automatiquement Label Encoding pour les variables catégoriques sur le train et le test
        for (var in predictors) {
          if (is.character(train_data[[var]]) || is.factor(train_data[[var]])) {
            train_data[[var]] <- as.numeric(as.factor(train_data[[var]]))
            test_data[[var]] <- as.numeric(as.factor(test_data[[var]]))  # appliquer l'encodage sur les données de test
          }
        }
        
        # Entraîner le modèle pour ce pli
        model <- NULL  # Assurez-vous que le modèle est réinitialisé pour chaque pli
        if (input$model_choice == "SVM") {
          library(e1071)
          model <- svm(
            as.formula(paste(target_var, "~ .")),
            data = train_data,
            kernel = input$svm_kernel,
            cost = input$svm_C
          )
        } else if (input$model_choice == "Random Forest") {
          library(randomForest)
          model <- randomForest(
            as.formula(paste(target_var, "~ .")),
            data = train_data,
            ntree = input$rf_trees
          )
        } else if (input$model_choice == "Logistic Regression") {
          library(glmnet)
          
          # Préparer les données pour glmnet
          x <- model.matrix(as.formula(paste(target_var, "~ .")), data = train_data)[, -1]
          y <- as.numeric(as.factor(train_data[[target_var]])) - 1  # Reindexer les classes à partir de 0
          
          # Déterminer le type de régression logistique (binaire ou multinomiale)
          if (length(unique(train_data[[target_var]])) == 2) {
            family_type <- "binomial"
          } else {
            family_type <- "multinomial"
          }
          
          # Appliquer glmnet avec ou sans pénalité
          if (input$log_reg_penalty) {
            penalty_type <- ifelse(input$log_reg_penalty_type == "L1 (Lasso)", 1, 0)
            model <- glmnet(
              x,
              y,
              alpha = penalty_type,
              lambda = input$log_reg_penalty_value,
              family = family_type
            )
          } else {
            # Modèle sans pénalité (régression classique)
            model <- glm(
              as.formula(paste(target_var, "~ .")),
              data = train_data,
              family = family_type
            )
          }
        }
        
        # Retourner le modèle entraîné et les données de test
        list(model = model, test_data = test_data)
      })
      
      # Calculer la performance du modèle sur chaque pli (par exemple, accuracy)
      performance <- sapply(cv_results, function(x) {
        model <- x$model
        test_data <- x$test_data
        predictions <- predict(model, test_data)
        accuracy <- mean(predictions == test_data[[target_var]])
        accuracy
      })
      
      # Calculer la moyenne des performances sur tous les plis
      avg_accuracy <- mean(performance)
      showNotification(paste("Cross-validation Accuracy: ", round(avg_accuracy, 4)), type = "message")
      
    } else {
      showNotification("Invalid split method.", type = "error")
      return()
    }
    
    # Stocker le modèle entraîné
    trained_model(model)
    model_trained(TRUE) # Indiquer que le modèle est entraîné
    showNotification("Model trained successfully!", type = "message")
  })
  
  
  
  
  
  observeEvent(c(input$model_choice, input$svm_C, input$svm_kernel, 
                 input$rf_trees, input$log_reg_penalty, input$log_reg_penalty_value, 
                 input$dt_max_depth, input$split_method), {
                   model_trained(FALSE)
                 })
  
  
  
  output$save_model_ui <- renderUI({
    if (model_trained()) {
      downloadButton("save_model", "Save Model", class = "btn-primary")
    }
  })
  
  
  
  # Télécharger le modèle
  output$save_model <- downloadHandler(
    filename = function() {
      paste("trained_model_", Sys.Date(), ".rds", sep = "")
    },
    content = function(file) {
      model <- trained_model()
      if (is.null(model)) {
        showNotification("No trained model available for download.", type = "error")
        return()
      }
      saveRDS(model, file)
    }
  )
  
  observeEvent(trained_model(), {
    req(trained_model(), uploaded_data(), input$target_variable)
    
    # Récupérer les données et le modèle
    model <- trained_model()
    data <- uploaded_data()
    target_var <- input$target_variable
    split_method <- input$split_method
    
    # Diviser les données en train/test (si ce n'est pas déjà fait)
    if (split_method == "Holdout") {
      test_data <- data[-train_index, ]
      train_data <- data[train_index, ]
    } else {
      test_data <- cv_results[[1]]$test_data  # Exemple avec le 1er pli pour simplifier
    }
    
    # Prédire sur les données de test
    predictions <- predict(model, test_data)
    actual <- test_data[[target_var]]
    
    # Calculer les métriques de performance
    library(caret)  # Pour confusionMatrix
    library(pROC)   # Pour la courbe ROC et AUC
    
    conf_matrix <- confusionMatrix(as.factor(predictions), as.factor(actual))
    precision <- conf_matrix$byClass["Precision"]
    recall <- conf_matrix$byClass["Recall"]
    f1_score <- 2 * (precision * recall) / (precision + recall)
    
    # Courbe ROC et AUC
    roc_data <- roc(as.numeric(actual), as.numeric(predictions), quiet = TRUE)
    auc_score <- auc(roc_data)
    
    # Sauvegarder les résultats dans des objets réactifs
    metrics <- reactive({
      data.frame(
        Metric = c("Precision", "Recall", "F1-Score"),
        Value = c(round(precision, 2), round(recall, 2), round(f1_score, 2))
      )
    })
    
    roc_curve_data <- reactive({
      data.frame(
        False_Positive_Rate = 1 - roc_data$specificities,
        True_Positive_Rate = roc_data$sensitivities
      )
    })
    
    auc_value <- reactive({ round(auc_score, 2) })
    
    # Sorties pour l'UI
    output$model_metrics <- renderTable({
      req(metrics())
      metrics()
      print("hello")
    })
    
    output$conf_matrix_plot <- renderPlot({
      req(conf_matrix)
      ggplot2::autoplot(conf_matrix$table, type = "heatmap")
    })
    
    output$roc_curve <- renderPlot({
      req(roc_curve_data())
      ggplot(roc_curve_data(), aes(x = False_Positive_Rate, y = True_Positive_Rate)) +
        geom_line(color = "blue") +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
        labs(title = "ROC Curve", x = "False Positive Rate", y = "True Positive Rate")
    })
    
    output$auc_score <- renderText({
      req(auc_value())
      paste("AUC:", auc_value())
    })
  })
