library(shiny)
library(shinydashboard)
library(bslib)
library(readxl)  
library(DT)
library(plotly)

server <- function(input, output, session) {
  # Reactive data storage
  uploaded_data <- reactiveVal(NULL)
  
  column_types <- reactiveVal(NULL)
  
  # Watch for the file input event
  observeEvent(input$fileInput, {
    req(input$fileInput)  # Ensure the file is uploaded
    
    # Read the file based on its extension
    file_ext <- tools::file_ext(input$fileInput$name)
    data <- NULL
    
    if (file_ext == "csv") {
      data <- read.csv(input$fileInput$datapath)
    } else if (file_ext %in% c("xlsx", "xls")) {
      sheet_names <- excel_sheets(input$fileInput$datapath)
      if ("Data" %in% sheet_names) {
        data <- read_excel(input$fileInput$datapath, sheet = "Data")
      } else {
        showNotification("Sheet 'Data' not found in the file.", type = "error")
        return(NULL)
      }
    } else {
      showNotification("Unsupported file type", type = "error")
      return(NULL)
    }
    
    # Store the loaded data in a reactive value
    uploaded_data(data)
    
    # Dynamically update the target variable selection in the third box
    output$target_variable <- renderUI({
      req(uploaded_data())  # Ensure data is available
      selectInput("target_variable", "Select the target variable", 
                  choices = names(uploaded_data()),  # Column names as choices
                  selected = NULL)  # Default selection
    })
    
    # Update drop feature choices dynamically
    output$drop_feature_ui <- renderUI({
      req(uploaded_data())
      selectInput("drop_feature", "Select Feature to Drop:", 
                  choices = names(uploaded_data()), 
                  selected = NULL)
    })
    
    # Display the uploaded file name in the first box
    output$fileName <- renderText({
      paste("Uploaded file:", input$fileInput$name)  # Display file name
    })
    
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
    
    column_types(sapply(data, class))
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
  
  # Drop Feature Logic
  observeEvent(input$apply_drop, {
    req(uploaded_data())
    selected_feature <- input$drop_feature
    if (!is.null(selected_feature) && selected_feature != "") {
      updated_data <- uploaded_data()[, !(names(uploaded_data()) %in% selected_feature)]
      uploaded_data(updated_data)  # Update the reactive data
      
      # Update UI components to reflect changes
      output$drop_feature_ui <- renderUI({
        req(uploaded_data())
        selectInput("drop_feature", "Select Feature to Drop:", 
                    choices = names(uploaded_data()), 
                    selected = NULL)
      })
      
      output$target_variable <- renderUI({
        req(uploaded_data())
        selectInput("target_variable", "Select the target variable", 
                    choices = names(uploaded_data()), 
                    selected = NULL)
      })
    }
  })
  
  # Example: Display updated data table
  output$table <- renderDT({
    req(uploaded_data())
    datatable(uploaded_data(), options = list(pageLength = 5))
  })
  
  # Display the uploaded file name in the first box
  output$fileName <- renderText({
    paste("Uploaded file:", input$fileInput$name)  # Display file name
  })
  
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

output$missing_method_ui <- renderUI({
  req(input$missing_var, uploaded_data())
  var <- uploaded_data()[[input$missing_var]]
  if (sum(is.na(var)) > 0) {
    selectInput(
      inputId = "missing_method", 
      label = "Select Method to Handle Missing Values:", 
      choices = c("Suppression", "Replace with Mode", "Replace with Median", "Replace with Mean"), 
      selected = current_missing_var() # Preserve the current selection
    )
  } else {
    tags$p("No missing values in the selected variable.", style = "color: green;")
  }
})

# Dynamic selection for outliers 

output$outlier_var_ui <- renderUI({
  req(uploaded_data()) # Ensure data is loaded
  numeric_vars <- names(uploaded_data())[sapply(uploaded_data(), is.numeric)]
  
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




output$transform_var_ui <- renderUI({
  req(uploaded_data())
  req(column_types())
  
  col_types <- column_types()
  numeric_vars <- names(col_types[col_types %in% c("numeric", "integer")])
  
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
  
  # Identify categorical variables (character or factor types)
  data <- uploaded_data()
  categorical_vars <- names(data)[sapply(data, function(col) is.factor(col) || is.character(col))]
  
  selectizeInput(
    inputId = "encoding_var",
    label = "Select Categorical Variables for Encoding:",
    choices = categorical_vars,
    selected = NULL, # No variable selected by default
    multiple = TRUE
  )
})



# Apply Logic of Preprocessing 

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
  
  uploaded_data(data)
  showNotification("Transformation applied successfully!", type = "message")
})


# Handling Data Encoding Logic

observeEvent(input$apply_encoding, {
  req(uploaded_data())
  req(input$encoding_var)
  
  data <- uploaded_data()
  selected_vars <- input$encoding_var
  col_types <- column_types()
  
  if (input$encoding_method == "Label Encoding") {
    for (var in selected_vars) {
      data[[var]] <- as.numeric(as.factor(data[[var]]))
    }
  } else if (input$encoding_method == "One-Hot Encoding") {
    one_hot <- model.matrix(~ . - 1, data[selected_vars, drop = FALSE])
    data <- cbind(data[, !(names(data) %in% selected_vars)], one_hot)
  }
  
  # Update dataset and preserve original column types
  uploaded_data(data)
  column_types(col_types) # Keep column types unchanged
  showNotification("Encoding applied successfully!", type = "message")
})


# Create a reactive expression to generate the histogram
output$target_plot <- renderPlot({
  req(uploaded_data())
  req(input$target_variable)
  
  # Get the target variable
  target_var <- uploaded_data()[[input$target_variable]]
  
  # Check if the target variable is numeric
  if (is.numeric(target_var)) {
    # Create a histogram using ggplot2
    ggplot(data.frame(x = target_var), aes(x = x)) + 
      geom_histogram(color = "black", fill = "lightblue", binwidth = 1) + 
      labs(title = paste("Histogram of", input$target_variable), 
           x = input$target_variable, 
           y = "Frequency")
  } else {
    # If the target variable is not numeric, create a bar plot using ggplot2
    ggplot(data.frame(x = target_var), aes(x = x)) + 
      geom_bar(stat = "count", color = "black", fill = "lightblue") + 
      labs(title = paste("Bar Plot of", input$target_variable), 
           x = input$target_variable, 
           y = "Frequency")
  }
})

output$resampling_method_ui <- renderUI({
  req(uploaded_data())
  req(input$target_variable)
  
  target_var <- uploaded_data()[[input$target_variable]]
  if (is.factor(target_var)) {
    selectInput(
      inputId = "resampling_method",
      label = "Select Resampling Method:",
      choices = c("None", "Oversample", "Undersample"),
      selected = "None"
    )
  } else {
    tags$p("Resampling methods are applicable only to categorical target variables.", style = "color: red;")
  }
})

# Resampling handler
observeEvent(input$apply_resampling, {
  req(uploaded_data())
  req(input$resampling_method)
  
  target_var <- uploaded_data()[[input$target_variable]]
  
  if (is.factor(target_var)) {
    data <- uploaded_data()
    
    if (input$resampling_method == "Oversampling") {
      oversample_data <- caret::upSample(x = data[, !(names(data) %in% input$target_variable)], 
                                         y = data[[input$target_variable]], 
                                         yname = input$target_variable)
      uploaded_data(oversample_data)
      showNotification("Oversampling applied.", type = "message")
    } else if (input$resampling_method == "Undersampling") {
      undersample_data <- caret::downSample(x = data[, !(names(data) %in% input$target_variable)], 
                                            y = data[[input$target_variable]], 
                                            yname = input$target_variable)
      uploaded_data(undersample_data)
      showNotification("Undersampling applied.", type = "message")
    } else {
      showNotification("No resampling method selected.", type = "warning")
    }
  } else {
    showNotification("Resampling methods are applicable only to categorical target variables.", type = "error")
  }
})

output$table <- renderDT({
  req(uploaded_data())
  datatable(uploaded_data(), options = list(pageLength = 5))
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
  
  # Charger les données et préparer les sets de train/test
  data <- uploaded_data()
  target_var <- input$target_variable
  split_method <- input$split_method
  
  # Vérifier et imputer les valeurs manquantes
  data <- na.omit(data)
  
  # Diviser les données
  if (split_method == "Holdout") {
    train_index <- sample(1:nrow(data), size = floor(input$train_percentage / 100 * nrow(data)))
    train_data <- data[train_index, ]
    test_data <- data[-train_index, ]
  } else {
    showNotification("Cross-validation training is not implemented yet.", type = "warning")
    model_trained(FALSE)
    return()
  }
  
  # Entraîner le modèle en fonction du choix
  model <- NULL
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
    x <- model.matrix(as.formula(paste(target_var, "~ .")), data = train_data)[, -1]
    y <- as.numeric(train_data[[target_var]]) - 1
    
    if (input$log_reg_penalty) {
      penalty_type <- ifelse(input$log_reg_penalty_type == "L1 (Lasso)", 1, 0)
      model <- glmnet(
        x,
        y,
        alpha = penalty_type,
        lambda = input$log_reg_penalty_value,
        family = "binomial"
      )
    } else {
      model <- glm(
        as.formula(paste(target_var, "~ .")),
        data = train_data,
        family = "binomial"
      )
    }
  } else if (input$model_choice == "Linear Regression") {
    model <- lm(
      as.formula(paste(target_var, "~ .")),
      data = train_data
    )
  } else if (input$model_choice == "Decision Tree") {
    library(rpart)
    model <- rpart(
      as.formula(paste(target_var, "~ .")),
      data = train_data,
      control = rpart.control(maxdepth = input$dt_max_depth)
    )
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




output$model_metrics <- renderTable({
  req(trained_model(), uploaded_data(), input$target_variable)
  
  data <- uploaded_data()
  target_var <- input$target_variable
  
  # Préparer les données de test
  if (input$split_method == "Holdout") {
    test_index <- setdiff(1:nrow(data), sample(1:nrow(data), size = floor(input$train_percentage / 100 * nrow(data))))
    test_data <- data[test_index, ]
  } else {
    showNotification("Cross-validation metrics are not implemented yet.", type = "warning")
    return(NULL)
  }
  
  predictions <- NULL
  actual <- as.factor(test_data[[target_var]])  # Convertir en facteur
  
  # Générer les prédictions selon le modèle
  if (input$model_choice == "SVM" || input$model_choice == "Random Forest") {
    predictions <- as.factor(predict(trained_model(), test_data))
  } else if (input$model_choice == "Logistic Regression") {
    probabilities <- predict(trained_model(), newdata = test_data, type = "response")
    predictions <- as.factor(ifelse(probabilities > 0.5, 1, 0))
  } else if (input$model_choice == "Decision Tree") {
    predictions <- as.factor(predict(trained_model(), test_data, type = "class"))
  }
  
  # Harmoniser les niveaux
  levels(predictions) <- levels(actual)
  
  # Calcul des métriques
  library(caret)
  cm <- confusionMatrix(predictions, actual)
  metrics <- data.frame(
    Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
    Value = c(
      cm$overall["Accuracy"],
      cm$byClass["Pos Pred Value"],
      cm$byClass["Sensitivity"],
      cm$byClass["F1"]
    )
  )
  
  return(metrics)
})

output$conf_matrix_plot <- renderPlot({
  req(trained_model(), uploaded_data(), input$target_variable)
  
  data <- uploaded_data()
  target_var <- input$target_variable
  
  # Préparer les données de test
  if (input$split_method == "Holdout") {
    test_index <- setdiff(1:nrow(data), sample(1:nrow(data), size = floor(input$train_percentage / 100 * nrow(data))))
    test_data <- data[test_index, ]
  } else {
    return()
  }
  
  predictions <- NULL
  actual <- as.factor(test_data[[target_var]])
  
  # Générer les prédictions selon le modèle
  if (input$model_choice == "SVM" || input$model_choice == "Random Forest") {
    predictions <- as.factor(predict(trained_model(), test_data))
  } else if (input$model_choice == "Logistic Regression") {
    probabilities <- predict(trained_model(), newdata = test_data, type = "response")
    predictions <- as.factor(ifelse(probabilities > 0.5, 1, 0))
  } else if (input$model_choice == "Decision Tree") {
    predictions <- as.factor(predict(trained_model(), test_data, type = "class"))
  }
  
  # Harmoniser les niveaux
  levels(predictions) <- levels(actual)
  
  # Matrice de confusion
  library(caret)
  cm <- confusionMatrix(predictions, actual)
  
  # Visualiser la matrice de confusion
  fourfoldplot(cm$table, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1)
})

output$roc_curve <- renderPlot({
  req(trained_model(), uploaded_data(), input$target_variable)
  
  data <- uploaded_data()
  target_var <- input$target_variable
  
  # Préparer les données de test
  if (input$split_method == "Holdout") {
    test_index <- setdiff(1:nrow(data), sample(1:nrow(data), size = floor(input$train_percentage / 100 * nrow(data))))
    test_data <- data[test_index, ]
  } else {
    return()
  }
  
  actual <- as.factor(test_data[[target_var]])
  probabilities <- NULL
  
  # Générer les probabilités selon le modèle
  if (input$model_choice == "Logistic Regression" || input$model_choice == "SVM") {
    probabilities <- predict(trained_model(), newdata = test_data, type = "response")
  } else if (input$model_choice == "Random Forest") {
    probabilities <- predict(trained_model(), test_data, type = "prob")[, 2]
  } else if (input$model_choice == "Decision Tree") {
    probabilities <- predict(trained_model(), test_data, type = "prob")[, 2]
  }
  
  # Tracer la courbe ROC
  library(pROC)
  roc_obj <- roc(actual, probabilities)
  plot(roc_obj, col = "blue", lwd = 2, main = "ROC Curve")
})


  
}