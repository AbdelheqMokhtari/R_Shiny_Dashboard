library(shiny)
library(shinydashboard)
library(bslib)
library(readxl)  
library(DT)
library(plotly)
library(ROSE)
library(e1071)
server <- function(input, output, session) {
  
  ## Loading Data
  
  # Reactive data to store the uploaded data 
  
  training_data <- reactiveVal(NULL)
  display_data <- reactiveVal(NULL)
  y <- reactiveVal(NULL)
  
  # Main observer for file input
  observeEvent(input$fileInput, {
    req(input$fileInput)
    
    data <- load_file_data(input$fileInput)  # Load the file data
    if (is.null(data)) return(NULL)  # Stop if data couldn't be loaded
    
    training_data(data)
    display_data(data)
    
    # Show success notification when data is loaded successfully
    showNotification("Data uploaded successfully!", type = "message")
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
  
  ## Dataset initial details
  
  # Observer to print file details
  observeEvent(display_data(), {
    req(display_data())  # Ensure the data is available
    
    data <- display_data()
    
    file_name <- tools::file_path_sans_ext(input$fileInput$name)
    file_ext <- tools::file_ext(input$fileInput$name)
    
    num_instances <- nrow(data)
    num_features <- ncol(data)
    
    # Identify categorical and numerical features
    num_categorical <- sum(sapply(display_data(), function(x) is.factor(x) || is.character(x)))
    num_numerical <- num_features - num_categorical
    
    # Print the details to the UI
    output$fileDetails <- renderText({
      paste(
        " File Name:", file_name , "\n",
        "File Format:", file_ext, "\n",
        "Number of Instances:", num_instances, "\n",
        "Number of Features:", num_features, "\n",
        "Categorical Features:", num_categorical, " /",
        "Numerical Features:", num_numerical
      )
    })
  })
  
  ## Data exploratation
  
  # Render the dataset in the "Show Data" tab
  output$table <- renderDT({
    req(display_data())  # Ensure data is available
    datatable(display_data(), options = list(scrollX = TRUE))
  })
  
  generate_summary <- function(data) {
    summary_df <- data.frame(
      Column = names(data),
      Class = sapply(data, function(x) if (is.numeric(x)) "Numerical" else "Categorical"),
      Missing = sapply(data, function(x) sum(is.na(x))),
      Min = sapply(data, function(x) if (is.numeric(x)) min(x, na.rm = TRUE) else "-"),
      Median = sapply(data, function(x) if (is.numeric(x)) median(x, na.rm = TRUE) else "-"),
      Max = sapply(data, function(x) if (is.numeric(x)) max(x, na.rm = TRUE) else "-"),
      Mean = sapply(data, function(x) if (is.numeric(x)) sprintf("%.3f", mean(x, na.rm = TRUE)) else "-"),
      SD = sapply(data, function(x) if (is.numeric(x)) sprintf("%.3f", sd(x, na.rm = TRUE)) else "-"),
      Variance = sapply(data, function(x) if (is.numeric(x)) sprintf("%.3f", var(x, na.rm = TRUE)) else "-"),
      Unique_Values = sapply(data, function(x) if (!is.numeric(x)) length(unique(x)) else "-")
    )
    
    # Ensure appropriate formatting for all columns
    summary_df[] <- lapply(summary_df, function(x) ifelse(is.na(x), "-", as.character(x)))
    return(summary_df)
  }
  
  # Render the dataset summary in the "Data Summary" tab
  
  output$dataSummary <- renderDT({
    req(display_data())  # Ensure data is available
    data <- display_data()
    
    # Generate the summary for numerical and categorical features
    summary_df <- generate_summary(data)
    
    # Render the summary as a datatable
    datatable(
      summary_df,
      options = list(scrollX = TRUE, pageLength = 10),  # Add scrolling and pagination
      rownames = FALSE
    )
  })
  
  ## Drop Feature
  
  # Dynamic UI for selecting features to drop
  output$drop_feature_ui <- renderUI({
    req(display_data())  # Ensure data is loaded
    selectInput(
      inputId = "drop_feature",
      label = "Select Feature(s) to Drop",
      choices = colnames(display_data()),  # Get column names from the displayed data
      multiple = TRUE  # Allow selecting multiple features
    )
  })
  
  # Observe the Apply Drop button
  observeEvent(input$apply_drop, {
    req(input$drop_feature)  # Ensure at least one feature is selected
    req(training_data())     # Ensure data is available
    
    # Get the selected features to drop
    selected_features <- input$drop_feature
    
    # Update training_data by removing the selected features
    updated_training_data <- training_data()
    updated_training_data <- updated_training_data[, !(colnames(updated_training_data) %in% selected_features)]
    training_data(updated_training_data)
    
    # Update display_data by removing the selected features
    updated_display_data <- display_data()
    updated_display_data <- updated_display_data[, !(colnames(updated_display_data) %in% selected_features)]
    display_data(updated_display_data)
    
    # Provide feedback to the user
    showNotification(
      paste("Dropped feature(s):", paste(selected_features, collapse = ", ")),
      type = "message"
    )
  })
  
  
  ## Switch Categorical and Numerical data (Drag & Drop)
  output$drag_drop_ui <- renderUI({
    req(display_data())
    data <- display_data()
    
    numeric_vars <- names(data)[sapply(data, is.numeric)]
    categorical_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
    
    tagList(
      fluidRow(
        column(
          width = 6,
          h4("Numerical Variables"),
          jqui_sortable(
            tags$ul(
              lapply(numeric_vars, function(x) tags$li(class = "list-group-item", x)),
              class = "list-group",
              id = "numeric_vars"
            ),
            options = list(connectWith = "#categorical_vars") # Allow dragging between lists
          )
        ),
        column(
          width = 6,
          h4("Categorical Variables"),
          jqui_sortable(
            tags$ul(
              lapply(categorical_vars, function(x) tags$li(class = "list-group-item", x)),
              class = "list-group",
              id = "categorical_vars"
            ),
            options = list(connectWith = "#numeric_vars") # Allow dragging between lists
          )
        )
      )
    )
  })
  
  # Apply the changes when "Apply Switch" is clicked
  observeEvent(input$apply_switch, {
    req(input$numeric_vars, input$categorical_vars)
    
    # Extract the updated variables from the sortable lists
    updated_numeric_vars <- input$numeric_vars
    updated_categorical_vars <- input$categorical_vars
    
    # Get the current display_data
    data <- display_data()
    
    # Update the data types based on the new classifications
    for (col in updated_numeric_vars) {
      if (!is.numeric(data[[col]])) {
        data[[col]] <- as.numeric(data[[col]])
      }
    }
    
    for (col in updated_categorical_vars) {
      if (!is.factor(data[[col]]) && !is.character(data[[col]])) {
        data[[col]] <- as.factor(data[[col]])
      }
    }
    
    # Save the updated data in training_data and display_data
    training_data(data)
    display_data(data)
    
    # Debugging: Print the updated variables to the console
    print("Updated Numerical Variables:")
    print(updated_numeric_vars)
    
    print("Updated Categorical Variables:")
    print(updated_categorical_vars)
  })

  ## Handling Missing values
  
  # Render the missing percentage for the selected variable.
  # Calculates the proportion of missing values in the selected variable 
  # from the displayed dataset (display_data) and displays it as a percentage.
  output$missing_percent <- renderText({
    req(input$missing_var, display_data())  # Ensure variable selection and data availability
    var <- display_data()[[input$missing_var]]  # Extract the selected variable's column
    percent <- sum(is.na(var)) / length(var) * 100  # Compute the percentage of missing values
    paste("Missing Percent:", round(percent, 2), "%")  # Format and return the result as text
  })
  
  current_missing_var <- reactiveVal(NULL)  # Holds the currently selected variable for missing value handling
  
  # UI for selecting a variable to handle missing values.
  output$missing_var_ui <- renderUI({
    req(display_data())  # Ensure displayed data is available
    variables <- names(display_data())  # Get the column names of the displayed data
    
    selectInput(
      inputId = "missing_var", 
      label = "Select Variable to Handle Missing Values:", 
      choices = variables, 
      selected = NULL
    )
  })
  
  # Missing value handling logic
  output$missing_method_ui <- renderUI({
    selectInput(
      inputId = "missing_method",
      label = "Select Method to Handle Missing Values:",
      choices = c("Suppression", "Replace with Mode", "Replace with Median", "Replace with Mean"),
      selected = "Suppression"
    )
  })
  
  observeEvent(input$apply_missing, {
    tryCatch({
      req(input$missing_var, input$missing_method, display_data(), training_data())
      
      displayed <- display_data()
      train <- training_data()
      var <- input$missing_var
      missing_count <- sum(is.na(displayed[[var]]))
      
      if (missing_count > 0) {
        if (input$missing_method == "Suppression") {
          rows_to_keep <- !is.na(displayed[[var]])
          displayed <- displayed[rows_to_keep, ]
          train <- train[rows_to_keep, ]
          
        } else if (input$missing_method == "Replace with Mode") {
          mode_val <- as.numeric(names(sort(table(displayed[[var]]), decreasing = TRUE)[1]))
          displayed[[var]][is.na(displayed[[var]])] <- mode_val
          train[[var]][is.na(train[[var]])] <- mode_val
          
        } else if (input$missing_method == "Replace with Median") {
          median_val <- median(displayed[[var]], na.rm = TRUE)
          displayed[[var]][is.na(displayed[[var]])] <- median_val
          train[[var]][is.na(train[[var]])] <- median_val
          
        } else if (input$missing_method == "Replace with Mean") {
          mean_val <- mean(displayed[[var]], na.rm = TRUE)
          displayed[[var]][is.na(displayed[[var]])] <- mean_val
          train[[var]][is.na(train[[var]])] <- mean_val
        }
        
        display_data(displayed)
        training_data(train)
        
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
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
    
  # Dynamic selection for outliers
  output$outlier_var_ui <- renderUI({
    req(display_data())
    numeric_vars <- names(display_data())[sapply(display_data(), is.numeric)]
      
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
    
  observeEvent(input$apply_outliers, {
    req(input$outlier_var, input$outlier_method, display_data(), training_data())
      
    # Retrieve both datasets
    displayed <- display_data()
    training <- training_data()
    var <- input$outlier_var
      
    if (is.numeric(displayed[[var]])) {
      # Detect outliers using the IQR method
      iqr <- IQR(displayed[[var]], na.rm = TRUE)
      q1 <- quantile(displayed[[var]], 0.25, na.rm = TRUE)
      q3 <- quantile(displayed[[var]], 0.75, na.rm = TRUE)
      lower_bound <- q1 - 1.5 * iqr
      upper_bound <- q3 + 1.5 * iqr
        
      # Identify outliers
      outliers <- which(displayed[[var]] < lower_bound | displayed[[var]] > upper_bound)
      outlier_count <- length(outliers)
        
      if (outlier_count > 0) {
        if (input$outlier_method == "Remove Outliers") {
          # Remove outliers from both datasets
          displayed <- displayed[-outliers, ]
          training <- training[-outliers, ]
        } else if (input$outlier_method == "Replace with Median") {
          # Replace outliers with the median in both datasets
          median_val <- median(displayed[[var]], na.rm = TRUE)
          displayed[[var]][outliers] <- median_val
          training[[var]][outliers] <- median_val
        } else if (input$outlier_method == "Replace with Mean") {
          # Replace outliers with the mean in both datasets
          mean_val <- mean(displayed[[var]], na.rm = TRUE)
          displayed[[var]][outliers] <- mean_val
          training[[var]][outliers] <- mean_val
        }
          
        # Update the reactive datasets
        display_data(displayed)  # Correct way to update a reactive value
        training_data(training)  # Correct way to update a reactive value
          
        # Show a modal message summarizing the changes
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
  
  ## Data Transformation
  
  # Dynamically selecting Data Transformation
  output$transform_var_ui <- renderUI({
    req(display_data())
    
    data <- display_data()
    # Dynamically identify numerical columns excluding derived columns
    numeric_vars <- names(data)[sapply(data, is.numeric)]
    
    selectizeInput(
      inputId = "transform_var",
      label = "Select Numerical Variables for Transformation:",
      choices = numeric_vars,
      selected = NULL, # No variable selected by default
      multiple = TRUE
    )
  })
  
  # Applying transformation logic 
  observeEvent(input$apply_transformation, {
    req(training_data())
    req(input$transform_var)
    req(input$transformation_method)
    
    data <- training_data()
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
    
    training_data(data) # Update the dataset
    showNotification("Transformation applied successfully!", type = "message")
    showNotification("PS: The results won't appear in Show data section!", type = "warning")
  })
  
  ## Data Encoding 
  
  # Data Encoding Variable Selection
  output$encoding_var_ui <- renderUI({
    req(display_data())
    
    data <- display_data()
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
  
  # Observer for applying encoding
  observeEvent(input$apply_encoding, {
    req(training_data())
    req(input$encoding_var)
    req(input$encoding_method) # Dropdown for encoding method
    
    data <- training_data()
    selected_vars <- input$encoding_var
    
    # Apply encoding for each selected variable
    for (var in selected_vars) {
      if (input$encoding_method == "Label Encoding") {
        # Label Encoding: Replace the variable with integer codes
        data[[var]] <- as.integer(as.factor(data[[var]]))
      } else if (input$encoding_method == "One-Hot Encoding") {
        # One-Hot Encoding: Use model.matrix and add new columns
        one_hot <- model.matrix(~ . - 1, data.frame(data[[var]]))
        colnames(one_hot) <- paste(var, colnames(one_hot), sep = "_")
        data <- cbind(data, one_hot)
        # Drop the original column after encoding
        data[[var]] <- NULL
      }
    }
    
    # Update the dataset
    training_data(data)
    showNotification("Encoding applied successfully!", type = "message")
  })
  
  
  ## Saving and submitting data 
  
  # Show submitted data table on Submit button click
  observeEvent(input$submit_data, {
    output$submitted_table <- renderDataTable({
      req(training_data())  # Ensure data exists before rendering
      training_data()
    })
  })
  
  # Enable the Save button only when Submit is clicked
  observe({
    toggleState("save_data", !is.null(training_data()))
  })
  
  # Save data as CSV on Save button click
  output$save_data <- downloadHandler(
    filename = function() {
      paste("training_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(training_data())  # Ensure data exists
      write.csv(training_data(), file, row.names = FALSE)
    }
  )
  
  ## EDA 
  
  # Mise à jour des choix pour les variables
  observe({
    req(display_data())
    data <- display_data()
    updateSelectInput(session, "x_variable", choices = names(data))
    updateSelectInput(session, "y_variable", choices = names(data))
  })
  
  # Histogramme
  output$histogram <- renderPlotly({
    req(display_data(), input$x_variable)
    data <- display_data()
    plot_ly(data, x = ~get(input$x_variable), type = "histogram", autobinx = FALSE, 
            xbins = list(size = input$binwidth_input))
  })
  
  # Boxplot
  output$boxplot <- renderPlotly({
    req(display_data(), input$x_variable)
    data <- display_data()
    plot_ly(data, y = ~get(input$x_variable), type = "box")
  })
  
  # Analyse univariée
  output$univariate_analysis <- renderPrint({
    req(display_data(), input$x_variable)
    data <- display_data()
    summary(data[[input$x_variable]])
  })
  
  # Pie Chart
  output$pie_chart <- renderPlot({
    req(display_data(), input$x_variable)
    data <- display_data()
    variable <- data[[input$x_variable]]
    if (is.factor(variable) || is.character(variable)) {
      pie(table(variable), main = "Pie Chart", col = rainbow(length(unique(variable))))
    } else {
      showNotification("Pie Chart is only available for categorical variables.", type = "error")
    }
  })
  
  # Analyse bidimensionnelle : Correlation plot
  output$bivariate_analysis <- renderPlotly({
    req(display_data(), input$x_variable, input$y_variable)
    data <- display_data()
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
    req(display_data(), input$x_variable, input$y_variable)
    
    data <- display_data()
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
    req(display_data())
    numeric_data <- display_data()[, sapply(display_data(), is.numeric)]
    corr <- cor(numeric_data, use = "complete.obs")
    corrplot::corrplot(corr, method = "color", type = "upper")
  })
  
  ### ML MODELS
  
  ## Target variable 
  
  # Observe the uploaded data and update the choices in target_variable
  observe({
    req(display_data())  # Ensure the data is loaded
    col_names <- colnames(display_data())  # Get column names from display_data
    updateSelectInput(session, "target_variable", choices = col_names)
  })
  
  # Observe the validate button and process the selected target variable
  observeEvent(input$validate_target, {
    req(input$target_variable)  # Ensure the target variable is selected
    req(training_data())  # Ensure training data is available
    
    data <- training_data()
    target_var <- input$target_variable
    
    # Drop the target variable from the training data
    if (target_var %in% colnames(data)) {
      Y <- data[[target_var]]  # Extract the target variable
      data[[target_var]] <- NULL  # Remove the target variable
      training_data(data)  # Update the reactive variable
    }
    print(Y)
    y(Y)
    
    # Provide feedback
    showNotification("Target variable successfully selected and removed from training data.", type = "message")
    
    # Disable the select input to prevent further changes
    shinyjs::disable("target_variable")
    shinyjs::disable("validate_target")
  })
  
  ## Resampling the target variable 
  
  # Render histogram for the target variable if it is categorical
  output$target_histogram <- renderPlot({
    req(input$target_variable)  # Ensure the target variable is selected
    req(y())  # Ensure the target variable is saved in Y
    
    target_data <- y()
    
    # Check if the target variable is categorical
    if (is.factor(target_data) || is.character(target_data)) {
      barplot(table(target_data), main = "Target Variable Distribution", col = "skyblue", 
              xlab = "Categories", ylab = "Frequency")
    } else {
      return(NULL)  # Do not render if the target variable is not categorical
    }
  })
  
  observeEvent(input$apply_resampling, {
    req(y()) 
    req(training_data())
    
    data <- training_data()
    target_data <- y()
    data$Target <- target_data
    
    if (input$resampling_technique == "undersampling") {
      balanced_data <- ovun.sample(Target ~ ., data = data, method = "under")$data
    } else if (input$resampling_technique == "oversampling") {
      balanced_data <- ovun.sample(Target ~ ., data = data, method = "over")$data
    }
    
    balanced_target <- balanced_data$Target
    balanced_data$Target <- NULL
    training_data(balanced_data)
    y(balanced_target)
    
    showNotification(paste("Resampling applied using", input$resampling_technique, "."), type = "message")
  })
  
  
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
  
  splits <- reactiveValues(
    train_data = NULL,
    train_target = NULL,
    test_data = NULL,
    test_target = NULL,
    folds = NULL
  )
  
  observeEvent(input$split_data, {
    req(training_data(), y())  # Ensure data and target variable are available
    
    # Fetch the data and target variable
    data <- training_data()
    target <- y()
    
    if (input$split_method == "Holdout") {
      train_percentage <- input$train_percentage / 100
      test_percentage <- input$test_percentage / 100
      
      if (abs(train_percentage + test_percentage - 1) > 0.01) {
        showNotification("Training and Testing percentages must add up to 100%.", type = "error")
        return()
      }
      
      # Generate indices for splitting
      set.seed(123)  # Ensure reproducibility
      train_indices <- sample(1:nrow(data), size = floor(train_percentage * nrow(data)))
      
      # Split data
      splits$train_data <- data[train_indices, , drop = FALSE]
      splits$train_target <- target[train_indices]
      splits$test_data <- data[-train_indices, , drop = FALSE]
      splits$test_target <- target[-train_indices]
      
      output$split_message <- renderText({
        paste(
          "Holdout split completed successfully.\n",
          "Training Data: ", nrow(splits$train_data), " rows\n",
          "Testing Data: ", nrow(splits$test_data), " rows\n"
        )
      })
      
      # Notify the user
      showNotification("Data successfully prepared for Holdout split.", type = "message")
      
    } else if (input$split_method == "Cross-validation") {
      req(input$k_folds)
      k <- input$k_folds
      
      if (k < 2) {
        showNotification("Number of folds must be at least 2.", type = "error")
        return()
      }
      
      # Create folds
      set.seed(123)  # Ensure reproducibility
      splits$folds <- caret::createFolds(target, k = k, list = TRUE, returnTrain = TRUE)
      
      output$split_message <- renderText({
        paste(
          "Cross-validation setup completed.\n",
          "Number of folds: ", k, "\n"
        )
      })
      
      # Notify the user
      showNotification(paste("Data successfully prepared for", k, "fold cross-validation."), type = "message")
    }
  })
  
  
  
  
  
  observeEvent(input$validate_target, {
    req(y())  # Ensure the target variable is available
    
    target <- y()
    if (is.factor(target) || is.character(target)) {
      # Categorical target variable
      updateSelectInput(session, "model_choice", 
                        choices = c("SVM", "Logistic Regression", "Decision Tree", "Random Forest"),
                        selected = "SVM")
    } else if (is.numeric(target)) {
      # Continuous target variable
      updateSelectInput(session, "model_choice", 
                        choices = c("Linear Regression", "Decision Tree", "Random Forest"),
                        selected = "Linear Regression")
    }
  })
  
  
  
  observe({
    req(y())  # Ensure the target variable is available
    
    # Check if the target variable is ordinal, categorical, or numeric continuous
    unique_values <- length(unique(y()))
    is_ordinal <- is.numeric(y()) && unique_values <= 10  # Treat as ordinal if numeric with few unique values
    is_categorical <- is.factor(y()) || is.character(y()) || is.ordered(y())
    is_continuous <- is.numeric(y()) && unique_values > 10  # Treat as continuous if numeric with many unique values
    
    # Update model selection options
    if (is_categorical || is_ordinal) {
      # Categorical or ordinal numeric
      updateSelectInput(session, "model_choice", 
                        choices = c("SVM", "Random Forest"), 
                        selected = "SVM")
    } else if (is_continuous) {
      # Continuous numeric
      updateSelectInput(session, "model_choice", 
                        choices = c("Linear Regression", "Decision Tree"), 
                        selected = "Linear Regression")
    } else {
      showNotification("Unsupported target variable type. Please check your data.", type = "error")
    }
  })
  reactive_metrics <- reactiveValues(data = NULL)
  
  observeEvent(input$train_model, {
    req(splits, input$model_choice)
    
    data <- NULL
    target <- NULL
    model <- NULL  # Initialize model variable
    
    # Handle Holdout or Cross-validation
    if (input$split_method == "Holdout") {
      req(splits$train_data, splits$train_target)  # Ensure Holdout split is ready
      data <- splits$train_data
      target <- splits$train_target
      
    } else if (input$split_method == "Cross-validation") {
      req(splits$folds)  # Ensure Cross-validation split is ready
      
      # Use the first fold for training as an example
      fold_indices <- splits$folds[[1]]
      data <- training_data()[fold_indices, , drop = FALSE]
      target <- y()[fold_indices]
    }
    
    # Train the model based on the selected model choice
    if (input$model_choice == "SVM") {
      req(input$svm_C, input$svm_kernel)
      
      # Train SVM model
      model <- e1071::svm(
        x = data,
        y = target,
        cost = input$svm_C,
        kernel = input$svm_kernel
      )
      
      output$model_message <- renderText("SVM model trained successfully!")
      
    } else if (input$model_choice == "Random Forest") {
      req(input$rf_trees)
      
      # Train Random Forest model
      model <- randomForest::randomForest(
        x = data,
        y = target,
        ntree = input$rf_trees
      )
      
      output$model_message <- renderText("Random Forest model trained successfully!")
      
    } else if (input$model_choice == "Linear Regression") {
      req(input$lin_reg_include_intercept)
      
      # Train Linear Regression model
      formula <- as.formula(paste("target ~ ."))
      model <- lm(formula, data = data.frame(data, target = target))
      
      output$model_message <- renderText("Linear Regression model trained successfully!")
      
    } else if (input$model_choice == "Decision Tree") {
      req(input$dt_max_depth, input$dt_criterion)
      
      # Train Decision Tree model
      model <- rpart::rpart(
        formula = as.formula(paste("target ~ .")),
        data = data.frame(data, target = target),
        method = ifelse(is.numeric(target), "anova", "class"),
        control = rpart::rpart.control(maxdepth = input$dt_max_depth),
        parms = list(split = input$dt_criterion)
      )
      
      output$model_message <- renderText("Decision Tree model trained successfully!")
      
    } else {
      output$model_message <- renderText("Invalid model choice. Please select a valid model.")
      return()
    }
    
    # Save trained model to a reactive value
    reactive_model <<- model
    
    # Enable save button
    output$save_model_ui <- renderUI({
      downloadButton("save_model", "Save Model")
    })
  })
  
  
  # Logic to save the trained model
  output$save_model <- downloadHandler(
    filename = function() {
      paste0(input$model_choice, "_model.rds")
    },
    content = function(file) {
      saveRDS(reactive_model, file)
    }
  ) 
  
  reactive_values <- reactiveValues(conf_matrix = NULL)
  
  
  observeEvent(input$show_results, {
    req(reactive_model, splits$test_data, splits$test_target)
    
    # Initialize variables
    predictions <- predict(reactive_model, newdata = splits$test_data)
    target <- splits$test_target
    
    # Metrics container
    metrics <- list()
    
    # Linear Regression and Decision Tree (Regression Metrics)
    if (inherits(reactive_model, "lm") || inherits(reactive_model, "rpart")) {
      mse <- mean((predictions - target)^2)
      rmse <- sqrt(mse)
      r_squared <- 1 - (sum((predictions - target)^2) / sum((target - mean(target))^2))
      
      metrics <- data.frame(
        Metric = c("MSE", "RMSE", "R-squared"),
        Value = c(mse, rmse, r_squared)
      )
    }
    
    # Random Forest and SVM (Classification Metrics)
    else if (inherits(reactive_model, "randomForest") || inherits(reactive_model, "svm")) {
      confusion <- table(Predicted = predictions, Actual = target)
      accuracy <- sum(diag(confusion)) / sum(confusion)
      precision <- diag(confusion) / rowSums(confusion)
      recall <- diag(confusion) / colSums(confusion)
      f1 <- 2 * (precision * recall) / (precision + recall)
      
      # Handle NA in F1 due to zero division
      f1[is.na(f1)] <- 0
      
      metrics <- data.frame(
        Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
        Value = c(
          accuracy,
          mean(precision, na.rm = TRUE),
          mean(recall, na.rm = TRUE),
          mean(f1, na.rm = TRUE)
        )
      )
    }
    
    # Render the metrics in the UI table
    output$model_metrics <- renderTable({
      metrics
    }, rownames = FALSE)
  })
  
  
  
  
  
  
  # Button to display confusion matrix plot
  observeEvent(input$show_conf_matrix, {
    req(reactive_model, reactive_values$conf_matrix)
    
    if (inherits(reactive_model, "randomForest") || inherits(reactive_model, "svm")) {
      output$conf_matrix_plot <- renderPlot({
        heatmap(as.matrix(reactive_values$conf_matrix), Rowv = NA, Colv = NA, 
                col = colorRampPalette(c("white", "blue"))(100), 
                scale = "none", margins = c(5, 5), xlab = "Actual", ylab = "Predicted")
      })
    } else {
      output$conf_matrix_plot <- renderPlot({
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Confusion matrix is available for classification models only.", cex = 1.2)
      })
    }
  })

  
  
  
  
  
  
}