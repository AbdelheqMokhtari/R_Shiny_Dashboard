library(shiny)
library(shinydashboard)
library(bslib)
library(readxl)  
library(DT)
library(plotly)
library(ROSE)
library(caret)
library(smotefamily)

server <- function(input, output, session) {
  
  ## Loading Data
  
  # Reactive data to store the uploaded data 
  
  training_data <- reactiveVal(NULL)
  display_data <- reactiveVal(NULL)
  y <- reactiveVal(NULL)
  y_display <- reactiveVal(NULL)
  save_variable <- reactiveVal(NULL)
  
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
  
  ## Switch Categorical and Numerical data
  
  # Dynamically update dropdown options
  observe({
    current_data <- display_data()
    
    numerical_vars <- names(current_data)[sapply(current_data, is.numeric)]
    categorical_vars <- names(current_data)[sapply(current_data, function(x) is.factor(x) || is.character(x))]
    
    updateSelectInput(session, "num_to_cat", choices = numerical_vars, selected = NULL)
    updateSelectInput(session, "cat_to_num", choices = categorical_vars, selected = NULL)
  })
  
  # Convert Numerical to Categorical
  observeEvent(input$apply_num_to_cat, {
    req(input$num_to_cat)  # Ensure input is not empty
    
    current_training_data <- training_data()
    current_display_data <- display_data()
    
    for (var in input$num_to_cat) {
      if (!is.factor(current_training_data[[var]])) {
        # Save the original values in an attribute for potential reconversion
        attr(current_training_data[[var]], "original_values") <- current_training_data[[var]]
        attr(current_display_data[[var]], "original_values") <- current_display_data[[var]]
      }
      current_training_data[[var]] <- as.factor(current_training_data[[var]])
      current_display_data[[var]] <- as.factor(current_display_data[[var]])
    }
    
    # Update datasets
    training_data(current_training_data)
    display_data(current_display_data)
    
    # Show confirmation dialog
    showModal(modalDialog(
      title = "Conversion Completed",
      paste("The following numerical variables have been converted to categorical:",
            paste(input$num_to_cat, collapse = ", ")),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  })
  
  # Convert Categorical to Numerical
  observeEvent(input$apply_cat_to_num, {
    req(input$cat_to_num)  # Ensure input is not empty
    
    current_training_data <- training_data()
    current_display_data <- display_data()
    
    successfully_converted <- c()
    failed_conversions <- c()
    
    for (var in input$cat_to_num) {
      # Check if the variable has original numerical values stored as an attribute
      original_values <- attr(current_training_data[[var]], "original_values")
      if (!is.null(original_values)) {
        # Restore the original numerical values
        current_training_data[[var]] <- original_values
        current_display_data[[var]] <- original_values
        successfully_converted <- c(successfully_converted, var)
      } else {
        # Handle missing values and check if non-missing values are numeric
        non_missing_values <- as.character(current_training_data[[var]][!is.na(current_training_data[[var]])])
        if (all(grepl("^[0-9.-]+$", non_missing_values))) {
          # Convert to numeric, preserving NA
          current_training_data[[var]] <- as.numeric(as.character(current_training_data[[var]]))
          current_display_data[[var]] <- as.numeric(as.character(current_display_data[[var]]))
          successfully_converted <- c(successfully_converted, var)
        } else {
          # Add to the failed list if conversion isn't possible
          failed_conversions <- c(failed_conversions, var)
        }
      }
    }
    
    # Update datasets if any conversion happened
    if (length(successfully_converted) > 0) {
      training_data(current_training_data)
      display_data(current_display_data)
    }
    
    # Show success dialog for converted variables
    if (length(successfully_converted) > 0) {
      showModal(modalDialog(
        title = "Conversion Completed",
        paste("The following categorical variables have been converted to numerical:",
              paste(successfully_converted, collapse = ", ")),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
    
    # Show error dialog for failed conversions
    if (length(failed_conversions) > 0) {
      showModal(modalDialog(
        title = "Conversion Error",
        paste("The following variables contain non-numeric values (excluding missing values) and could not be converted to numerical:",
              paste(failed_conversions, collapse = ", ")),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
  })
  
  ### Pre Processing
  
  
  ## Handling Missing values
  
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
  
  observeEvent(input$apply_transformation, {
    req(training_data())
    req(input$transform_var)
    req(input$transformation_method)
    
    # Get the training data and selected variables
    data <- training_data()
    selected_vars <- input$transform_var
    transformed_vars <- list()
    
    for (var in selected_vars) {
      if (input$transformation_method == "Min-Max Scaling") {
        data[[var]] <- (data[[var]] - min(data[[var]], na.rm = TRUE)) / 
          (max(data[[var]], na.rm = TRUE) - min(data[[var]], na.rm = TRUE))
        transformed_vars[[var]] <- "Min-Max Scaling"
      } else if (input$transformation_method == "Z-Score Normalization") {
        data[[var]] <- scale(data[[var]], center = TRUE, scale = TRUE)
        transformed_vars[[var]] <- "Z-Score Normalization"
      } else if (input$transformation_method == "Log Transformation") {
        data[[var]] <- log(data[[var]] + 1) # Adding 1 to handle zero values
        transformed_vars[[var]] <- "Log Transformation"
      }
    }
    
    # Update the dataset with the transformations
    training_data(data) 
    
    # Show notification
    showNotification("Transformation applied successfully!", type = "message")
    
    # Prepare the transformation message
    if (length(transformed_vars) == 1) {
      var_list <- paste(names(transformed_vars), collapse = ", ")
      transformation_msg <- paste(input$transformation_method, "applied in the following variable:", var_list)
    } else {
      var_list <- paste(names(transformed_vars), collapse = ", ")
      transformation_msg <- paste(input$transformation_method, "applied in the following variables:", var_list)
    }
    
    # Show modal dialog with applied transformations
    showModal(
      modalDialog(
        title = "Transformation Applied",
        transformation_msg,
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
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
    req(input$encoding_method)
    
    # Get the training data and selected variables
    data <- training_data()
    selected_vars <- input$encoding_var
    encoded_vars <- list()
    
    for (var in selected_vars) {
      if (input$encoding_method == "Label Encoding") {
        data[[var]] <- as.integer(as.factor(data[[var]]))
        encoded_vars[[var]] <- "Label Encoding"
      } else if (input$encoding_method == "One-Hot Encoding") {
        one_hot <- model.matrix(~ . - 1, data.frame(data[[var]]))
        colnames(one_hot) <- paste(var, colnames(one_hot), sep = "_")
        data <- cbind(data, one_hot)
        data[[var]] <- NULL
        encoded_vars[[var]] <- "One-Hot Encoding"
      }
    }
    
    # Update the dataset with the encoding applied
    training_data(data)
    
    # Show notification
    showNotification("Encoding applied successfully!", type = "message")
    
    # Prepare the encoding message
    if (length(encoded_vars) == 1) {
      var_list <- paste(names(encoded_vars), collapse = ", ")
      encoding_msg <- paste(encoded_vars[[1]], "applied in the following variable:", var_list)
    } else {
      var_list <- paste(names(encoded_vars), collapse = ", ")
      encoding_msg <- paste(encoded_vars[[1]], "applied in the following variables:", var_list)
    }
    
    # Show modal dialog with encoded variables
    showModal(
      modalDialog(
        title = "Encoding Applied",
        encoding_msg,
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
  
  ## Submit button 
  
  observeEvent(input$submit_data, {
    # Retrieve the current state of training_data and display_data
    req(training_data(), display_data())  # Ensure both datasets are available
    training <- training_data()
    display <- display_data()
    
    # Initialize messages for dialog box
    alert_messages <- c()  # For alerts
    warning_message <- "⚠️ Once you submit, further preprocessing will be locked and cannot be changed."
    
    # Check for categorical variables in training_data
    if (any(sapply(training, function(col) is.factor(col) || is.character(col)))) {
      alert_messages <- c(
        alert_messages, 
        "❗ The training data contains categorical variables. Label encoding will be automatically applied to these variables before training."
      )
    }
    
    # Check if the features in training_data differ from those in display_data
    if (ncol(training) != ncol(display)) {
      alert_messages <- c(
        alert_messages,
        "❗ The interface cannot handle a target variable that has been one-hot encoded. Please ensure the target variable is not encoded in this way."
      )
    }
    
    # Check for missing values in training_data
    if (any(is.na(training))) {
      alert_messages <- c(
        alert_messages,
        "❗ The training data contains missing values. If you don't handle them, all rows with null values will be removed automatically before training."
      )
    }
    
    # Prepare dialog box content
    dialog_content <- tagList(
      tags$h4("Alerts:", style = "color: red;"),  # Header for alert messages
      lapply(alert_messages, function(msg) tags$p(msg)),
      tags$hr(),
      tags$h4("Warning:", style = "color: orange;"),  # Header for the warning message
      tags$p(warning_message)
    )
    
    # Show modal dialog with confirm and cancel options
    showModal(
      modalDialog(
        title = "Submission Confirmation",
        dialog_content,
        easyClose = FALSE,  # Prevent closing without user action
        footer = tagList(
          actionButton("confirm_submit", "Confirm", icon = icon("check-circle")),
          modalButton("Cancel", icon = icon("times-circle"))
        )
      )
    )
  })
  
  
  # Handle Confirm and Cancel actions
  observeEvent(input$confirm_submit, {
    # Disable all preprocessing components
    shinyjs::disable("missing_values_section")
    shinyjs::disable("outliers_section")
    shinyjs::disable("transformation_section")
    shinyjs::disable("encoding_section")
    shinyjs::disable("submit_button")
    
    # Proceed to finalize the submission
    complete_submission()
    
    # Close the modal dialog
    removeModal()
  })
  
  observeEvent(input$cancel, {
    # Close the modal dialog without taking further action
    removeModal()
  })
  
  
  # Observe Confirm button click
  observeEvent(input$confirm_submit, {
    # Proceed with submission
    complete_submission()
  })
  
  # Define a function to handle submission
  complete_submission <- function() {
    # Disable all components
    shinyjs::disable("missing_var")
    shinyjs::disable("missing_method")
    shinyjs::disable("apply_missing")
    shinyjs::disable("outlier_var")
    shinyjs::disable("outlier_method")
    shinyjs::disable("apply_outliers")
    shinyjs::disable("transform_var")
    shinyjs::disable("transformation_method")
    shinyjs::disable("apply_transformation")
    shinyjs::disable("encoding_var")
    shinyjs::disable("encoding_method")
    shinyjs::disable("apply_encoding")
    shinyjs::disable("submit_data")
    save_variable("confirmed")
  }
  
  ## Save Button 
  
  # Enable the Save button only when Submit is clicked
  observe({
    toggleState("save_data", !is.null(save_variable()))
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
  ## Show training data 
  
  # Render the training Dataset
  output$table_training <- renderDT({
    req(display_data())  # Ensure data is available
    datatable(training_data(), options = list(scrollX = TRUE))
  })
  
  ### EDA 
  
  # Mise à jour des choix pour les variables
  # Update variable selection choices
  # Update variable selection choices for univariate and bivariate analysis
  # Update variable selection choices for univariate and bivariate analysis
  observe({
    req(display_data())
    data <- display_data()
    
    # Get numeric and categorical variables
    numeric_vars <- names(data)[sapply(data, is.numeric)]  # Numeric variables for histograms
    categorical_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x) || 
                                             (is.numeric(x) && length(unique(x)) <= 10))]  # Categorical variables
    
    # Combine numeric and categorical variables
    all_vars <- unique(c(numeric_vars, categorical_vars))
    
    updateSelectInput(session, "x_variable", choices = all_vars)
    updateSelectInput(session, "x_variable_bi", choices = names(data))  # For bivariate analysis
    updateSelectInput(session, "y_variable", choices = names(data))  # For bivariate analysis
  })
  
  # Unidimensional Analysis - Histogram or Bar plot
  output$histogram <- renderPlotly({
    req(display_data(), input$x_variable)
    data <- display_data()
    
    # Get the selected variable
    variable_data <- data[[input$x_variable]]
    
    # Check if the variable is numeric or categorical
    if (is.numeric(variable_data)) {
      # If numeric, create a histogram
      bin_range <- range(variable_data, na.rm = TRUE)
      bin_width <- (bin_range[2] - bin_range[1]) / 10
      
      plot_ly(data, x = ~get(input$x_variable), type = "histogram", autobinx = FALSE,
              xbins = list(size = bin_width)) %>%
        layout(
          title = paste("Histogram of", input$x_variable),
          xaxis = list(title = input$x_variable),
          yaxis = list(title = "Count")
        )
    } else if (is.factor(variable_data) || is.character(variable_data) || 
               (is.numeric(variable_data) && length(unique(variable_data)) <= 10)) {
      # If categorical, create a bar plot
      category_counts <- as.data.frame(table(variable_data))
      colnames(category_counts) <- c("Category", "Count")  # Rename columns for clarity
      
      plot_ly(category_counts, x = ~Category, y = ~Count, type = "bar", 
              marker = list(line = list(width = 2, color = 'rgb(255, 255, 255)'))) %>%
        layout(
          title = paste("Bar Plot of", input$x_variable),
          xaxis = list(title = input$x_variable, tickangle = 45),
          yaxis = list(title = "Count"),
          barmode = "group"
        )
    }
  })
  
  
  
  
  
  # Boxplot
  output$boxplot <- renderPlotly({
    req(display_data(), input$x_variable)
    data <- display_data()
    plot_ly(data, y = ~get(input$x_variable), type = "box")
  })
  
  # Univariate analysis summary
  output$univariate_analysis <- renderPrint({
    req(display_data(), input$x_variable)
    data <- display_data()
    summary(data[[input$x_variable]])
  })
  
  # Pie Chart
  # Reactive check if the selected variable is categorical
  is_categorical <- reactive({
    req(display_data(), input$x_variable)
    data <- display_data()
    variable <- data[[input$x_variable]]
    
    # Treat as categorical if it's a factor, character, or numeric with few unique values
    is.factor(variable) || is.character(variable) || 
      (is.numeric(variable) && length(unique(variable)) <= 10)
  })
  
  # Send the result of the categorical check to the UI
  output$is_categorical <- reactive({
    is_categorical()
  })
  # Required for conditionalPanel
  outputOptions(output, "is_categorical", suspendWhenHidden = FALSE)
  
  # Render the Pie Chart only for categorical variables
  output$pie_chart <- renderPlot({
    req(display_data(), is_categorical())
    data <- display_data()
    variable <- data[[input$x_variable]]
    
    # Convert numeric categorical variables to factors for better pie chart display
    if (is.numeric(variable)) {
      variable <- as.factor(variable)
    }
    
    pie(table(variable), main = "Pie Chart", col = rainbow(length(unique(variable))))
  })
  
  
  
  # Bidimensional Analysis
  
  
  
  # Correlation plot
  output$bivariate_analysis <- renderPlotly({
    req(display_data(), input$x_variable_bi, input$y_variable)
    data <- display_data()
    plot_ly(data, x = ~get(input$x_variable_bi), y = ~get(input$y_variable), type = "scatter", mode = "markers")
  })
  # correlation coefficient 
  # Reactive check if both variables are numeric
  is_both_numeric <- reactive({
    req(display_data(), input$x_variable_bi, input$y_variable)
    data <- display_data()
    is.numeric(data[[input$x_variable_bi]]) && is.numeric(data[[input$y_variable]])
  })
  
  # Send the result of numeric check to the UI
  output$show_correlation <- reactive({
    is_both_numeric()
  })
  outputOptions(output, "show_correlation", suspendWhenHidden = FALSE)
  
  # Render correlation plot with regression line
  output$bivariate_analysis <- renderPlotly({
    req(display_data(), input$x_variable_bi, input$y_variable)
    data <- display_data()
    
    x <- data[[input$x_variable_bi]]
    y <- data[[input$y_variable]]
    
    # If both variables are numeric, add regression line
    if (is_both_numeric()) {
      ggplotly(
        ggplot(data, aes(x = x, y = y)) +
          geom_point(color = "blue", alpha = 0.6) +
          geom_smooth(method = "lm", color = "red", se = FALSE) +
          labs(
            title = "Correlation Plot with Regression Line",
            x = input$x_variable_bi,
            y = input$y_variable
          ) +
          theme_minimal()
      )
    } else {
      # Default scatter plot for non-numeric combinations
      plot_ly(data, x = ~x, y = ~y, type = "scatter", mode = "markers")
    }
  })
  
  # Calculate and display the correlation coefficient
  output$correlation_coefficient <- renderText({
    req(is_both_numeric())
    data <- display_data()
    x <- data[[input$x_variable_bi]]
    y <- data[[input$y_variable]]
    
    corr <- cor(x, y, use = "complete.obs")
    paste("Correlation Coefficient (r):", round(corr, 3))
  })
  
  
  # Correlation matrix
  output$correlation_matrix_plot <- renderPlot({
    req(display_data())
    numeric_data <- display_data()[, sapply(display_data(), is.numeric)]
    corr <- cor(numeric_data, use = "complete.obs")
    corrplot::corrplot(corr, method = "color", type = "upper")
  })
  
  #calculate the coefficient between quantitative and qualitative 
  # Reactive check if X is qualitative and Y is quantitative
  is_qualitative_quantitative <- reactive({
    req(display_data(), input$x_variable_bi, input$y_variable)
    data <- display_data()
    (is.factor(data[[input$x_variable_bi]]) || is.character(data[[input$x_variable_bi]])) &&
      is.numeric(data[[input$y_variable]])
  })
  
  # Send the result of the qualitative/quantitative check to the UI
  output$show_correlation_ratio <- reactive({
    is_qualitative_quantitative()
  })
  outputOptions(output, "show_correlation_ratio", suspendWhenHidden = FALSE)
  
  # Calculate and render the correlation ratio c_{Y|X}
  output$correlation_ratio <- renderText({
    req(is_qualitative_quantitative())
    data <- display_data()
    x <- data[[input$x_variable_bi]]
    y <- data[[input$y_variable]]
    
    # Compute the overall mean of Y
    y_bar <- mean(y, na.rm = TRUE)
    
    # Compute the total variance of Y
    s2_y <- mean((y - y_bar)^2, na.rm = TRUE)
    
    # Group data by levels of X
    grouped_data <- split(y, x)
    
    # Compute explained variance (s2_E) and residual variance (s2_R)
    s2_E <- sum(sapply(grouped_data, function(group) {
      n_l <- length(group)
      y_bar_l <- mean(group, na.rm = TRUE)
      n_l * (y_bar_l - y_bar)^2
    })) / length(y)
    
    s2_R <- sum(sapply(grouped_data, function(group) {
      n_l <- length(group)
      var_l <- var(group, na.rm = TRUE)
      n_l * var_l
    })) / length(y)
    
    # Correlation ratio
    c_y_given_x <- sqrt(s2_E / s2_y)
    
    paste("Correlation Ratio (c_Y|X):", round(c_y_given_x, 3))
  })
  
  # Boxplot (parallel)
  output$boxplot_parallel <- renderPlot({
    req(display_data(), input$x_variable_bi, input$y_variable)
    data <- display_data()
    x <- data[[input$x_variable_bi]]
    y <- data[[input$y_variable]]
    
    validate(
      need(is.numeric(y), "Y Variable must be numeric."),
      need(is.factor(x) || is.character(x), "X Variable must be categorical.")
    )
    
    ggplot(data, aes(x = as.factor(x), y = y)) +
      geom_boxplot(fill = "orange", alpha = 0.7) +
      theme_minimal() +
      labs(title = "Parallel Boxplots", x = input$x_variable_bi, y = input$y_variable)
  })
  
  # Bar Plot (Column Profiles)
  output$bar_plot_profiles <- renderPlot({
    req(display_data(), input$x_variable_bi, input$y_variable)
    data <- display_data()
    x <- data[[input$x_variable_bi]]
    y <- data[[input$y_variable]]
    
    validate(
      need(is.factor(x) || is.character(x), "X Variable must be categorical."),
      need(is.factor(y) || is.character(y), "Y Variable must be categorical.")
    )
    
    contingency_table <- table(x, y)
    col_profiles <- prop.table(contingency_table, margin = 2)
    col_profiles_df <- as.data.frame(as.table(col_profiles))
    colnames(col_profiles_df) <- c("X", "Y", "Proportion")
    
    ggplot(col_profiles_df, aes(x = X, y = Proportion, fill = Y)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(
        title = "Diagramme en barres des profils-colonnes",
        x = input$x_variable_bi,
        y = "Proportion",
        fill = input$y_variable
      ) +
      scale_y_continuous(labels = scales::percent)
  })
  
  ## contingency table and cramer coefficent 
  # Reactive check if both variables are qualitative (including label-encoded)
  is_qualitative_qualitative <- reactive({
    req(display_data(), input$x_variable_bi, input$y_variable)
    data <- display_data()
    
    is_qualitative <- function(var) {
      is.factor(var) || is.character(var) || (is.numeric(var) && length(unique(var)) <= 10)
    }
    
    is_qualitative(data[[input$x_variable_bi]]) && is_qualitative(data[[input$y_variable]])
  })
  
  # Contingency table
  output$contingency_table <- renderTable({
    req(is_qualitative_qualitative())
    data <- display_data()
    x <- data[[input$x_variable_bi]]
    y <- data[[input$y_variable]]
    
    table(x, y)
  }, rownames = TRUE)
  
  # Cramér's V calculation
  output$cramers_v <- renderText({
    req(is_qualitative_qualitative())
    data <- display_data()
    x <- data[[input$x_variable_bi]]
    y <- data[[input$y_variable]]
    
    # Create contingency table
    contingency_table <- table(x, y)
    
    # Compute Cramér's V
    chi2_stat <- chisq.test(contingency_table, correct = FALSE)$statistic  # Chi-squared statistic
    n <- sum(contingency_table)  # Total number of observations
    min_dim <- min(nrow(contingency_table), ncol(contingency_table)) - 1  # Min(rows, cols) - 1
    
    cramers_v <- sqrt(as.numeric(chi2_stat) / (n * min_dim))  # Cramér's V formula
    
    paste("Cramér's V:", round(cramers_v, 3))
  })
  
  ### ML MODELS
  
  ## Select Target Variable
  
  # Observe the uploaded data and update the choices in target_variable
  observe({
    req(display_data())  # Ensure data is loaded
    col_names <- colnames(display_data())  # Get column names
    updateSelectInput(session, "target_variable", choices = col_names)
  })
  
  # Observe the validate button and process the selected target variable
  observeEvent(input$validate_target, {
    req(input$target_variable)  # Ensure the target variable is selected
    req(training_data())  # Ensure training data is available
    
    data <- training_data()
    target_var <- input$target_variable
    
    data_display <- display_data()
    target_var_display <- input$target_variable
    
    # Handle missing values
    data <- na.omit(data)  # Remove rows with missing values
    data_display <- na.omit(data_display)
    
    # Apply label encoding for categorical data
    data <- data %>%
      mutate(across(where(is.character), ~ as.numeric(factor(.)))) %>%
      mutate(across(where(is.factor), ~ as.numeric(factor(.))))
    
    # Drop the target variable from the training data
    if (target_var %in% colnames(data)) {
      Y <- data[[target_var]]  # Extract the target variable
      y_display(data_display[[target_var_display]])
      data[[target_var]] <- NULL  # Remove the target variable
      training_data(data)# Update the reactive variable
      display_data(data_display)# Update the Reactive Variable
    }
    y(Y)  # Update the reactive value of y
    
    # Provide feedback
    showNotification(
      paste("Target variable", input$target_variable, "successfully selected and removed from training data..."),
      type = "message"
    )
    
    # Disable the select input to prevent further changes
    shinyjs::disable("target_variable")
    shinyjs::disable("validate_target")
    
    # Show the boxes if the target variable is categorical
    if (is.factor(Y) || is.character(Y)) {
      shinyjs::show("target_histogram_box")  # Show the histogram box
      shinyjs::show("resampling_box")  # Show the resampling box
    }
    
    # Debugging: Print row counts
    cat("Number of rows in training_data:\n", nrow(training_data()), "\n")
    cat("Number of rows in display_data:\n", nrow(display_data()), "\n")
    cat("Number of rows in y:\n", length(y()), "\n")
    cat("Number of rows in y_display:\n", length(y_display()), "\n")
  })
  
  
  
  ## Resampling the target variable 
  
  # Render the output for the target variable
  output$target_summary <- renderUI({
    req(y, y_display())  # Ensure the target variable (y) is set
    
    target_data <- y_display()  # Retrieve the target variable stored in y()
    
    if (is.factor(target_data) || is.character(target_data)) {
      # Render histogram for categorical target variable
      plotOutput("target_histogram", height = "350px")
    } else if (is.numeric(target_data)) {
      # Render summary statistics for numerical target variable
      stats <- list(
        "Mean" = mean(target_data, na.rm = TRUE),
        "Median" = median(target_data, na.rm = TRUE),
        "Min" = min(target_data, na.rm = TRUE),
        "Max" = max(target_data, na.rm = TRUE),
        "Range" = diff(range(target_data, na.rm = TRUE)),
        "Variance" = var(target_data, na.rm = TRUE),
        "Standard Deviation" = sd(target_data, na.rm = TRUE),
        "Number of Values" = sum(!is.na(target_data))
      )
      renderStatsTable(stats)
    } else {
      return(NULL)  # Do not render anything if target is neither categorical nor numeric
    }
  })
  
  # Helper function to render statistics as a table
  renderStatsTable <- function(stats) {
    tags$table(
      class = "table table-striped",
      tags$thead(
        tags$tr(
          tags$th("Statistic"),
          tags$th("Value")
        )
      ),
      tags$tbody(
        lapply(names(stats), function(stat_name) {
          tags$tr(
            tags$td(stat_name),
            tags$td(round(stats[[stat_name]], 2))
          )
        })
      )
    )
  }
  
  # Render histogram for categorical target variable
  output$target_histogram <- renderPlot({
    req(y, y_display())  # Ensure the target variable (y) is set
    
    target_data <- y_display()  # Retrieve the target variable stored in y()
    
    barplot(
      table(target_data),
      main = "Target Variable Distribution",
      col = "skyblue",
      xlab = "Categories",
      ylab = "Frequency"
    )
  }, height = 350)
  
  # Dynamic title for the target variable box
  output$target_box_title <- renderText({
    req(input$target_variable)  # Ensure a target variable is selected
    
    target_data <- y_display()  # Replace with y_display() or your actual target data
    
    if (is.factor(target_data) || is.character(target_data)) {
      "Target Variable Distribution"
    } else if (is.numeric(target_data)) {
      "Target Variable Summary"
    } else {
      "Target Variable Details"
    }
  })
  
  # Server logic to dynamically update the UI
  output$resampling_ui <- renderUI({
    req(y_display())
    target_variable <- y_display()
    if (is.numeric(target_variable)) { # Replace with your logic to check if the target variable is numerical
      tagList(
        p("Numerical values have no resampling technique available.")
      )
    } else {
      tagList(
        selectInput("resampling_technique", "Choose Resampling Technique", 
                    choices = c("undersampling", "oversampling"), 
                    selected = "oversampling"),
        actionButton("apply_resampling", "Apply Resampling")
      )
    }
  })
  
  # Reactive value to track if resampling is active
  resampling_active <- reactiveVal(FALSE)
  
  # Confirmation dialog for resampling
  observeEvent(input$apply_resampling, {
    # Show confirmation modal
    showModal(
      modalDialog(
        title = "Confirm Resampling",
        "Are you sure you want to apply the selected resampling technique?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_resampling", "Confirm")
        )
      )
    )
  })
  
  # Handle confirm action
  observeEvent(input$confirm_resampling, {
    removeModal()  # Close the modal
    
    # Immediately disable controls
    shinyjs::disable("resampling_technique")
    shinyjs::disable("apply_resampling")
    
    # Set resampling as active
    resampling_active(TRUE)
    
    # Perform resampling logic
    resampling_technique <- input$resampling_technique
    target_var <- as.factor(y())
    target_var_display <- y_display()
    training_data_current <- training_data()
    
    # Validation
    if (nrow(training_data_current) != length(target_var)) {
      showNotification("Mismatch between training data and target variable rows.", type = "error")
      shinyjs::enable("resampling_technique")
      shinyjs::enable("apply_resampling")
      resampling_active(FALSE)
      return()
    }
    
    target_mapping <- data.frame(
      encoded = levels(target_var),
      original = unique(target_var_display)
    )
    if (nrow(target_mapping) != length(levels(target_var))) {
      showNotification("Mismatch in mapping between encoded and original target values.", type = "error")
      shinyjs::enable("resampling_technique")
      shinyjs::enable("apply_resampling")
      resampling_active(FALSE)
      return()
    }
    
    # Perform the resampling
    if (resampling_technique == "undersampling") {
      resampled_data <- caret::downSample(x = training_data_current, y = target_var)
    } else if (resampling_technique == "oversampling") {
      resampled_data <- caret::upSample(x = training_data_current, y = target_var)
    } else {
      showNotification("Invalid resampling technique selected.", type = "error")
      shinyjs::enable("resampling_technique")
      shinyjs::enable("apply_resampling")
      resampling_active(FALSE)
      return()
    }
    
    if (!".dataClass" %in% colnames(resampled_data)) {
      resampled_data$.dataClass <- rep(levels(target_var), length.out = nrow(resampled_data))
    }
    
    resampled_y <- as.character(resampled_data$.dataClass)
    resampled_y_display <- target_mapping$original[match(resampled_y, target_mapping$encoded)]
    
    # Update reactive variables
    training_data(resampled_data[, !(names(resampled_data) %in% ".dataClass"), drop = FALSE])
    y(as.factor(resampled_data$.dataClass))
    y_display(resampled_y_display)
    
    # Show success notification
    showNotification(
      paste("Resampling using", resampling_technique, "successfully applied."),
      type = "message"
    )
  })
  
  # Re-enable controls manually if needed
  observeEvent(input$reset_resampling, {
    resampling_active(FALSE)  # Reset state
    shinyjs::enable("resampling_technique")
    shinyjs::enable("apply_resampling")
    showNotification("Resampling controls re-enabled.", type = "message")
  })
  
  
  ## Split Data
  
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
  )
  
  observeEvent(input$split_data, {
    if (is.null(y())) {
      showModal(modalDialog(
        title = "Missing Target Variable",
        "You should select a target variable before splitting your data.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    req(training_data(), y())
    
    data <- training_data()
    target <- y()
    
    # Ensure all features are numeric
    non_numeric_columns <- sapply(data, is.numeric) == FALSE
    
    if (any(non_numeric_columns)) {
      data[non_numeric_columns] <- lapply(data[non_numeric_columns], function(col) {
        if (is.factor(col) || is.character(col)) {
          as.numeric(factor(col))  # Convert categorical variables to numeric using factor encoding
        } else {
          stop("Unsupported non-numeric column detected.")  # Handle unexpected cases
        }
      })
    }
    
    if (input$split_method == "Holdout") {
      train_percentage <- input$train_percentage / 100
      test_percentage <- input$test_percentage / 100
      
      if (abs(train_percentage + test_percentage - 1) > 0.01) {
        showNotification("Training and Testing percentages must add up to 100%.", type = "error")
        return()
      }
      
      set.seed(123)
      train_indices <- sample(1:nrow(data), size = floor(train_percentage * nrow(data)))
      
      splits$train_data <- data[train_indices, , drop = FALSE]
      splits$train_target <- target[train_indices]
      splits$test_data <- data[-train_indices, , drop = FALSE]
      splits$test_target <- target[-train_indices]
      
      # Post-Split Analysis
      output$split_summary <- renderPrint({
        list(
          Train_Distribution = table(splits$train_target),
          Test_Distribution = table(splits$test_target)
        )
      })
      
      
      output$split_message <- renderText({
        paste(
          "Holdout split completed successfully.\n",
          "Training Data: ", nrow(splits$train_data), " rows\n",
          "Training Target: ", length(splits$train_target), " rows\n",
          "Testing Data: ", nrow(splits$test_data), " rows\n",
          "Testing Target: ", length(splits$test_target), " rows\n"
        )
      })
      
      showNotification("Data successfully prepared for Holdout split.", type = "message")
      
    } 
    else {
      print("split method don't exist")
    }
    
  })
  
  # Select the models depends on the target variable
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
                        choices = c("Random Forest", "SVM"), 
                        selected = "Random Forest")
    } else if (is_continuous) {
      # Continuous numeric
      updateSelectInput(session, "model_choice", 
                        choices = c("Linear Regression", "Decision Tree"), 
                        selected = "Linear Regression")
    } else {
      showNotification("Unsupported target variable type. Please check your data.", type = "error")
    }
  })
  
  # Train Model
  reactive_metrics <- reactiveValues(data = NULL)
  feature_importance <- reactiveValues(data = NULL)
  
  observeEvent(input$train_model, {
    req(splits, input$model_choice)
    
    # Check if splits are defined
    if (is.null(splits) || is.null(splits$train_data)) {
      showModal(
        modalDialog(
          title = "Error",
          "You must split your data before training the model.",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
      return()
    }
    
    output$model_message <- renderText("Training in progress... Please wait.")
    
    data <- NULL
    target <- NULL
    model <- NULL  # Initialize model variable
    
    # Handle Holdout or Cross-validation
    if (input$split_method == "Holdout") {
      req(splits$train_data, splits$train_target)  # Ensure Holdout split is ready
      data <- splits$train_data
      target <- splits$train_target
      
      # Train the model based on the selected model choice
      if (input$model_choice == "SVM") {
        
        # Ensure the target is treated as a factor for classification tasks
        if (!is.factor(target)) {
          target <- as.factor(target)
        }
        
        req(input$svm_C, input$svm_kernel)
        
        # Train SVM model for classification
        model <- e1071::svm(
          x = data,
          y = target,
          cost = input$svm_C,
          kernel = input$svm_kernel,
          type = ifelse(is.factor(target), "C-classification", "eps-regression"),
          probability = TRUE  # Classification vs. Regression
        )
        
        # Since SVM doesn't provide feature importance directly, we skip this for SVM
        feature_importance$data <- NULL  # No feature importance to track for SVM
        
        output$model_message <- renderText("SVM model trained successfully!")
        
      } else if (input$model_choice == "Random Forest") {
        req(input$rf_trees)
        
        # Ensure the target is treated as a factor for classification tasks
        if (!is.factor(target)) {
          target <- as.factor(target)
        }
        
        # Train Random Forest model
        model <- randomForest::randomForest(
          x = data,
          y = target,
          ntree = input$rf_trees,
          probability = TRUE
        )
        
        # Track feature importance for Random Forest
        feature_importance$data <- randomForest::importance(model)
        
        output$model_message <- renderText("Random Forest model trained successfully!")
        
      } else if (input$model_choice == "Linear Regression") {
        
        # Train Linear Regression model
        formula <- as.formula(paste("target ~ ."))
        model <- lm(formula, data = data.frame(data, target = target))
        
        # For Linear Regression, use coefficients as feature importance
        feature_importance$data <- coef(model)[-1]  # Exclude the intercept
        
        output$model_message <- renderText("Linear Regression model trained successfully!")
        
      } else if (input$model_choice == "Decision Tree") {
        req(input$dt_max_depth, input$dt_criterion)
        
        # Train Decision Tree model
        model <- rpart::rpart(
          formula = as.formula(paste("target ~ .")),
          data = data.frame(data, target = target),
          method = ifelse(is.factor(target), "class", "anova"),  # Classification vs. Regression
          control = rpart::rpart.control(maxdepth = input$dt_max_depth),
          parms = list(split = input$dt_criterion)
        )
        
        # Track feature importance for Decision Tree
        # imp <- caret::varImp(reactive_model, scale = FALSE)
        # feature_importance$data <- imp$importance[,1]
        feature_importance$data <- NULL 
        
        output$model_message <- renderText("Decision Tree model trained successfully!")
        
      } else {
        output$model_message <- renderText("Invalid model choice. Please select a valid model.")
        return()
      }
      
    } else {
      print("no model is chosen")
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
  
  ### Results
  
  # Helper Function to Calculate Regression Metrics
  calculate_regression_metrics <- function(model, test_data, test_target) {
    predictions <- predict(model, newdata = test_data)
    
    # Calculate RMSE, MSE, and R²
    rmse <- sqrt(mean((test_target - predictions)^2))
    mse <- mean((test_target - predictions)^2)
    r2 <- 1 - sum((test_target - predictions)^2) / sum((test_target - mean(test_target))^2)
    n <- length(test_target)
    p <- length(coefficients(model)) - 1
    adjusted_r2 <- 1 - (1 - r2) * (n - 1) / (n - p - 1)
    
    # Return metrics as a data frame
    data.frame(
      Metric = c("RMSE", "MSE", "R²", "Adjusted R²"),
      Value = c(rmse, mse, r2, adjusted_r2)
    )
  }
  
  # Helper Function to Calculate Classification Metrics
  calculate_classification_metrics <- function(model, test_data, test_target) {
    # Ensure test_target is a factor
    if (!is.factor(test_target)) {
      test_target <- as.factor(test_target)
    }
    
    # Get predictions as class labels
    predictions <- predict(model, newdata = test_data, type = "class")
    
    # Debugging: Print Actual vs Predicted values
    actual_vs_predicted <- data.frame(Actual = test_target, Predicted = predictions)
    print("Debugging: Actual vs Predicted values")
    print(head(actual_vs_predicted, 10))  # Show the first 10 rows
    
    # Calculate metrics from confusion matrix
    confusion_matrix <- table(Predicted = predictions, Actual = test_target)
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
    recall <- diag(confusion_matrix) / colSums(confusion_matrix)
    specificity <- (sum(confusion_matrix) - colSums(confusion_matrix) - rowSums(confusion_matrix) + diag(confusion_matrix)) / 
      (sum(confusion_matrix) - colSums(confusion_matrix))
    f1_score <- 2 * (precision * recall) / (precision + recall)
    
    # Handle potential NA values (e.g., no predictions for certain classes)
    precision[is.na(precision)] <- 0
    recall[is.na(recall)] <- 0
    f1_score[is.na(f1_score)] <- 0
    specificity[is.na(specificity)] <- 0
    
    # Return metrics as a data frame
    data.frame(
      Metric = c("Accuracy", "Precision", "Recall","Specificity",  "F1 Score"),
      Value = c(
        accuracy, 
        mean(precision, na.rm = TRUE), 
        mean(recall, na.rm = TRUE), 
        mean(specificity, na.rm = TRUE), 
        mean(f1_score, na.rm = TRUE)
      )
    )
  }
  
  # Reactive expression to calculate metrics
  calculated_metrics <- reactive({
    req(reactive_model, splits$test_data, splits$test_target)  # Ensure necessary data and model are available
    
    # Fetch test data and test target
    test_data <- splits$test_data
    test_target <- splits$test_target
    
    # Determine the model type and calculate metrics
    if (inherits(reactive_model, "lm") || (inherits(reactive_model, "rpart") && is.numeric(test_target))) {
      # Regression Model (Linear Regression or Decision Tree with numeric target)
      calculate_regression_metrics(reactive_model, test_data, test_target)
    } else if (inherits(reactive_model, "randomForest") || inherits(reactive_model, "svm")) {
      # Classification Model
      calculate_classification_metrics(reactive_model, test_data, test_target)
    } else {
      # Unsupported Model Type
      data.frame(Metric = "Error", Value = "Unsupported model type for metric calculation.")
    }
  })
  
  # Render metrics in the table output
  output$model_metrics <- renderTable({
    calculated_metrics()  # Call the reactive expression to fetch the metrics
  })
  
  ## Confusion Matrix & Plot (Predicted & Actual)
  
  # Dynamic title logic
  output$dynamic_title <- renderText({
    req(input$model_choice)
    
    if (input$model_choice == "") {
      return("")
    } else if (input$model_choice %in% c("SVM", "Random Forest")) {
      return("Confusion Matrix")
    } else if (input$model_choice %in% c("Linear Regression", "Decision Tree")) {
      return("Plot Predicted vs Actual")
    } else {
      return("Unknown Model Type")
    }
  })
  
  # Dynamic plot rendering based on model type
  output$conf_matrix_plot <- renderPlot({
    req(reactive_model, splits$test_data, splits$test_target, input$model_choice)  # Ensure data and model choice are available
    
    # Fetch test data and test target
    test_data <- splits$test_data
    test_target <- splits$test_target
    
    # Classification Models: Render Confusion Matrix
    if (input$model_choice %in% c("SVM", "Random Forest")) {
      # Ensure test_target is a factor
      if (!is.factor(test_target)) {
        test_target <- as.factor(test_target)
      }
      
      # Get predictions
      predictions <- predict(reactive_model, newdata = test_data, type = "class")
      
      # Create confusion matrix
      confusion_matrix <- table(Predicted = predictions, Actual = test_target)
      
      # Visualize confusion matrix
      ggplot(data = as.data.frame(confusion_matrix), aes(x = Actual, y = Predicted)) +
        geom_tile(aes(fill = Freq), color = "white") +
        geom_text(aes(label = Freq), size = 5, color = "black") +
        scale_fill_gradient(low = "white", high = "steelblue") +
        labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12)
        )
    } 
    # Regression Models: Plot Actual vs Predicted
    else if (input$model_choice %in% c("Linear Regression", "Decision Tree")) {
      # Get predictions
      predictions <- predict(reactive_model, newdata = test_data)
      
      # Create scatter plot
      ggplot(data = data.frame(Actual = test_target, Predicted = predictions), aes(x = Actual, y = Predicted)) +
        geom_point(color = "steelblue", alpha = 0.7) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
        labs(title = "Actual vs Predicted", x = "Actual", y = "Predicted") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12)
        ) +
        coord_fixed()  # Ensure equal scaling
    } else {
      # Default: Display nothing if the model type is unsupported
      NULL
    }
  }, height = 330)  # Max plot height
  
  ## Plot Residual & RUC Curve
  
  # Dynamic title logic
  output$Roc_Residual <- renderText({
    req(input$model_choice)
    
    if (input$model_choice == "") {
      return("")
    } else if (input$model_choice %in% c("SVM", "Random Forest")) {
      return("AUC-RUC Curve")
    } else if (input$model_choice %in% c("Linear Regression", "Decision Tree")) {
      return("Plot Residual vs Predicted")
    } else {
      return("Unknown Model Type")
    }
  })
  
  # Render plot for ROC Curve or Residuals
  output$roc_curve <- renderPlot({
    req(reactive_model, splits$test_data, splits$test_target, input$model_choice)  # Ensure necessary inputs
    
    # Fetch test data and test target
    test_data <- splits$test_data
    test_target <- splits$test_target
    
    # Classification Models
    if (input$model_choice %in% c("SVM", "Random Forest")) {
      # Ensure test_target is a factor
      if (!is.factor(test_target)) {
        test_target <- as.factor(test_target)
      }
      
      # Handle model-specific probability predictions
      if (inherits(reactive_model, "svm")) {
        # For SVM, use `probability = TRUE` when training the model
        prob_predictions <- attr(predict(reactive_model, newdata = test_data, probability = TRUE), "probabilities")
      } else if (inherits(reactive_model, "randomForest")) {
        # For Random Forest
        prob_predictions <- predict(reactive_model, newdata = test_data, type = "prob")
      } else {
        stop("Unsupported classification model type.")
      }
      
      if (ncol(prob_predictions) == 2) {
        # Binary Classification
        roc_obj <- pROC::roc(test_target, prob_predictions[, 2])  # Use probabilities for the positive class
        auc_score <- pROC::auc(roc_obj)
        
        # Plot ROC Curve with diagonal line and customized axes
        p <- pROC::ggroc(roc_obj) +
          ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +  # Baseline diagonal line
          ggplot2::labs(
            title = paste("ROC Curve (AUC =", round(auc_score, 2), ")"),
            x = "FPR (1 - Specificity)",
            y = "TPR (Sensitivity)"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(size = 14, face = "bold"),
            axis.text = ggplot2::element_text(size = 12),
            axis.title = ggplot2::element_text(size = 12)
          )
        print(p)
      } else {
        # Multiclass Classification
        roc_list <- lapply(seq_len(ncol(prob_predictions)), function(class_idx) {
          pROC::roc(test_target == colnames(prob_predictions)[class_idx], prob_predictions[, class_idx])
        })
        
        # Average AUC
        auc_scores <- sapply(roc_list, pROC::auc)
        mean_auc <- mean(auc_scores)
        
        # Plot first class ROC as example with diagonal line
        p <- pROC::ggroc(roc_list[[1]]) +
          # ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +  # Baseline diagonal line
          ggplot2::labs(
            title = paste("Multiclass ROC Curve (Mean AUC =", round(mean_auc, 2), ")"),
            x = "FPR (1 - Specificity)",
            y = "TPR (Sensitivity)"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(size = 14, face = "bold"),
            axis.text = ggplot2::element_text(size = 12),
            axis.title = ggplot2::element_text(size = 12)
          )
        print(p)
      }
    } 
    
    # Regression Models
    else if (input$model_choice %in% c("Linear Regression", "Decision Tree")) {
      # Get predictions
      predictions <- predict(reactive_model, newdata = test_data)
      
      # Calculate residuals
      residuals <- test_target - predictions
      
      # Create scatter plot for residuals vs predicted
      ggplot(data = data.frame(Predicted = predictions, Residuals = residuals), aes(x = Predicted, y = Residuals)) +
        geom_point(color = "steelblue", alpha = 0.7) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(title = "Residuals vs Predicted", x = "Predicted", y = "Residuals") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12)
        )
    } else {
      NULL  # Unsupported model type
    }
  }, height = 330)  # Max plot height
  
  ## Features Importance
  
  fluidRow(
    # Box for Feature Importance Plot
    box(
      title = "Feature Importance",
      status = "success",
      solidHeader = TRUE,
      width = 12,
      plotOutput("importance_plot")
    )
  )
  
  output$importance_plot <- renderPlot({
    # Check if the model is trained
    req(reactive_model)
    if (input$model_choice == "SVM") {
      # Display a message for SVM importance not applicable
      plot.new()
      title(main = "SVM Feature Importance")
      text(0.5, 0.5, "SVM doesn't have a way to calculate\nfeature importance", cex = 1.2)
    }
    
    if (input$model_choice == "Decision Tree") {
      # Display a message for Decision Tree importance not implemented yet
      plot.new()
      title(main = "Decision Tree Feature Importance")
      text(0.5, 0.5, "Feature importance for Decision Tree\nisn't implemented yet", cex = 1.2)
    }
    
    # Check model choice
    if (!is.null(feature_importance$data)) {
      if (input$model_choice == "Random Forest") {
        # Extract feature importance from Random Forest model
        rf_imp <- randomForest::importance(reactive_model)
        feature_importance$data <- rf_imp[, 1]  # Get the importance values (first column)
        
        # Random Forest feature importance
        barplot(feature_importance$data, 
                main = "Random Forest Feature Importance", 
                col = "lightblue", 
                las = 2,            # Rotate labels to vertical
                cex.names = 0.8,    # Adjust size of labels
                names.arg = rownames(rf_imp),  # Set feature names from row names of importance
                horiz = TRUE)       # Make the bars horizontal for better readability
      } else if (input$model_choice == "Linear Regression") {
        # Linear Regression coefficients as feature importance
        barplot(feature_importance$data, 
                main = "Linear Regression Feature Importance", 
                col = "lightgreen", 
                las = 2, 
                cex.names = 0.8, 
                names.arg = names(feature_importance$data), 
                horiz = TRUE)
      }
    }
  })
  
}