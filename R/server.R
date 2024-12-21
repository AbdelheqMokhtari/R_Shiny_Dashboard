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
  # Update variable selection choices
  observe({
    req(display_data())
    data <- display_data()
    # Update univariate analysis selection
    updateSelectInput(session, "x_variable", choices = names(data))
    # Update bivariate analysis selections
    updateSelectInput(session, "x_variable_bi", choices = names(data))
    updateSelectInput(session, "y_variable", choices = names(data))
  })
  
  # Unidimensional Analysis
  # Histogram
  # Unidimensional Analysis - Enhanced Histogram or Bar Chart
  output$histogram <- renderPlotly({
    req(display_data(), input$x_variable)
    
    # Get the selected data
    data <- display_data()
    variable_data <- data[[input$x_variable]]
    
    # Check if the variable is numeric
    if (is.numeric(variable_data)) {
      # Calculate the range and determine bin width
      data_range <- range(variable_data, na.rm = TRUE)
      bin_width <- (data_range[2] - data_range[1]) / 10  # Divide range into 10 intervals
      
      # Plot histogram
      plot_ly(
        data, 
        x = ~get(input$x_variable), 
        type = "histogram", 
        autobinx = FALSE, 
        xbins = list(size = bin_width)  # Use calculated bin width
      ) %>%
        layout(
          title = paste("Histogram of", input$x_variable),
          xaxis = list(title = input$x_variable),
          yaxis = list(title = "Count"),
          bargap = 0.1
        )
    } else if (is.factor(variable_data) || is.character(variable_data)) {
      # For categorical variables, create a bar chart
      category_counts <- table(variable_data)  # Count occurrences of each category
      
      plot_ly(
        x = names(category_counts), 
        y = as.numeric(category_counts), 
        type = "bar"
      ) %>%
        layout(
          title = paste("Bar Chart of", input$x_variable),
          xaxis = list(title = input$x_variable),
          yaxis = list(title = "Count"),
          bargap = 0.3
        )
    } else {
      # Display a message if the variable is not numeric or categorical
      plot_ly() %>%
        layout(
          title = "Cannot generate plot",
          annotations = list(
            text = "Selected variable is neither numeric nor categorical.",
            x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 16)
          )
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
  )
  
  observeEvent(input$split_data, {
    req(training_data(), y())
    
    data <- training_data()
    target <- y()
    
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
      
    } 
    else {
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
  
  reactive_values <- reactiveValues(conf_matrix = NULL)
  
  observeEvent(input$show_results, {
    req(reactive_model, splits$test_data, splits$test_target)
    req(splits, input$model_choice)
    
    # Initialize variables
    predictions <- predict(reactive_model, newdata = splits$test_data)
    target <- factor(splits$test_target)
    
    # Extract unique values in the target variable and store them in levels
    levels <- unique(target)
    
    # Print the unique values (categories) of the target variable
    print("Unique values (categories) of the target variable:")
    print(levels)
    
    # Print dimensions of predictions and target to console
    print(paste("Dimensions of predictions:", length(predictions)))
    print(paste("Dimensions of target:", length(target)))
    
    # Check if predictions are NULL or empty
    if (is.null(predictions) || length(predictions) == 0) {
      stop("Predictions are NULL or empty!")
    }
    
    # Check if the model is SVM or Random Forest (classification models)
    if (inherits(reactive_model, "svm") || inherits(reactive_model, "randomForest")) {
      
      # Convert categorical target to ordinal numeric values (assumption: target is ordinal)
      unique_categories <- unique(target)  # Extract unique categories
      category_mapping <- setNames(1:length(unique_categories), unique_categories)  # Map categories to ordinal values
      
      # Convert target to numeric ordinal values for consistency
      target_numeric <- category_mapping[as.character(target)]
      
      # If the predictions are continuous (e.g., probabilities), convert them to class labels
      if (is.numeric(predictions)) {
        predictions <- round(predictions)  # Round predictions to the nearest integer
        
        # Ensure predictions are within valid range
        predictions <- pmin(pmax(predictions, 1), length(unique_categories))  # Clip predictions to valid category range
        predictions <- factor(predictions, levels = 1:length(unique_categories))  # Convert to factor
        predictions <- unique_categories[as.integer(predictions)]  # Map to actual category names
      }
      
      # Ensure predictions are factors with the same levels as target
      predictions <- factor(predictions, levels = levels(target))
      
      # Handle missing predictions correctly
      if (any(is.na(predictions))) {
        predictions[is.na(predictions)] <- levels(target)[1]  # Use the first level of target as default
      }
      
      # Print after handling NAs
      print("Predictions after handling NAs:")
      print(head(predictions))  # Print first few predictions after handling NAs
      
      # Classification Metrics Calculation
      confusion <- table(Predicted = predictions, Actual = target)
      print("Confusion Matrix:")
      print(confusion)  # Print the confusion matrix
      
      if (sum(confusion) > 0) {
        # If confusion matrix is not empty, calculate metrics
        accuracy <- sum(diag(confusion)) / sum(confusion)
        precision <- diag(confusion) / ifelse(rowSums(confusion) == 0, 1, rowSums(confusion))
        recall <- diag(confusion) / ifelse(colSums(confusion) == 0, 1, colSums(confusion))
        f1 <- 2 * (precision * recall) / ifelse((precision + recall) == 0, 1, (precision + recall))
        
        # Collect the classification metrics
        metrics <- data.frame(
          Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
          Value = c(
            accuracy,
            mean(precision, na.rm = TRUE),
            mean(recall, na.rm = TRUE),
            mean(f1, na.rm = TRUE)
          )
        )
        
        # Render classification metrics
        output$model_metrics <- renderTable({
          metrics
        }, rownames = FALSE)
        
        # Plot ROC curve and display AUC
        output$roc_curve <- renderPlot({
          library(pROC)  # Ensure pROC package is available
          
          # Convert target to numeric for pROC compatibility
          numeric_target <- as.numeric(target) - 1  # Convert levels to 0 and 1
          
          # Check if predictions are probabilities or factors
          if (is.numeric(predictions)) {
            prob_predictions <- predictions
          } else {
            prob_predictions <- as.numeric(as.character(predictions)) - 1
          }
          
          # Compute ROC curve and AUC
          roc_obj <- roc(numeric_target, prob_predictions)
          auc_score <- auc(roc_obj)
          
          # Plot ROC curve
          plot(roc_obj, col = "blue", lwd = 2, main = "ROC Curve")
          legend("bottomright", legend = paste("AUC =", round(auc_score, 3)), bty = "n", cex = 1.2)
        })
        
        # Button to display confusion matrix plot
        output$conf_matrix_plot <- renderPlot({
          heatmap(as.matrix(confusion), Rowv = NA, Colv = NA,
                  col = colorRampPalette(c("white", "blue"))(100),
                  scale = "none", margins = c(5, 5), xlab = "Actual", ylab = "Predicted")
        })
      } else {
        # If confusion matrix is empty, print a message
        output$model_metrics <- renderTable({
          data.frame(Metric = "Error", Value = "Confusion Matrix is empty")
        })
      }
      
    } else if (inherits(reactive_model, "rpart") || inherits(reactive_model, "lm")) {
      # Convert target to numeric if it's a factor
      if (is.factor(target)) {
        target <- as.numeric(as.character(target))
      }
      
      # Convert predictions to numeric if they're factors (just in case)
      if (is.factor(predictions)) {
        predictions <- as.numeric(as.character(predictions))
      }
      
      # Ensure predictions and target are not NULL or empty
      if (is.null(predictions) || length(predictions) == 0 || is.null(target) || length(target) == 0) {
        stop("Predictions or target are NULL or empty!")
      }
      
      # Print first few predictions and target for debugging
      print("Sample of predictions:")
      print(head(predictions))
      print("Sample of target:")
      print(head(target))
      
      # Calculate regression metrics
      mse <- mean((predictions - target)^2, na.rm = TRUE)
      rmse <- sqrt(mse)
      r_squared <- 1 - (sum((predictions - target)^2, na.rm = TRUE) / 
                          sum((target - mean(target, na.rm = TRUE))^2, na.rm = TRUE))
      
      # Metrics container for regression
      metrics <- data.frame(
        Metric = c("MSE", "RMSE", "R-squared"),
        Value = c(mse, rmse, r_squared)
      )
      
      # Render regression metrics
      output$model_metrics <- renderTable({
        metrics
      }, rownames = FALSE)
      
      # Render regression metrics
      output$conf_matrix_plot <- renderPlot({
        plot(target, predictions, 
             xlab = "Actual Values", 
             ylab = "Predicted Values", 
             main = "Prediction vs Actual Plot",
             col = "blue", pch = 16)
        abline(0, 1, col = "red", lwd = 2) # Add line y = x
      })
      
      # Plot 2: Residuals vs. Predictions
      output$roc_curve <- renderPlot({
        residuals <- target - predictions
        plot(predictions, residuals, 
             xlab = "Predicted Values", 
             ylab = "Residuals", 
             main = "Residuals vs Predicted Plot",
             col = "darkgreen", pch = 16)
        abline(h = 0, col = "red", lwd = 2) # Add horizontal line at 0
      })
      
    }
    
    output$feature_importance_plot <- renderPlot({
      # Random Forest
      if (inherits(reactive_model, "randomForest")) {
        # Extract feature importance for Random Forest
        importance_vals <- randomForest::importance(reactive_model)
        barplot(importance_vals[, 1], 
                names.arg = rownames(importance_vals),
                main = "Random Forest Feature Importance",
                col = "lightblue", 
                las = 2, 
                beside = TRUE,
                cex.names = 0.8)
        
      } else if (inherits(reactive_model, "rpart")) {
        # Extract feature importance for Decision Tree using caret
        importance_vals <- caret::varImp(reactive_model)
        
        # Ensure that the importance is a vector (extract only the importance values)
        importance_vector <- importance_vals$importance[, 1]  # This extracts the first column (importance values)
        
        # Check if importance_vector is a valid vector
        if (is.vector(importance_vector)) {
          # Create a bar plot for feature importance
          barplot(importance_vector, 
                  names.arg = rownames(importance_vals$importance),
                  main = "Decision Tree Feature Importance",
                  col = "lightgreen", 
                  las = 2, 
                  beside = TRUE,
                  cex.names = 0.8)
        } else {
          print("Error: The importance values are not in a valid format.")
        }
        
      } else if (inherits(reactive_model, "svm")) {
        # Ensure that the SVM model is linear
        if (reactive_model$type == "C-classification" && reactive_model$kernel == "linear") {
          # Extract coefficients for feature importance in a linear SVM
          coef_vals <- abs(reactive_model$coefs)
          feature_names <- colnames(splits$test_data)
          barplot(coef_vals, 
                  names.arg = feature_names,
                  main = "SVM Feature Importance",
                  col = "lightcoral", 
                  las = 2, 
                  beside = TRUE,
                  cex.names = 0.8)
        } else {
          print("Non-linear SVM does not have a simple feature importance plot.")
        }
        
      } else if (inherits(reactive_model, "lm")) {
        # For Linear Regression, extract coefficients for feature importance
        coef_vals <- abs(coef(reactive_model)[-1])  # Exclude the intercept (first element)
        feature_names <- names(coef(reactive_model))[-1]  # Exclude the intercept from feature names
        barplot(coef_vals, 
                names.arg = feature_names,
                main = "Linear Regression Feature Importance",
                col = "lightyellow", 
                las = 2, 
                beside = TRUE,
                cex.names = 0.8)
      }
    })
  })
  
  
    
  }


  


  
  
