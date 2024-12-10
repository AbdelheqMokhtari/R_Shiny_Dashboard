library(shiny)
library(shinydashboard)
library(bslib)
library(readxl)  
library(DT)
library(plotly)
# Code côté serveur
server <- function(input, output, session) {
  # Reactive data storage
  uploaded_data <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    req(input$file)
    
    file_ext <- tools::file_ext(input$file$name)
    
    if (file_ext == "csv") {
      data <- read.csv(input$file$datapath)
    } else if (file_ext %in% c("xlsx", "xls")) {
      sheet_names <- excel_sheets(input$file$datapath)
      if ("Data" %in% sheet_names) {
        data <- read_excel(input$file$datapath, sheet = "Data")
      } else {
        showNotification("Sheet 'Data' not found in the file.", type = "error")
        return(NULL)
      }
    } else {
      showNotification("Unsupported file type", type = "error")
      return(NULL)
    }
    
    uploaded_data(data)
  })
  
  # Aperçu des données
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
  
}