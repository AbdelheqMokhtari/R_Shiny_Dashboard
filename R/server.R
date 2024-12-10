library(shiny)
library(shinydashboard)
library(bslib)
library(readxl)  
library(DT)
library(plotly)
server <- function(input, output, session) {
  # Reactive to store dataset
  dataset <- reactiveVal(NULL)
  
  # Observer to load the dataset
  observeEvent(input$load_data, {
    req(input$file_input)  # Vérifie que l'utilisateur a choisi un fichier
    
    file_path <- input$file_input$datapath
    file_name <- input$file_input$name
    file_ext <- tools::file_ext(file_name)
    
    if (file_ext == "csv") {
      data <- read.csv(file_path)
    } else if (file_ext == "xlsx") {
      library(readxl)
      data <- read_excel(file_path)
    } else if (file_ext == "txt") {
      data <- read.delim(file_path)
    } else {
      showNotification("Unsupported file type!", type = "error")
      return(NULL)
    }
    
    dataset(data)  # Sauvegarde les données chargées
    
    # Met à jour les choix pour le selectInput
    updateSelectInput(session, "target_variable", choices = names(data))
  })
  
  # Output: Dataset Information
  output$dataset_info <- renderPrint({
    req(dataset())  # Assure que le dataset est chargé
    
    data <- dataset()
    num_features <- ncol(data)
    num_instances <- nrow(data)
    file_name <- input$file_input$name
    file_ext <- tools::file_ext(file_name)
    
    categories <- sapply(data, function(col) if (is.factor(col)) length(unique(col)) else NA)
    num_categories <- sum(!is.na(categories))
    
    list(
      "Title of the file" = file_name,
      "Type of the file" = file_ext,
      "Number of features" = num_features,
      "Number of instances" = num_instances,
      "Number of categories" = num_categories
    )
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
  trained_model <- reactiveVal(NULL)
  
  # Observer pour entraîner le modèle quand on clique sur le bouton "Train Model"
  observeEvent(input$train_model, {
    req(input$target_variable, input$model_choice)  # Vérifier que les choix nécessaires sont faits
    
    # Charger les données et effectuer la séparation train/test
    dataset <- req(data())  # Assurez-vous que vos données sont chargées et disponibles
    target <- input$target_variable
    split_method <- input$split_method
    
    if (split_method == "Holdout") {
      train_index <- sample(seq_len(nrow(dataset)), size = floor(input$train_percentage / 100 * nrow(dataset)))
      train_data <- dataset[train_index, ]
      test_data <- dataset[-train_index, ]
    } else if (split_method == "Cross-validation") {
      train_data <- dataset  # Dans le cas de la cross-validation, on utilise toutes les données
      test_data <- NULL      # Pas de test direct ici, mais ce sera géré lors de la validation croisée
    }
    
    # Entraîner le modèle selon le choix de l'utilisateur
    model <- NULL
    if (input$model_choice == "SVM") {
      library(e1071)
      model <- svm(
        as.formula(paste(target, "~ .")),
        data = train_data,
        cost = input$svm_C,
        kernel = input$svm_kernel
      )
    } else if (input$model_choice == "Random Forest") {
      library(randomForest)
      model <- randomForest(
        as.formula(paste(target, "~ .")),
        data = train_data,
        ntree = input$rf_trees
      )
    } else if (input$model_choice == "Logistic Regression") {
      library(glmnet)
      x <- model.matrix(as.formula(paste(target, "~ .")), train_data)[, -1]
      y <- train_data[[target]]
      model <- glmnet(
        x, y,
        alpha = ifelse(input$log_reg_penalty_type == "L1 (Lasso)", 1, 0),
        lambda = input$log_reg_penalty_value
      )
    } else if (input$model_choice == "Linear Regression") {
      model <- lm(as.formula(paste(target, "~ .")), data = train_data)
    } else if (input$model_choice == "Decision Tree") {
      library(rpart)
      model <- rpart(
        as.formula(paste(target, "~ .")),
        data = train_data,
        method = ifelse(is.numeric(train_data[[target]]), "anova", "class"),
        control = rpart.control(maxdepth = input$dt_max_depth)
      )
    }
    
    # Stocker le modèle dans une variable réactive
    trained_model(model)
    
    # Message de confirmation
    showNotification("Model trained successfully!", type = "message")
  })
  
  # Condition pour afficher le bouton "Save Model"
  output$model_trained <- reactive({
    !is.null(trained_model())
  })
  outputOptions(output, "model_trained", suspendWhenHidden = FALSE)
  
  # Logique pour le téléchargement du modèle
  output$save_model <- downloadHandler(
    filename = function() {
      paste("trained_model_", Sys.Date(), ".RDS", sep = "")
    },
    content = function(file) {
      saveRDS(trained_model(), file)
    }
  )
  
  # Charger et afficher un exemple de dataset
  data <- reactive({
    infile <- input$file
    req(infile)
    if (grepl("\\.csv$", infile$name)) {
      read.csv(infile$datapath)
    } else if (grepl("\\.xlsx$|\\.xls$", infile$name)) {
      readxl::read_excel(infile$datapath)
    }
  })
  
  # Créer les jeux de données d'entraînement et de test
  train_data <- reactive({
    req(input$file)
    dataset <- read.csv(input$file$datapath)
    set.seed(123)  # Pour garantir la reproductibilité
    
    # Si Holdout est sélectionné
    if (input$split_method == "Holdout") {
      train_index <- sample(1:nrow(dataset), size = input$train_percentage / 100 * nrow(dataset))
      dataset[train_index, ]
    }
    # Si Cross-validation est sélectionné
    else if (input$split_method == "Cross-validation") {
      k <- input$k_folds
      folds <- cut(seq(1, nrow(dataset)), breaks = k, labels = FALSE)
      train_index <- folds != 1  # Par exemple, pour la 1ère fold
      dataset[train_index, ]
    }
  })
  
  # Créer les données de test
  test_data <- reactive({
    req(input$file)
    dataset <- read.csv(input$file$datapath)
    set.seed(123)
    
    # Si Holdout est sélectionné
    if (input$split_method == "Holdout") {
      train_index <- sample(1:nrow(dataset), size = input$train_percentage / 100 * nrow(dataset))
      dataset[-train_index, ]  # Le complément pour le test
    }
    # Si Cross-validation est sélectionné
    else if (input$split_method == "Cross-validation") {
      k <- input$k_folds
      folds <- cut(seq(1, nrow(dataset)), breaks = k, labels = FALSE)
      test_index <- folds == 1  # Par exemple, pour la 1ère fold
      dataset[test_index, ]
    }
  })
  
  
  output$data_preview <- renderDT({
    req(data())
    datatable(data(), options = list(scrollX = TRUE))
  })
  # Reactive expression to compute predictions and metrics
  test_predictions <- reactive({
    req(trained_model(), test_data())  # Ensure model and test data exist
    
    # Make predictions
    predictions <- predict(trained_model(), test_data()[-ncol(test_data())])
    
    # True labels
    true_labels <- test_data()[[input$target_variable]]
    
    # Compute metrics and confusion matrix
    confusion <- table(Predicted = predictions, Actual = true_labels)
    precision <- sum(diag(confusion)) / sum(confusion)
    recall <- diag(confusion) / rowSums(confusion)
    f1_score <- 2 * ((precision * recall) / (precision + recall))
    
    # Return all values
    list(
      predictions = predictions,
      true_labels = true_labels,
      confusion = confusion,
      metrics = list(
        Precision = mean(precision, na.rm = TRUE),
        Recall = mean(recall, na.rm = TRUE),
        Accuracy = sum(diag(confusion)) / sum(confusion),
        F1_Score = mean(f1_score, na.rm = TRUE)
      )
    )
  })
  
  
  # Metrics Output
  output$evaluation_metrics <- renderPrint({
    req(test_predictions())
    metrics <- test_predictions()$metrics
    metrics
  })
  
  # Confusion Matrix Plot
  output$confusion_matrix_plot <- renderPlot({
    req(test_predictions())
    confusion <- test_predictions()$confusion
    heatmap(confusion, main = "Confusion Matrix", col = heat.colors(256), scale = "none", margins = c(5, 5))
  })
  
  # ROC Curve and AUC
  output$roc_curve <- renderPlot({
    req(test_predictions())
    library(pROC)
    
    # Compute ROC Curve
    roc_curve <- roc(
      test_predictions()$true_labels, 
      as.numeric(test_predictions()$predictions), 
      plot = TRUE, 
      col = "blue",
      main = "ROC Curve"
    )
    auc_value <- auc(roc_curve)
    
    # Add AUC to the plot
    legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), col = "blue", lwd = 2)
  })
  
  output$auc_value <- renderPrint({
    req(test_predictions())
    roc_curve <- roc(test_predictions()$true_labels, as.numeric(test_predictions()$predictions))
    auc(roc_curve)
  })
  
  
}