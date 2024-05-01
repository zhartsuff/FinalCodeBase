#Code is for the model dashboard and final application of our project.

library(shiny)
library(flexdashboard)
library(DT)
library(dplyr)
library(plotly)
library(pROC)
library(randomForest)
library(xgboost)
library(forecast)

# Define UI
ui <- navbarPage("Model Dashboard: Senior Project",
                 header = tagList(
                   tags$style(type="text/css", ".navbar { margin-bottom: 0; }"),
                   div(
                     class = "navbar-custom-menu",
                     actionButton("helpBtn", "Help", icon = icon("info-circle"), class = "btn btn-info navbar-btn")
                   )
                 ),
                 tabPanel("Model Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              fileInput("dataset_upload", "Choose CSV File",
                                        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                              uiOutput("variable_selector"),
                              uiOutput("predictor_selector"),
                              selectInput("model_type", label = h5("Choose model type"),
                                          choices = c("Linear Regression" = "lm",
                                                      "Logistic Regression" = "glm",
                                                      "Random Forest Regression" = "rfr",
                                                      "Random Forest Classification" = "rfc",
                                                      "XG Boost" = "xgb"), selected = "lm"),
                              actionButton("run_model", "Run Model")
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Model Summary", verbatimTextOutput("model_summary")),
                                tabPanel("Data Table", DTOutput("data_table")),
                                tabPanel("Scatter Plot", plotlyOutput("scatter_plot")),
                                tabPanel("Histogram", plotlyOutput("histogram_plot"))
                              )
                            )
                          )
                 ),
                 tabPanel("Time Series Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              fileInput("ts_dataset_upload", "Upload Time Series CSV",
                                        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                              selectInput("ts_time_column", "Select Time Column", choices = NULL),
                              selectInput("ts_variable", "Select Variable to Analyze", choices = NULL),
                              actionButton("run_ts_analysis", "Run Time Series Analysis"),
                              downloadButton("downloadData", "Download Data with Predictions")
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Time Series Plot", plotOutput("ts_plot")),
                                tabPanel("Forecast Plot", plotOutput("ts_forecast_plot")),
                                tabPanel("Cycle Boxplot", plotOutput("cycle_boxplot"))
                              )
                            )
                          ))
)

# Define server logic
server <- function(input, output, session) {
  uploaded_data <- reactive({
    req(input$dataset_upload)
    
    inFile <- input$dataset_upload
    read.csv(inFile$datapath, stringsAsFactors = FALSE)
  })
  
  output$variable_selector <- renderUI({
    data <- uploaded_data()
    if (!is.null(data)) {
      selectInput("variable_choice", label = h5("Choose one or more predictor variables"), 
                  choices = names(data), selected = names(data)[1], multiple = TRUE)
    }
  })
  
  output$predictor_selector <- renderUI({
    data <- uploaded_data()
    if (!is.null(data)) {
      selectInput("predictor_variable", label = h5("Choose response variable: "), 
                  choices = names(data), selected = names(data)[1], multiple = FALSE)
    }
  })
  
  # Create a reactive expression for the model that triggers when the 'Run Model' button is clicked
  model1 <- eventReactive(input$run_model, {
    req(uploaded_data()) 
    data <- uploaded_data()
    
    formula_str <- paste(paste(input$predictor_variable), "~", paste(input$variable_choice, collapse = " + "))
    if (input$model_type == "lm") {
      lm(formula_str, data = data)
    } else if (input$model_type == "glm") {
      glm(formula_str, data = data, family = binomial())
    } else if(input$model_type == "rfc") {
      model <- randomForest(x = data[, input$variable_choice], y = data[[input$predictor_variable]], 
                            ntree = 25,
                            type = ifelse(is.factor(data[[input$predictor_variable]]), "classification", "regression"))
    } else if(input$model_type == "rfr") {
      model <- randomForest(x = data[, input$variable_choice, drop = FALSE], y = data[[input$predictor_variable]],
                            ntree = 25) 
    } else if(input$model_type == "xgb"){
      dtrain <- xgb.DMatrix(data = as.matrix(data[, input$variable_choice, drop = FALSE]), label = data[[input$predictor_variable]])
      
      params <- list(objective = "reg:squarederror")
      
      model <- xgboost(params = params, data = dtrain, nrounds = 100, verbose = 0)
    } else {
      NULL
    }
  }, ignoreNULL = FALSE)
  
  output$model_summary <- renderPrint({
    req(model1())
    model <- model1()
    if (input$model_type == "xgb") {
      data <- uploaded_data()
      dtest <- xgb.DMatrix(data = as.matrix(data[, input$variable_choice]))
      predictions <- predict(model, dtest)
      
      actuals <- data[[input$predictor_variable]]
      n<-length(actuals)
      mean_actuals <- mean(actuals)
      ss_total <- sum((actuals - mean_actuals)^2)
      ss_res <- sum((actuals - predictions)^2)
      rsq <- 1 - ss_res / ss_total
      res <- sqrt(ss_res/(n-2))
      
      # Print the importance matrix
      importance_matrix <- xgb.importance(feature_names = colnames(dtest), model = model)
      print(importance_matrix)
      
      cat("\nR-squared for XGBoost model:", rsq, "\n")
      cat("\nResidual Standard Error: ", ss_res, "\n")
    } else {
      print(summary(model1()))
    }
  })
  
  output$data_table <- renderDT({
    req(model1())
    data <- uploaded_data()
    
    if (input$model_type == "xgb") {
      dtest <- xgb.DMatrix(data = as.matrix(data[, input$variable_choice]))
      predictions <- predict(model1(), dtest)
      data$residuals <- data[[input$predictor_variable]] - predictions
    } else {
      predictions <- predict(model1(), newdata = data, type = ifelse(input$model_type %in% c("rfc", "glm"), "response", "response"))
    }
    
    if (input$model_type %in% c("rfr", "lm")) {
      data$residuals <- with(data, data[[input$predictor_variable]] - predictions)
    }
    
    data$predicted <- predictions
    
    display_columns <- c(input$predictor_variable, "predicted")
    if (input$model_type %in% c("rfr", "lm", "xgb")) {
      display_columns <- c(display_columns, "residuals")
    }
    
    display_columns <- c(display_columns, input$variable_choice)
    
    datatable(data[, display_columns, drop = FALSE], options = list(pageLength = 10))
  })
  
  output$scatter_plot <- renderPlotly({
  req(model1())
  data <- uploaded_data()
  
  # Generate predictions
  if (input$model_type == "xgb") {
    dtest <- xgb.DMatrix(data = as.matrix(data[, input$variable_choice]))
    predictions <- predict(model1(), dtest)
  } else {
    predictions <- predict(model1(), newdata = data)
  }
  
  # Add predictions as a new column in the dataset
  data$predicted <- predictions
  
  actual_values <- data[[input$predictor_variable]]
  if (!is.numeric(actual_values)) {
    stop("The predictor variable must be numeric for a scatter plot.")
  }
  if (!is.numeric(predictions)) {
    stop("Predicted values must be numeric for a scatter plot.")
  }
  
  # Create the scatter plot with actual vs predicted values
  plot_ly(data = data, x = ~actual_values, y = ~predicted,
          type = 'scatter', mode = 'markers') %>%
    layout(xaxis = list(title = input$predictor_variable),
           yaxis = list(title = 'Predicted'))
})

  
  
  output$histogram_plot <- renderPlotly({
    req(model1())  # Ensure the model is calculated
    data <- uploaded_data()
    
    if (input$model_type %in% c("rfr", "lm", "xgb")) {
      if (input$model_type == "xgb") {
        dtest <- xgb.DMatrix(data = as.matrix(data[, input$variable_choice]))
        predictions <- predict(model1(), dtest)
      } else {
        predictions <- predict(model1(), newdata = data)
      }
      
      # Calculate residuals for regression models
      residuals <- data[[input$predictor_variable]] - predictions
      
      plot_ly(x = ~residuals, type = 'histogram') %>%
        layout(title = "Residuals Distribution",
               xaxis = list(title = "Residuals"),
               yaxis = list(title = "Frequency"))
    } else if (input$model_type == "rfc" || input$model_type == "glm") {
      # Classification models: ROC curve plotting
      req(pROC)
      pred_probs <- predict(model1(), newdata = data, type = "response")
      actual_outcomes <- data$YourBinaryOutcomeColumn
      roc_curve <- roc(response = actual_outcomes, predictor = pred_probs)
      auc_value <- auc(roc_curve)
      
      plot_ly(x = ~roc_curve$specificities, y = ~roc_curve$sensitivities, type = 'scatter', mode = 'lines',
              name = paste("AUC:", round(auc_value, 3)),
              line = list(color = 'blue')) %>%
        layout(title = "ROC Curve",
               xaxis = list(title = "1 - Specificity"),
               yaxis = list(title = "Sensitivity"))
    } else {
      plot_ly() %>%
        add_annotations(text = "Plot not applicable for this model type",
                        x = 0.5, y = 0.5, showarrow = F, font = list(size = 20))
    }
  })
  
  processedData <- eventReactive(input$run_ts_analysis, {
    req(input$ts_dataset_upload)  # Ensure file is uploaded
    inFile <- input$ts_dataset_upload
    
    # Read the uploaded file
    ts_data <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
    
    # Process dates and remove year and month columns
    ts_data <- ts_data %>%
      mutate(
        date = mdy(date), 
        year = year(date),
        month = month(date)
      ) %>%
      arrange(date) %>%
      select(-year, -month)
    
    return(ts_data)
  })
  
  observe({
    req(input$ts_dataset_upload)
    data <- tryCatch({
      read.csv(input$ts_dataset_upload$datapath)
    }, error = function(e) {
      return(NULL)
    })
    
    if (!is.null(data)) {
      updateSelectInput(session, "ts_time_column", choices = names(data))
      updateSelectInput(session, "ts_variable", choices = names(data))
    }
  })
  
  # Create time series object
  reactiveTimeSeries <- reactive({
    req(processedData(), input$ts_variable)
    data <-processedData()
    
    if (nrow(data) == 0) {
      return(ts())
    }
    
    # Extract the time series column based on user input
    ts_values <- as.numeric(data[[input$ts_variable]])
    ts_start_year <- year(min(data$date))
    ts_start_month <- month(min(data$date))
    
    # Create the time series object
    ts(ts_values, frequency = 12, start = c(ts_start_year, ts_start_month))
  })
  
  output$ts_plot <- renderPlot({
    ts_data <- reactiveTimeSeries()
    if (is.null(ts_data) || length(ts_data) == 0) {
      plot.new()
      title(main = "No valid data available")
    } else {
      plot.ts(ts_data, main = "Time Series Plot", ylab = "Values", xlab = "Time")
      abline(reg = lm(ts_data ~ time(ts_data)), col = "red")
    }
  })
  
  output$ts_forecast_plot <- renderPlot({
    ts_data <- reactiveTimeSeries()
    if (is.null(ts_data) || length(ts_data) == 0) {
      plot.new()
      title(main = "No valid data available")
    } else {
      fit <- auto.arima(log(ts_data))
      future <- forecast(fit, h = 24)
      ts.plot(ts_data, exp(future$mean), log = "y", lty = c(1, 3), col = c("blue", "red"),
              main = "Forecast Plot", ylab = "Values", xlab = "Time")
    }
  })
  
  # Cycle boxplot output
  output$cycle_boxplot <- renderPlot({
    req(processedData())
    ts_data <- processedData()
    ts_values <- as.numeric(ts_data[[input$ts_variable]])
    ts_series <- ts(ts_values, frequency = 12)
    boxplot(ts_series ~ cycle(ts_series), main = "Cycle Boxplot", xlab = "Month", ylab = "Values")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data_with_predictions-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      req(processedData())
      data <- processedData()
      ts_values <- as.numeric(data[[input$ts_variable]])
      ts_series <- ts(ts_values, frequency = 12)
      fit <- auto.arima(ts_series)
      future <- forecast(fit, h = 24)
      predictions <- data.frame(Time = time(future$mean), Predicted = future$mean)
      write.csv(predictions, file)
    }
  )
  
  observeEvent(input$helpBtn, {
    showModal(modalDialog(
      title = "Help",
      HTML("
        <h4>Model Descriptions:</h4>
        <ul>
          <li><b>Linear Regression (LM):</b> A regression model that estimates the relationship between one independent variable and one dependent variable using a straight line</li>
          <li><b>Logistic Regression (GLM):</b> A process of modeling the probability of a discrete outcome given an input variable. Binary data.</li>
          <li><b>Random Forest Regression (RFR):</b> A type of ensemble learning method for regression.</li>
          <li><b>Random Forest Classification (RFC):</b> A type of ensemble learning method for classification.</li>
          <li><b>XGBoost (XGB):</b> An implementation of gradient boosted decision trees designed for speed and performance on regression variables.</li>
        </ul>
        <h4>What R-squared Tells You:</h4>
        <p>R-squared is a statistical measure of how close the data are to the fitted regression line. It is also known as the coefficient of determination, or the coefficient of multiple determination for multiple regression. The higher the R-squared, the better the model fits your data.</p>
        <h4>Time Series Analysis:</h4>
        <p>Time Series Analysis allows you to analyze data when the data is collected over a set period of time over specific intervals</p>
        <p>The cycle graph allows you to see clearer when certain months have greater importance then others</p>
      "),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
