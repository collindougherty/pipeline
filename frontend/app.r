# app.R
library(shiny)
options(shiny.maxRequestSize = 3000 * 1024^2)  # Increase file size limit

# Load ncdb_recode.R and dtypes.r scripts
source("/Users/collindougherty/Documents/Work/pipeline/backend/ncdb_recode.R")
source("/Users/collindougherty/Documents/Work/pipeline/backend/dtypes.r")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
            .centered-title {
                text-align: center;
                margin-top: 20px;
                margin-bottom: 20px;
            }
        "))
  ),
  tags$div(class = "centered-title", 
           titlePanel("NCDB Data Analysis")
  ),
  fluidRow(
    column(4, offset = 4,
           fileInput("file1", "Choose CSV File",
                     accept = c("text/csv", 
                                "text/comma-separated-values,text/plain", 
                                ".csv"))
    )
  ),
  fluidRow(
    column(6, uiOutput("x_vars_ui")),
    column(6, uiOutput("y_var_ui"))
  ),
  fluidRow(
    column(4, offset = 4,
           uiOutput("submit_ui"))
  ),
  fluidRow(
    column(12, tableOutput("table")),
    column(12, textOutput("proceedMessage")),
    column(4, uiOutput("rfButtonUI")),
    column(4, uiOutput("lrButtonUI")),
    column(4, uiOutput("kmButtonUI"))
  ),
  fluidRow(
    column(12, tableOutput("analysisResults"))
  )
)

server <- function(input, output, session) {
  reactiveDf <- reactiveVal()
  showDropdowns <- reactiveVal(FALSE)
  showButtons <- reactiveVal(FALSE)
  showAnalysisButtons <- reactiveVal(FALSE)
  
  observeEvent(input$file1, {
    req(input$file1)
    withProgress(message = 'Uploading and processing data...', value = 0, {
      setProgress(value = 0.25)
      df <- read.csv(input$file1$datapath)
      setProgress(value = 0.50)
      recodedDf <- ncdb_recode(df)
      setProgress(value = 0.75)
      dtypes_data <- dtype(recodedDf)
      reactiveDf(dtypes_data)
      setProgress(value = 1)
    })
    showDropdowns(TRUE)
  })
  
  output$x_vars_ui <- renderUI({
    if(showDropdowns()) {
      selectInput("x_vars", "Choose X Variables:", choices = names(reactiveDf()), multiple = TRUE)
    }
  })
  
  output$y_var_ui <- renderUI({
    if(showDropdowns()) {
      availableYChoices <- setdiff(names(reactiveDf()), input$x_vars)
      selectInput("y_var", "Choose Y Variable:", choices = availableYChoices, multiple = FALSE)
    }
  })
  
  observe({
    if(length(input$x_vars) > 0 && !is.null(input$y_var)) {
      showButtons(TRUE)
    } else {
      showButtons(FALSE)
    }
  })
  
  output$submit_ui <- renderUI({
    if(showButtons()) {
      actionButton("submit", "Submit", class = "btn-primary")
    }
  })
  
  selectedDf <- reactiveVal()
  
  observeEvent(input$submit, {
    selectedDf(reactiveDf()[, c(input$x_vars, input$y_var), drop = FALSE])
    showAnalysisButtons(TRUE)
  })
  
  output$proceedMessage <- renderText({
    if(showAnalysisButtons()) {
      "If these look correct, choose an analysis method."
    }
  })
  
  output$rfButtonUI <- renderUI({
    if(showAnalysisButtons()) {
      actionButton("rfButton", "Random Forest", class = "btn-primary")
    }
  })
  
  output$lrButtonUI <- renderUI({
    if(showAnalysisButtons()) {
      actionButton("lrButton", "Linear Regression", class = "btn-primary")
    }
  })
  
  output$kmButtonUI <- renderUI({
    if(showAnalysisButtons()) {
      actionButton("kmButton", "K-Means Clustering", class = "btn-primary")
    }
  })
  
  # Placeholder functions for analyses
  random_forest <- function(x_vars, y_var, df) { return(data.frame(Result = "Random Forest")) }
  linear_regression <- function(x_vars, y_var, df) { return(data.frame(Result = "Linear Regression")) }
  k_means_clustering <- function(x_vars, df) { return(data.frame(Result = "K-Means Clustering")) }
  
  analysisResults <- reactiveVal()
  
  observeEvent(input$rfButton, {
    req(selectedDf(), input$x_vars, input$y_var)
    results <- random_forest(input$x_vars, input$y_var, selectedDf())
    analysisResults(results)
  })
  
  observeEvent(input$lrButton, {
    req(selectedDf(), input$x_vars, input$y_var)
    results <- linear_regression(input$x_vars, input$y_var, selectedDf())
    analysisResults(results)
  })
  
  observeEvent(input$kmButton, {
    req(selectedDf(), input$x_vars)
    results <- k_means_clustering(input$x_vars, selectedDf())
    analysisResults(results)
  })
  
  output$table <- renderTable({
    req(selectedDf())
    head(selectedDf())
  })
  
  output$analysisResults <- renderTable({
    req(analysisResults())
    analysisResults()
  })
}

shinyApp(ui = ui, server = server)

    

