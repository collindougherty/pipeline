options(shiny.maxRequestSize = 3000 * 1024^2)  # Increase file size limit

# Load ncdb_recode.R and dtypes.r scripts
source("/Users/collindougherty/Documents/Work/pipeline/backend/ncdb_recode.R")
source("/Users/collindougherty/Documents/Work/pipeline/backend/dtypes.r")

ui <- fluidPage(
  tags$head(
	@@ -67,102 +67,99 @@ server <- function(input, output, session) {
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

