# app.R
library(shiny)
library(shinydashboard)
options(shiny.maxRequestSize = 3000 * 1024^2)  # Increase file size limit

# Load ncdb_recode.R and dtypes.r scripts
source("/Users/collindougherty/Documents/Work/pipeline/backend/ncdb_recode.R")
source("/Users/collindougherty/Documents/Work/pipeline/backend/dtypes.r")
source("/Users/collindougherty/Documents/Work/pipeline/backend/random_forest_fx.r")

ui <- fluidPage(
    dashboardPage(
        dashboardHeader(),
        dashboardSidebar(),
        dashboardBody(
            # Your existing UI components
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
            # fluidRow(
            #     column(12, tableOutput("analysisResults"))
            # ),
            # Placeholder for the valueBox
            fluidRow(
              valueBoxOutput("rfValueBox_acc")
            ),
            # Placeholder for the valueBox
            fluidRow(
              valueBoxOutput("rfValueBox")
            )
            
)))

#########################################################################


server <- function(input, output, session) {
  reactiveDf <- reactiveVal()
  showDropdowns <- reactiveVal(FALSE)
  showButtons <- reactiveVal(FALSE)
  showAnalysisButtons <- reactiveVal(FALSE)
  
  observeEvent(input$file1, {
    req(input$file1)
    withProgress(message = 'Processing data...', value = 0, {
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
  
  
  
  
  analysisResults <- reactiveVal()
  

  
  observeEvent(input$rfButton, {
    # Show a progress bar
    withProgress(message = 'Running random forest model', value = 0, {
      req(selectedDf(), input$x_vars, input$y_var)
      results <- random_forest_fx(input$x_vars, input$y_var, selectedDf())
      # Update progress bar after each step
      for(i in 1:10) {
        incProgress(1/10)
        Sys.sleep(0.1)
        }}) # This represents the time-consuming model fitting process


    analysisResults(results)
    
    # Render the valueBox in the server output
    output$rfValueBox_acc <- renderValueBox({
      req(analysisResults())  # Ensure analysisResults is available
      # This is a placeholder, replace with your actual metric calculation
      metric <- analysisResults()[[".estimate"]][analysisResults()[[".metric"]] == "accuracy"]
      valueBox(
        value = round(metric,2),
        subtitle = "Accuracy",
        icon = icon("area-chart"),
        color = if(metric > .80) "green" else if(metric > .50) "yellow" else "red"
      )
    })

    # Render the valueBox in the server output
    output$rfValueBox <- renderValueBox({
        req(analysisResults())  # Ensure analysisResults is available
        # This is a placeholder, replace with your actual metric calculation
        metric <- analysisResults()[[".estimate"]][analysisResults()[[".metric"]] == "kap"]
        valueBox(
            value = round(metric,2),
            subtitle = "Cohen's Kappa",
            icon = icon("area-chart"),
            color = if(metric > .80) "green" else if(metric > .50) "yellow" else "red"
        )
    })
    

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
