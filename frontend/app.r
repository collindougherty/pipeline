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

            fluidRow(
                column(4, offset = 4,
                       fileInput("file1", "Choose CSV File",
                                 accept = c("text/csv", 
                                            "text/comma-separated-values,text/plain", 
                                            ".csv")))),

            fluidRow(
                column(5, uiOutput("choose_analysis_ui")),
                # The dynamic filter UI will be added here
                column(8, uiOutput("dynamicFilterUI"))),

            fluidRow(
                column(12, tableOutput("filterButton"))),

            fluidRow(
                column(12, tableOutput("filteredTable"))),

            fluidRow(
                column(6, uiOutput("x_vars_ui")),
                column(6, uiOutput("y_var_ui"))),

            fluidRow(
                column(4, offset = 4,
                       uiOutput("submit_ui"))),

            fluidRow(
                column(12, tableOutput("table")),
                column(12, textOutput("proceedMessage")),
                column(4, uiOutput("rfButtonUI")),
                column(4, uiOutput("lrButtonUI")),
                column(4, uiOutput("kmButtonUI"))),

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
            ),
            
            # Placeholder for the valueBox
            fluidRow(
              valueBoxOutput("lrValueBox_acc")
            ),
            # Placeholder for the valueBox
            fluidRow(
              valueBoxOutput("lrValueBox")
            )
            
)))

#########################################################################


server <- function(input, output, session) {
  reactiveDf <- reactiveVal()
  showDropdowns <- reactiveVal(FALSE)
  showButtons <- reactiveVal(FALSE)
  showAnalysisButtons <- reactiveVal(FALSE)
  showSurvivalAnalysisButtons <- reactiveVal(FALSE)
  
  observeEvent(input$file1, {
    req(input$file1)
    withProgress(message = 'Processing data...', value = 0, {
      setProgress(value = 0.25)
      df <- read.csv(input$file1$datapath)
      setProgress(value = 0.50)
      recodedDf <- ncdb_recode(df)
      setProgress(value = 0.75)
      dtypes_data <- dtype(recodedDf)
      #ncdb_drop_ids(dtypes_data)
      reactiveDf(dtypes_data)
      setProgress(value = 1)
    })
  })

  output$choose_analysis_ui <- renderUI({
    if(!is.null(input$file1)) {
      selectInput("choose_analysis", "Choose Analysis:", choices = c("Survival Analysis", "Predictive Modeling"))
    }
  })

  observeEvent(input$choose_analysis, {
    if(input$choose_analysis == "Survival Analysis") {
      showDropdowns(FALSE)
      showButtons(FALSE)
      showAnalysisButtons(FALSE)
      showSurvivalAnalysisButtons(TRUE)
    } else if(input$choose_analysis == "Predictive Modeling") {
      showDropdowns(TRUE)
      showButtons(FALSE)
      showAnalysisButtons(FALSE)
      showSurvivalAnalysisButtons(FALSE)
    }
  })









 # Dynamic filter UI
  output$dynamicFilterUI <- renderUI({
    if(showSurvivalAnalysisButtons()) {
      fluidRow(
        column(2, 
          selectInput("filterType", "Control/Tx", 
            choices = c('Control', 'Tx'))),
        column(4, 
          selectInput("variable", "Variable", 
            choices = names(reactiveDf()))),
        column(2, 
          selectInput("comparison", "Comparison", 
            choices = c('>', '<', '=='))),
        #column(4, selectizeInput("value", "Value", choices = NULL)),
        column(4, uiOutput("valueInput")),
        actionButton("filterButton", "Apply Filter"))
    }
  })





  observeEvent(input$variable, {
    varSelected <- input$variable

    if(is.factor(reactiveDf()[[varSelected]])) {
      # Update comparison options for factor variables
      updateSelectInput(session, "comparison", choices = c("==", "!="))

      levels_subset <- levels(reactiveDf()[[varSelected]])
      # Load a subset of levels if there are too many
      if(length(levels_subset) > 1000) {
        levels_subset <- levels_subset[1:1000]
      }

      output$valueInput <- renderUI({
        selectizeInput("value", "Value", choices = levels_subset, multiple = TRUE,
                       options = list(placeholder = "Type to search"))
      })
    } else if(is.numeric(reactiveDf()[[varSelected]])) {
      # Update comparison options for numeric variables
      updateSelectInput(session, "comparison", choices = c(">", "<", "=="))

      # Render sliderInput for numeric variables
      output$valueInput <- renderUI({
        sliderInput("value", "Value", min = min(reactiveDf()[[varSelected]], na.rm = TRUE), 
                    max = max(reactiveDf()[[varSelected]], na.rm = TRUE), 
                    value = median(reactiveDf()[[varSelected]], na.rm = TRUE))
      })
    } else {
      # Fallback UI for other types
      output$valueInput <- renderUI({
        textInput("value", "Value", value = "")
      })
    }
  }, ignoreNULL = FALSE)


  # Reactive expression to hold the filtered data
  filteredData <- reactive({
    # Make sure the dataframe is available and the inputs are set
    req(reactiveDf(), input$variable, input$comparison, input$value)
    df <- reactiveDf()
    varSelected <- input$variable
    comp <- input$comparison
    val <- input$value

    # Apply the filter based on the type of variable and the comparison selected
    if(is.factor(df[[varSelected]])) {
      if(comp == "==") {
        df <- df[df[[varSelected]] %in% val, ]
      } else if(comp == "!=") {
        df <- df[!df[[varSelected]] %in% val, ]
      }
    } else if(is.numeric(df[[varSelected]])) {
      if(comp == ">") {
        df <- df[df[[varSelected]] > as.numeric(val), ]
      } else if(comp == "<") {
        df <- df[df[[varSelected]] < as.numeric(val), ]
      } else if(comp == "==") {
        df <- df[df[[varSelected]] == as.numeric(val), ]
      }
    }
    df
  })

  # Display the filtered data
  output$filteredTable <- renderTable({
    # Render the table only when the filter button is clicked
    # This prevents filtering with incomplete user inputs
    input$filterButton
    # This is necessary to isolate the button event from the reactive expression
    isolate({
      head(filteredData(), 10)
    })
  }, server = TRUE)  # Using server-side processing if the table is large





















  
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
    # Show a progress bar
    withProgress(message = 'Running linear regression model', value = 0, {
      req(selectedDf(), input$x_vars, input$y_var)
      results <- random_forest_fx(input$x_vars, input$y_var, selectedDf())
      # Update progress bar after each step
      for(i in 1:10) {
        incProgress(1/10)
        Sys.sleep(0.1)
      }}) # This represents the time-consuming model fitting process
    
    
    analysisResults(results)
    
    # Render the valueBox in the server output
    output$lrValueBox_acc <- renderValueBox({
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
    output$lrValueBox <- renderValueBox({
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
