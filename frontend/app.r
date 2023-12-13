# app.R
library(shiny)
library(shinydashboard)
options(shiny.maxRequestSize = 3000 * 1024^2)  # Increase file size limit
library(dplyr)

# Load ncdb_recode.R and dtypes.r scripts
source("/Users/collindougherty/Documents/Work/pipeline/backend/ncdb_recode.R")
source("/Users/collindougherty/Documents/Work/pipeline/backend/dtypes.r")
source("/Users/collindougherty/Documents/Work/pipeline/backend/random_forest_fx.r")
source("/Users/collindougherty/Documents/Work/pipeline/backend/survival_analysis.r")

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
                column(5, uiOutput("choose_analysis_ui"))),

            fluidRow(
                # The dynamic filter UI will be added here
                column(12, uiOutput("dynamicFilterUI"))),

            fluidRow(
                column(12, div(id = "filterArea"))  # Area where new filters will be inserted
            ),

            fluidRow(
                column(4, uiOutput("addFilter")),  # Placeholder for dynamic "Add Filter" button
                column(4, uiOutput("filterButton")),
                column(4, uiOutput("survPlot")) # Assuming this is another dynamic element
            ),

            fluidRow(
                column(12, tableOutput("filteredTable"))),

            fluidRow(
                column(12, uiOutput("controlledVariables"))),

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
            ),

            # Output for the survival plot
            plotOutput("survivalPlot")
            
)))

#########################################################################


server <- function(input, output, session) {
  reactiveDf <- reactiveVal()
  showDropdowns <- reactiveVal(FALSE)
  showButtons <- reactiveVal(FALSE)
  showAnalysisButtons <- reactiveVal(FALSE)
  showSurvivalAnalysisButtons <- reactiveVal(FALSE)
  showAddFilter <- reactiveVal(FALSE)
  showFilterButton <- reactiveVal(FALSE)
  showFilteredTable <- reactiveVal(FALSE)
  showSurvButton <- reactiveVal(FALSE)
  
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
        column(2, selectInput("filterType0", "Control/Tx", choices = c('Control', 'Tx'))),
        column(4, selectInput("variable0", "Variable", choices = names(reactiveDf()))),
        column(2, selectInput("comparison0", "Comparison", choices = c('>', '<', '=='))),
        #column(4, selectizeInput("value", "Value", choices = NULL)),
        column(3, uiOutput("valueInput0")))
    }
  })

filterCounter <- reactiveVal(0)

createFilterUI <- function(id) {
  fluidRow(
    id = paste0("filterRow", id),  # Assign an ID to the fluidRow for easy removal
    column(2, selectInput(paste0("filterType", id), "Control/Tx", choices = c('Control', 'Tx'))),
    column(4, selectInput(paste0("variable", id), "Variable", choices = names(reactiveDf()))),
    column(2, selectInput(paste0("comparison", id), "Comparison", choices = c('>', '<', '=='))),
    column(3, uiOutput(paste0("valueInput", id))),
    column(1, actionButton(paste0("removeFilter", id), "X", class = "btn-danger"))
  )
}

observeEvent(input$addFilter, {
  currentFilterCount <- filterCounter()
  filterCounter(currentFilterCount + 1)
  insertUI("#filterArea", where = "beforeEnd", ui = createFilterUI(filterCounter()))
})



#############
  observeEvent(input$value0, {
      showAddFilter(TRUE)
      showFilterButton(TRUE)
      showSurvButton(TRUE)
    })

    output$addFilter <- renderUI({
      if(showAddFilter()) {
        actionButton("addFilter", "Add Filter")
      }
    })

    output$filterButton <- renderUI({
      if(showFilterButton()) {
        actionButton("filterButton", "Apply Filter(s)")
      }
    })

    output$survPlot <- renderUI({
      if(showSurvButton()) {
        actionButton("survPlot", "Plot Survival Curve")
      }
    })


  observeEvent(input$variable0, {
    varSelected <- input$variable0

    if(is.factor(reactiveDf()[[varSelected]])) {
      # Update comparison options for factor variables
      updateSelectInput(session, "comparison0", choices = c("==", "!="))

      levels_subset <- levels(reactiveDf()[[varSelected]])
      # Load a subset of levels if there are too many
      if(length(levels_subset) > 1000) {
        levels_subset <- levels_subset[1:1000]
      }

      output$valueInput <- renderUI({
        selectizeInput("value0", "Value", choices = levels_subset, multiple = TRUE,
                       options = list(placeholder = "Type to search"))
      })
    } else if(is.numeric(reactiveDf()[[varSelected]])) {
      # Update comparison options for numeric variables
      updateSelectInput(session, "comparison0", choices = c(">", "<", "=="))

      # Render sliderInput for numeric variables
      output$valueInput <- renderUI({
        sliderInput("value0", "Value", min = min(reactiveDf()[[varSelected]], na.rm = TRUE), 
                    max = max(reactiveDf()[[varSelected]], na.rm = TRUE), 
                    value = median(reactiveDf()[[varSelected]], na.rm = TRUE))
      })
    } else {
      # Fallback UI for other types
      output$valueInput <- renderUI({
        textInput("value0", "Value", value = "")
      })
    }
  }, ignoreNULL = FALSE)



# Server logic to render the value input dynamically based on the selected variable
observe({
  lapply(1:filterCounter(), function(i) {
    observeEvent(input[[paste0("variable", i)]], {
      varSelected <- input[[paste0("variable", i)]]
      req(varSelected, reactiveDf())

      # Observer for the remove button of each filter
      observeEvent(input[[paste0("removeFilter", i)]], {
      # Use the ID of the fluidRow to remove the filter UI
      removeUI(selector = paste0("#filterRow", i))
      
      # Update the filter counter by decrementing it
      currentFilterCount <- filterCounter()
      if (currentFilterCount > 0) {
        filterCounter(currentFilterCount - 1)
      }})
      
      # Output the UI for the value input based on the type of variable selected
      output[[paste0("valueInput", i)]] <- renderUI({
        if(is.factor(reactiveDf()[[varSelected]])) {
          levels_subset <- levels(reactiveDf()[[varSelected]])
          # Load a subset of levels if there are too many
            if(length(levels_subset) > 1000) {
              levels_subset <- levels_subset[1:1000]
            }
          # Update comparison options for factor variables
          updateSelectInput(session, paste0("comparison", i), choices = c("==", "!="))
          
          # Render selectizeInput for factor variables
          selectizeInput(paste0("value", i), "Value", 
                         choices = levels_subset, multiple = TRUE,
                         options = list(placeholder = "Type to search"))
        } else if(is.numeric(reactiveDf()[[varSelected]])) {
          # Update comparison options for numeric variables
          updateSelectInput(session, paste0("comparison", i), choices = c(">", "<", "=="))
          
          # Render sliderInput for numeric variables
          sliderInput(paste0("value", i), "Value", 
                      min = min(reactiveDf()[[varSelected]], na.rm = TRUE), 
                      max = max(reactiveDf()[[varSelected]], na.rm = TRUE), 
                      value = median(reactiveDf()[[varSelected]], na.rm = TRUE))
        } else {
          # Fallback UI for other types
          textInput(paste0("value", i), "Value", value = "")
        }
      })
    }, ignoreNULL = TRUE)
  })
})






# Reactive expression to hold the filtered data
filteredData <- reactive({
  # Start with the unfiltered data
  df <- reactiveDf()
  control <- df
  treatment <- df

  # Ensure the reactive data frame is available
  req(df, control, treatment)

  # Loop over all filters and apply them sequentially
  for(i in 0:filterCounter()) {
    # Retrieve the inputs for the current filter
    varSelected <- input[[paste0("variable", i)]]
    comp <- input[[paste0("comparison", i)]]
    val <- input[[paste0("value", i)]]
    group <- input[[paste0("filterType", i)]]

    # Check if the inputs are not NULL
    if (!is.null(varSelected) && !is.null(comp) && !is.null(val) && group == "Control") {
      # Apply the filter based on the type of variable and the comparison selected
      if (is.factor(control[[varSelected]])) {
        if (comp == "==") {
          control <- control[control[[varSelected]] %in% val, ]
        } else if (comp == "!=") {
          control <- control[!control[[varSelected]] %in% val, ]
        }
      } else if (is.numeric(control[[varSelected]])) {
        val <- as.numeric(val)  # Convert value to numeric
        if (comp == ">") {
          control <- control[control[[varSelected]] > val, ]
        } else if (comp == "<") {
          control <- control[control[[varSelected]] < val, ]
        } else if (comp == "==") {
          control <- control[control[[varSelected]] == val, ]
        }
      }
    }
    else if (!is.null(varSelected) && !is.null(comp) && !is.null(val) && group == "Tx") {
      # Apply the filter based on the type of variable and the comparison selected
      if (is.factor(treatment[[varSelected]])) {
        if (comp == "==") {
          treatment <- treatment[treatment[[varSelected]] %in% val, ]
        } else if (comp == "!=") {
          treatment <- treatment[!treatment[[varSelected]] %in% val, ]
        }
      } else if (is.numeric(treatment[[varSelected]])) {
        val <- as.numeric(val)  # Convert value to numeric
        if (comp == ">") {
          treatment <- treatment[treatment[[varSelected]] > val, ]
        } else if (comp == "<") {
          treatment <- treatment[treatment[[varSelected]] < val, ]
        } else if (comp == "==") {
          treatment <- treatment[treatment[[varSelected]] == val, ]
        }
      }
    }
  }

  # lastly join the two dataframes row-wise into df
  # Combine dataframes with bind_rows and remove duplicate rows
  combined_df <- bind_rows(control, treatment) %>%
    distinct()

  # Return all 3 filtered data frames
  # Return all filtered data frames as a list
  list(control = control, treatment = treatment, combined = combined_df)
})


observeEvent(input$filterButton, {
  # Trigger re-rendering of the table with the current filtered data
  output$filteredTable <- renderTable({
    # Using isolate to prevent reactivity from anything other than the filter button click
    isolate({
      head(filteredData()$combined, 10)
    })
  }, server = TRUE)
})


  # Define the columns to exclude
  excluded_cols <- c("DX_LASTCONTACT_DEATH_MONTHS", "PUF_VITAL_STATUS_RECODE") # Replace with your actual column names

  output$controlledVariables <- renderUI({
    if(showSurvButton()) {
      # Get the list of variable names, excluding the survival-related ones
      var_names <- setdiff(names(reactiveDf()), excluded_cols)

      # Create a list of choices with 'Select All' option
      choices_list <- c("Select All" = "all", var_names)

      selectInput("controlled_vars", 
                  "Choose Controlled Variables:", 
                  choices = choices_list, 
                  multiple = TRUE)
    }
  })

  observeEvent(input$controlled_vars, {
    if("all" %in% input$controlled_vars) {
      # Update the input to select all variables
      updateSelectInput(session, "controlled_vars", 
                        selected = setdiff(names(reactiveDf()), excluded_cols))
    }
  })


observeEvent(input$survPlot, {
  # Get the user-selected controlled variables
  controlled_vars <- input$controlled_vars

  # Check if "all" is selected and adjust accordingly
  if("all" %in% controlled_vars) {
    controlled_vars <- setdiff(names(filteredData()$control), 
                               c("DX_LASTCONTACT_DEATH_MONTHS", "PUF_VITAL_STATUS_RECODE"))
  }

  # Call the survival analysis function with controlled variables as additional arguments
  p <- survival_analysis_fx(
    control = filteredData()$control, 
    treatment = filteredData()$treatment, 
    time_col = "DX_LASTCONTACT_DEATH_MONTHS", 
    status_col = "PUF_VITAL_STATUS_RECODE",
    covariates = controlled_vars
  )

  # Render the survival plot
  output$survivalPlot <- renderPlot({
    p
  })
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
