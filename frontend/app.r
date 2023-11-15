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
    column(12, tableOutput("table"))
  ),
  fluidRow(
    column(12, textOutput("proceedMessage"))
  )
)

server <- function(input, output, session) {
  reactiveDf <- reactiveVal()
  showDropdowns <- reactiveVal(FALSE)
  
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
      selectInput("x_vars", "Choose X Variables:", 
                  choices = names(reactiveDf()), 
                  multiple = TRUE)
    }
  })
  
  output$y_var_ui <- renderUI({
    if(showDropdowns()) {
      availableYChoices <- setdiff(names(reactiveDf()), input$x_vars)
      selectInput("y_var", "Choose Y Variable:", 
                  choices = availableYChoices, 
                  multiple = FALSE)
    }
  })
  
  observeEvent(input$x_vars, {
    updateSelectInput(session, "y_var", choices = setdiff(names(reactiveDf()), input$x_vars))
  })
  
  output$submit_ui <- renderUI({
    if(showDropdowns()) {
      actionButton("submit", "Submit", class = "btn-primary")
    }
  })
  
  selectedDf <- reactiveVal()
  
  observeEvent(input$submit, {
    req(input$x_vars, input$y_var, reactiveDf())
    selectedDf(reactiveDf()[, c(input$x_vars, input$y_var), drop = FALSE])
  })
  
  output$table <- renderTable({
    req(selectedDf())
    head(selectedDf())
  })
  
  output$proceedMessage <- renderText({
    if(!is.null(selectedDf())) {
      return("If these look correct, let's proceed to analysis. Otherwise, edit your selection.")
    }
  })
}

shinyApp(ui = ui, server = server)

