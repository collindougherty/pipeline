# app.R
library(shiny)
options(shiny.maxRequestSize = 3000 * 1024^2)  # Increase file size limit

# Load ncdb_recode.R script
source("/Users/collindougherty/Documents/Work/pipeline/backend/dtypes.r")
source("/Users/collindougherty/Documents/Work/pipeline/backend/ncdb_recode.R")

ui <- fluidPage(
  # Center the title using HTML tags and CSS
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
  
  # Center the file upload input
  fluidRow(
    column(4, offset = 4,  # Adjust the width and offset to center
           fileInput("file1", "Choose CSV File",
                     accept = c("text/csv", 
                                "text/comma-separated-values,text/plain", 
                                ".csv"))
    )
  ),
  
  # Place the table output below the upload box
  fluidRow(
    column(12,
           tableOutput("table")  # Output for displaying the table
    )
  )
)

server <- function(input, output) {
    # Render table with processed data
    output$table <- renderTable({
        req(input$file1)
        
        # Initialize the progress bar
        withProgress(message = 'Processing data...', value = 0, {
            # Number of steps in the process
            total_steps <- 4

            for(i in 1:total_steps) {
                # Update the progress bar
                setProgress(value = i/total_steps)

                # Simulating a long process
                Sys.sleep(0.5)  # Half-second delay to simulate processing

                # Actual data processing steps go here
                # For instance, processing a part of the data in each iteration
                # lets put each processing step in here
                if (i == 1) {
                    # Read and process the data
                    df <- read.csv(input$file1$datapath)
                }
                if (i == 2) {
                    recodedDf <- ncdb_recode(df)
                }
                if (i == 3) {
                    dtypes_data <- dtype(recodedDf)
                }
            }

            return(head(dtypes_data))
        })
    })
}





# Run the application 
shinyApp(ui = ui, server = server)
