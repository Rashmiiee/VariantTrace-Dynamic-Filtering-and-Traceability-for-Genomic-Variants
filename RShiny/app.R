library(shiny)     # Shiny web app
library(DT)        # for data tables

# user interface just shows the table
ui <- fluidPage(
  fluidRow(
    column(12, div(dataTableOutput("dataTable")))
  )
)

# server is where all calculations are done, tables are pre-rendered
server <- function(input, output, session) {
  
  # load reactive CSV file
  myCSV <- reactiveFileReader(100, session, 'baseball.csv', read.csv)
  
  #-----------------------------------------------------------------------------
  #  render data table
  #-----------------------------------------------------------------------------
  
  output$dataTable <- renderDT(
    myCSV(), # reactive data
    class = "display nowrap compact", # style
    filter = "top", # location of column filters
    
    options = list(  # options
      scrollX = TRUE # allow user to scroll wide tables horizontally
    )
  )
  
  #-----------------------------------------------------------------------------
  #  observer to log filters applied
  #-----------------------------------------------------------------------------
  
  observe({
    filters <- input$dataTable_columns_search
    if (!is.null(filters)) {
      log_entry <- "Filters applied:\n"
      for (i in seq_along(filters)) {
        if (filters[[i]] != "") {
          log_entry <- paste(log_entry, paste("Column", i, ":", filters[[i]], "\n"), sep = "")
        }
      }
      
      # Print to console
      cat(log_entry)
      
      # Save to text file
      write(log_entry, file = "filter_log.txt", append = TRUE)
    }
  })
}

# run the app
shinyApp(ui, server)


