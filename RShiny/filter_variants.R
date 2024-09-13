library(shiny)
library(DT)
library(shinythemes)

# Increase maximum upload size
options(shiny.maxRequestSize = 400*1024^2) # 400MB

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("cerulean"), # Apply a theme
  titlePanel("Filter Variants"),
  
  # Place the upload portion above the table
  fluidRow(
    column(12,
           h3("Upload CSV or TSV"),
           fileInput("file1", "Choose CSV or TSV File",
                     accept = c(
                       "text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv",
                       "text/tab-separated-values,text/plain",
                       ".tsv")
           ),
           tags$hr()
    )
  ),
  
  # Main panel for displaying the table and the current filters
  fluidRow(
    column(12,
           dataTableOutput("dataTable")
    )
  ),
  
  fluidRow(
    column(12,
           h3("Current Filters"),
           verbatimTextOutput("currentFilters")
    )
  ),
  
  # Download button for filtered CSV
  fluidRow(
    column(12,
           downloadButton("downloadFiltered", "Download Filtered CSV")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to store the log file name
  logFile <- reactiveVal(NULL)
  
  # Reactive expression to read the CSV or TSV file
  data <- reactive({
    req(input$file1)
    
    ext <- tools::file_ext(input$file1$name)
    
    # Create a unique log file based on the file name
    fileName <- gsub(paste0(".", ext), "", input$file1$name) # Remove file extension
    logFileName <- paste0("filter_log_", fileName, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
    logFile(logFileName)
    
    # Read the file based on its extension
    if (ext == "csv") {
      df <- read.csv(input$file1$datapath)
    } else if (ext == "tsv") {
      df <- read.delim(input$file1$datapath)
    } else {
      stop("Invalid file type. Please upload a CSV or TSV file.")
    }
    
    df
  })
  
  # Render the DataTable
  output$dataTable <- renderDataTable({
    req(data())
    datatable(data(),
              class = "display nowrap compact",
              filter = "top",
              options = list(
                scrollX = TRUE,
                autoWidth = TRUE,
                pageLength = 10,
                lengthMenu = c(10, 25, 50, 100),
                dom = 'lftipr', # Remove the global search bar
                searchHighlight = TRUE, # Highlight search terms
                columnDefs = list(
                  list(width = '70px', targets = '_all') # Set column width
                )
              )
    )
  }, server = TRUE)
  
  # Function to write log entries to a file with date and timestamp
  write_log <- function(log_entry) {
    log_file <- logFile()
    if (!is.null(log_file)) {
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      log_entry <- paste(timestamp, log_entry, sep = " - ")
      write(log_entry, file = log_file, append = TRUE)
    }
  }
  
  # Observe filter changes in the DataTable
  observe({
    input$dataTable_search_columns
    isolate({
      filters <- input$dataTable_search_columns
      column_names <- colnames(data()) # Get column names
      
      log_entry <- "Filters applied:\n"
      current_filters <- "Current Filters:\n"
      for (i in seq_along(filters)) {
        if (filters[[i]] != "") {
          column_name <- ifelse(i <= length(column_names), column_names[i], paste("Unknown Column", i))
          log_entry <- paste(log_entry, paste("Column", column_name, ":", filters[[i]], "\n"), sep = "")
          current_filters <- paste(current_filters, paste("Column", column_name, ":", filters[[i]], "\n"), sep = "")
        }
      }
      
      if (log_entry != "Filters applied:\n") {
        # Write the log to a file
        write_log(log_entry)
      }
      
      # Update the current filters display
      output$currentFilters <- renderText({
        if (nchar(current_filters) == 0) "No filters applied." else current_filters
      })
    })
  })
  
  # Download handler for filtered CSV
  output$downloadFiltered <- downloadHandler(
    filename = function() {
      paste("filtered_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Get the filtered rows
      filtered_data <- data()[input$dataTable_rows_all, ]
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
