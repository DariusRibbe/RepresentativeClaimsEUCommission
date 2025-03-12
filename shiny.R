#' ClaimR - Finding Representative Claims in TXT
#'
#' @param df The data frame containing the claims.
#' @param text_col The name of the column containing the text (as a string).
#' @export
#' @import shiny
#' @import shinyjs
#' @examples
#' run_claimr(woman_sentences, "sentences")
run_claimr <- function(df, text_col) {
  library(shiny)
  library(dplyr)
  library(readr)
  
  # Ensure all necessary columns exist
  if (!"class" %in% colnames(df)) {
    df <- df %>% mutate(class = NA)
  }
  if (!"distance" %in% colnames(df)) {
    df <- df %>% mutate(distance = NA)
  }
  
  # Define the UI
  ui <- fluidPage(
    titlePanel("ClaimR - Finding Representative Claims in TXT"),
    
    # Create the display area
    mainPanel(
      div(style="text-align:center;font-size:30px;font-weight:bold;",
          uiOutput("text"), tags$br(),
          uiOutput("class"), tags$br(),
          uiOutput("distance")
      ),
      
      # Navigation Buttons
      fluidRow(
        column(6, align = "center", actionButton("back", "Back", class = "btn-primary")),
        column(6, align = "center", actionButton("forward", "Forward", class = "btn-primary"))
      ),
      
      # Create the Claim, No Claim and ? buttons
      div(style="display:flex;justify-content:space-around;margin-top:30px;",
          actionButton("claim", "Claim", class = "btn-success"),
          actionButton("no_claim", "No Claim", class = "btn-danger"),
          actionButton("unclear", "?", class = "btn-warning")
      ),
      
      # Create the Export button
      div(style="text-align:center;margin-top:50px;",
          actionButton("update", "Update Classifier")
      ),
      
      # Add this in the UI section
      div(style = "text-align:center; margin-top:20px;",
          actionButton("close_app", "Close App", class = "btn-danger")
      )
    ),
    
    # JavaScript for Keyboard Shortcuts
    tags$script(HTML("
      $(document).on('keydown', function(e) {
        if (e.key === 'y' || e.key === 'Y' || e.key === 'z' || e.key === 'Z') {
          $('#claim').trigger('click');
        } else if (e.key === 'n' || e.key === 'N') {
          $('#no_claim').trigger('click');
        } else if (e.key === 'q' || e.key === 'Q') {
          $('#unclear').trigger('click');
        }
      });
    "))
  )
  
  # Define the server
  server <- function(input, output, session) {
    # Use reactiveValues to store the data without constantly modifying the global environment
    data <- reactiveValues(df = df)
    
    # Create a reactive variable for the current row
    current_row <- reactiveVal(1)
    
    # Define the output UI elements
    output$text <- renderUI({
      req(data$df[[text_col]][current_row()])  # Ensure the text exists
      div(data$df[[text_col]][current_row()], style="font-size:30px;font-weight:bold;")
    })
    
    output$class <- renderUI({
      div(data$df$class[current_row()], style="font-size:30px;")
    })
    
    output$distance <- renderUI({
      if ("distance" %in% colnames(data$df)) {
        div(data$df$distance[current_row()], style="font-size:30px;")
      }
    })
    
    # Update the current row when the Back or Forward button is clicked
    observeEvent(input$back, {
      current_row(max(1, current_row() - 1))
    })
    
    observeEvent(input$forward, {
      current_row(min(nrow(data$df), current_row() + 1))
    })
    
    # Update the class variable in memory without writing to file immediately
    classify <- function(label) {
      data$df$class[current_row()] <- label
      if (current_row() < nrow(data$df)) {
        current_row(current_row() + 1)  # Move to next sentence automatically
      }
    }
    
    observeEvent(input$claim, {
      classify(TRUE)
    })
    
    observeEvent(input$no_claim, {
      classify(FALSE)
    })
    
    observeEvent(input$unclear, {
      classify("?")
    })
    
    # Export classifier when the update button is clicked
    observeEvent(input$update, {
      write_csv(data$df, "classified_claims.csv")
    })
    
    # Close the app
    observeEvent(input$close_app, {
      write_csv(data$df, "classified_claims.csv")  # Save once at the end
      assign("df", data$df, envir = .GlobalEnv)  # Update global environment
      stopApp()
    })
  }
  
  # Run the app
  shinyApp(ui, server)
}

