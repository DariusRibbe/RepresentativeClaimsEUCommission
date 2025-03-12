###claimR package 1.2###

#' @import library(stringr)
#' @import library(dplyr)
#' @import library(tidytext)
#' @import library(text2vec)
#' @import library(nnet)
#' @import library(e1071) # SVM and Naïve Bayes
#' @import library(shiny)
#' @import library(shinyjs)
#' @import library(reticulate) # For Python integration

# Set Python Environment for Deep Learning Models
set_python_env <- function(python_path) {
  use_python(python_path, required = TRUE)
  print(paste("Using Python from:", python_path))
}


#' Split Text into Sentences
#' @param df A data frame
#' @param text_col The name of the text column in the data frame
#' @param circ A boolean indicating whether to include surrounding sentences
#' @return A data frame with sentence IDs
#' @export
split_sentences <- function(df, text_col, circ = FALSE) {
  df <- df %>%
    mutate(sentences = str_split(.data[[text_col]], "(?<!\\b[A-Z])(?<=[.!?])\\s+")) %>%
    unnest(sentences) %>%
    mutate(id = row_number())
  
  if (circ) {
    df <- df %>%
      mutate(
        prev_sentence = lag(sentences, default = ""),
        next_sentence = lead(sentences, default = "")
      ) %>%
      mutate(context = paste(prev_sentence, sentences, next_sentence))
  }
  
  return(df)
}


#' Identify Claims in Text
#' @param df A data frame
#' @param text_col The name of the text column
#' @param search_word The word to search for
#' @param synon Boolean indicating whether to use synonyms
#' @return Data frame with an "identifier" column
#' @export
claim_id <- function(df, text_col, search_word, synon = FALSE) {
  if (synon) {
    synonyms <- tryCatch(text2vec::synonyms(search_word), error = function(e) character(0))
    search_words <- unique(c(search_word, synonyms))
  } else {
    search_words <- search_word
  }
  
  df <- df %>%
    rowwise() %>%
    mutate(identifier = ifelse(any(str_detect(.data[[text_col]], search_words, ignore.case = TRUE)), search_word, "FALSE"))
  
  return(df)
}


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



#' Classify Representative Claims
#' @param df A data frame containing the text to classify
#' @param text_col The column name containing text
#' @param label_col The column name containing labels for training
#' @param training_df A data frame containing training data
#' @param training_text_col The column name containing text in the training data
#' @param method The classification method to use ("svm", "naive_bayes", "xlnet", "xlm-roberta", "mbert")
#' @return A data frame with classification results
#' @export
classify_representative_claims <- function(df, text_col, label_col, training_df, training_text_col, method) {
  
  # Check if method is an R-based model (SVM/Naïve Bayes)
  if (method %in% c("svm", "naive_bayes")) {
    
    # Ensure training data is properly formatted
    training_data <- training_df %>%
      unnest_tokens(word, !!sym(training_text_col)) %>%
      anti_join(stop_words) %>%
      count(id, word) %>%
      cast_dtm(id, word, n)
    
    training_data$label <- as.factor(training_df[[label_col]])
    
    # Train the chosen model
    if (method == "svm") {
      model <- svm(label ~ ., data = training_data)
    } else if (method == "naive_bayes") {
      model <- naiveBayes(label ~ ., data = training_data)
    }
    
    # Process the test data
    test_data <- df %>%
      unnest_tokens(word, !!sym(text_col)) %>%
      anti_join(stop_words) %>%
      count(id, word) %>%
      cast_dtm(id, word, n)
    
    # Classify the text
    predictions <- predict(model, test_data)
    df$predictions <- predictions
    
    return(df)
    
  } else if (method %in% c("xlnet", "xlm-roberta", "mbert")) {
    
    # Check if Python is available
    py_available <- py_available()
    if (!py_available) stop("Python environment is not set. Call set_python_env() first.")
    
    # Ensure Python script is sourced
    source_python("classify_text.py")
    
    # Call Python-based classifier
    df$probability <- classify_text(df[[text_col]], method)
    
    return(df)
    
  } else {
    stop("Invalid method. Choose 'svm', 'naive_bayes', 'xlnet', 'xlm-roberta', or 'mbert'.")
  }
}



train_text_classifier <- function(df, text_col, label_col, method = "svm") {
  df <- df %>%
    unnest_tokens(word, {{ text_col }}) %>%
    anti_join(stop_words) %>%
    count(id, word) %>%
    cast_dtm(id, word, n)
  
  df$label <- as.factor(df[[label_col]])
  
  if (method == "svm") {
    model <- svm(label ~ ., data = df)
  } else if (method == "naive_bayes") {
    model <- naiveBayes(label ~ ., data = df)
  } else {
    stop("Invalid method. Choose 'svm' or 'naive_bayes'.")
  }
  
  return(model)
}

classify_text <- function(df, text_col, model) {
  df <- df %>%
    unnest_tokens(word, {{ text_col }}) %>%
    anti_join(stop_words) %>%
    count(id, word) %>%
    cast_dtm(id, word, n)
  
  predictions <- predict(model, df)
  df$predictions <- predictions
  
  return(df)
}


classify_with_python <- function(df, text_col, model_choice) {
  py_available <- py_available()
  if (!py_available) stop("Python environment is not set. Call set_python_env() first.")
  
  source_python("classify_text.py")
  
  df$probability <- classify_text(df[[text_col]], model_choice)
  return(df)
}


