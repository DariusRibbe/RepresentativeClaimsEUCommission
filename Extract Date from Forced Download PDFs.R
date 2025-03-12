# Load necessary libraries
library(pdftools)
library(dplyr)
library(stringr)

# Define the folder containing the PDFs
pdf_folder <- "C:/Users/User/Downloads/downloaded_pdfs"

# List all PDF files in the folder
pdf_files <- list.files(pdf_folder, pattern = "\\.pdf$", full.names = TRUE)

# Initialize an empty dataframe
df <- data.frame(
  pdf_identifier = character(),
  full_text = character(),
  bold_text = character(),
  date = character(),
  remaining_text = character(),
  stringsAsFactors = FALSE
)

# Function to extract bold text (assuming it's at the start of the text)
extract_bold_text <- function(text) {
  # Assuming bold text is the first non-newline text before the first paragraph break
  match <- str_match(text, "^[^\\n]+")
  if (!is.na(match[1])) {
    return(trimws(match[1]))
  } else {
    return(NA)
  }
}

# Function to extract a date
extract_date <- function(text) {
  # Match dates in formats like "10 December 2024"
  match <- str_match(text, "\\b\\d{1,2}\\s\\w+\\s\\d{4}\\b")
  if (!is.na(match[1])) {
    return(trimws(match[1]))
  } else {
    return(NA)
  }
}

# Process each PDF file
for (pdf_file in pdf_files) {
  pdf_identifier <- gsub("\\.pdf$", "", basename(pdf_file))
  
  # Read the PDF text
  pdf_text <- tryCatch({
    pdf_text(pdf_file) %>% paste(collapse = " ")  # Combine all pages into one string
  }, error = function(e) {
    warning(paste("Failed to read PDF:", pdf_file))
    return(NA)
  })
  
  if (!is.na(pdf_text)) {
    # Extract components
    bold_text <- extract_bold_text(pdf_text)
    date <- extract_date(pdf_text)
    remaining_text <- gsub(paste0(bold_text, "|", date), "", pdf_text, ignore.case = TRUE) %>% trimws()
    
    # Add to the dataframe
    df <- rbind(df, data.frame(
      pdf_identifier = pdf_identifier,
      full_text = pdf_text,
      bold_text = bold_text,
      date = date,
      remaining_text = remaining_text,
      stringsAsFactors = FALSE
    ))
  }
}

# Display the dataframe
print(df)

# Optionally save the dataframe to a CSV file
output_file <- file.path(dirname(pdf_folder), "pdf_text_data2.csv")
write.csv(df, file = output_file, row.names = FALSE)
cat("Dataframe saved to", output_file, "\n")
