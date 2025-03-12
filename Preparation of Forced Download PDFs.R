# Load necessary libraries
library(pdftools)
library(dplyr)

# Define the folder containing the PDFs
pdf_folder <- "C:/Users/User/Downloads/downloaded_pdfs"

# List all PDF files in the folder
pdf_files <- list.files(pdf_folder, pattern = "\\.pdf$", full.names = TRUE)

# Initialize an empty dataframe
df <- data.frame(
  pdf_identifier = character(),
  pdf_text = character(),
  stringsAsFactors = FALSE
)

# Loop through each PDF file
for (pdf_file in pdf_files) {
  # Extract the PDF identifier from the file name (remove folder path and .pdf extension)
  pdf_identifier <- gsub("\\.pdf$", "", basename(pdf_file))
  
  # Extract text from the PDF
  pdf_text <- tryCatch({
    pdf_text(pdf_file) %>% paste(collapse = " ")  # Combine text from all pages into a single string
  }, error = function(e) {
    warning(paste("Failed to read PDF:", pdf_file))
    NA  # Return NA if text extraction fails
  })
  
  # Append to the dataframe
  df <- rbind(df, data.frame(pdf_identifier = pdf_identifier, pdf_text = pdf_text, stringsAsFactors = FALSE))
}

# Display the dataframe
print(df)

# Optionally save the dataframe to a CSV file
write.csv(df, file = "C:/Users/User/Downloads/pdf_text_data_speeches_update.csv", row.names = FALSE)

df1 <- df
df1$text <- sub(".*?\\b(President |Commissioner )\\b", "", df1$pdf_text)

df1$speaker <- sub("\\b(at|on)\\b.*", "", df1$text)
