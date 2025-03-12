library(dplyr)
library(writexl)
library(stringr)
library(tidyverse)
library(rvest)
library(cld2)
library(quanteda)
library(readxl)
library(tidyr)
library(dplyr)
library(stringi)
library(stringdist)
# Filter rows with NA in speaker.name
filtered_data <- speeches2020c[is.na(speeches2020c$speaker.name), ]

# Truncate all character columns to 32,767 characters
max_length <- 32767
filtered_data <- as.data.frame(lapply(filtered_data, function(col) {
  if (is.character(col)) substr(col, 1, max_length) else col
}))

# Export to Excel
write_xlsx(filtered_data, "filtered_speeches.xlsx")
speeches2020c$language <- detect_language(speeches2020c$rawtext, lang_code = F)
table(speeches2020c$language)

speeches2020c$length <- str_count(speeches2020c$rawtext, " ") + 1
hist(speeches2020c$length)

speeches2020c$date2 <- gsub("\\.", "", speeches2020c$date)
speeches2020c$date2 <- str_extract(speeches2020c$date2, "\\d{1,2} \\w+ \\d{4}")




month_translation <- list(
  # Bulgarian
  "януари" = "January", "февруари" = "February", "март" = "March", "април" = "April", "май" = "May", "юни" = "June", "юли" = "July", "август" = "August",
  "септември" = "September", "октомври" = "October", "ноември" = "November", "декември" = "December",
  
  # Croatian
  "siječanj" = "January", "veljača" = "February", "ožujak" = "March", "travanj" = "April", "svibanj" = "May", "lipanj" = "June", "srpanj" = "July", "kolovoz" = "August",
  "rujan" = "September", "listopad" = "October", "studeni" = "November", "prosinac" = "December",
  
  # Czech
  "leden" = "January", "únor" = "February", "březen" = "March", "duben" = "April", "květen" = "May", "červen" = "June", "červenec" = "July", "srpen" = "August",
  "září" = "September", "říjen" = "October", "listopad" = "November", "prosinec" = "December",
  
  # Danish
  "januar" = "January", "februar" = "February", "marts" = "March", "april" = "April", "maj" = "May", "juni" = "June", "juli" = "July", "august" = "August",
  "september" = "September", "oktober" = "October", "november" = "November", "december" = "December",
  
  # Dutch
  "januari" = "January", "februari" = "February", "maart" = "March", "april" = "April", "mei" = "May", "juni" = "June", "juli" = "July", "augustus" = "August",
  "september" = "September", "oktober" = "October", "november" = "November", "december" = "December",
  
  # English (already correct)
  
  # Finnish
  "tammikuu" = "January", "helmikuu" = "February", "maaliskuu" = "March", "huhtikuu" = "April", "toukokuu" = "May", "kesäkuu" = "June", "heinäkuu" = "July", "elokuu" = "August",
  "syyskuu" = "September", "lokakuu" = "October", "marraskuu" = "November", "joulukuu" = "December",
  
  # French
  "janvier" = "January", "février" = "February", "mars" = "March", "avril" = "April", "mai" = "May", "juin" = "June", "juillet" = "July", "août" = "August",
  "septembre" = "September", "octobre" = "October", "novembre" = "November", "décembre" = "December",
  
  # German
  "Januar" = "January", "Februar" = "February", "März" = "March", "April" = "April", "Mai" = "May", "Juni" = "June", "Juli" = "July", "August" = "August",
  "September" = "September", "Oktober" = "October", "November" = "November", "Dezember" = "December",
  
  # Greek
  "Ιανουάριος" = "January", "Φεβρουάριος" = "February", "Μάρτιος" = "March", "Απρίλιος" = "April", "Μάιος" = "May", "Ιούνιος" = "June", "Ιούλιος" = "July", "Αύγουστος" = "August",
  "Σεπτέμβριος" = "September", "Οκτώβριος" = "October", "Νοέμβριος" = "November", "Δεκέμβριος" = "December",
  
  # Hungarian
  "január" = "January", "február" = "February", "március" = "March", "április" = "April", "május" = "May", "június" = "June", "július" = "July", "augusztus" = "August",
  "szeptember" = "September", "október" = "October", "november" = "November", "december" = "December",
  
  # Italian
  "gennaio" = "January", "febbraio" = "February", "marzo" = "March", "aprile" = "April", "maggio" = "May", "giugno" = "June", "luglio" = "July", "agosto" = "August",
  "settembre" = "September", "ottobre" = "October", "novembre" = "November", "dicembre" = "December",
  
  # Latvian
  "janvāris" = "January", "februāris" = "February", "marts" = "March", "aprīlis" = "April", "maijs" = "May", "jūnijs" = "June", "jūlijs" = "July", "augusts" = "August",
  "septembris" = "September", "oktobris" = "October", "novembris" = "November", "decembris" = "December",
  
  # Lithuanian
  "sausis" = "January", "vasaris" = "February", "kovas" = "March", "balandis" = "April", "gegužė" = "May", "birželis" = "June", "liepa" = "July", "rugpjūtis" = "August",
  "rugsėjis" = "September", "spalis" = "October", "lapkritis" = "November", "gruodis" = "December"
)


for (month in names(month_translation)) {
  speeches2020c$date2 <- str_replace(speeches2020c$date2, paste0("\\b", month, "\\b"), month_translation[[month]])
}

speeches2020c$date2 <- as.character(speeches2020c$date2)

speeches2020c$date2 <- sub("January", ".1.", speeches2020c$date2)
speeches2020c$date2 <- sub("February", ".2.", speeches2020c$date2)
speeches2020c$date2 <- sub("March", ".3.", speeches2020c$date2)
speeches2020c$date2 <- sub("April", ".4.", speeches2020c$date2)
speeches2020c$date2 <- sub("May", ".5.", speeches2020c$date2)
speeches2020c$date2 <- sub("June", ".6.", speeches2020c$date2)
speeches2020c$date2 <- sub("July", ".7.", speeches2020c$date2)
speeches2020c$date2 <- sub("August", ".8.", speeches2020c$date2)
speeches2020c$date2 <- sub("September", ".9.", speeches2020c$date2)
speeches2020c$date2 <- sub("October", ".10.", speeches2020c$date2)
speeches2020c$date2 <- sub("November", ".11.", speeches2020c$date2)
speeches2020c$date2 <- sub("December", ".12.", speeches2020c$date2)

speeches2020c$date2 <- gsub("\\s", "", speeches2020c$date2)


# Convert to Date format correctly
speeches2020c$date2 <- as.Date(speeches2020c$date2, format = "%d.%m.%Y")



colnames(speeches2020c)[colnames(speeches2020c) == "pdf_identifier"] <- "file"
colnames(speeches2020c)[colnames(speeches2020c) == "boldTrue"] <- "context"

speeches2020c$year <- format(speeches2020c$date2, "%Y")

# Create 'month' variable (format YYYY-MM)
speeches2020c$month <- format(speeches2020c$date2, "%Y-%m")

speeches2020c$date <- speeches2020c$date2
speeches2020c$date2 <- NULL
speeches2020c$title6 <- NULL
speeches2020c$title6_normalized<- NULL
speeches2020c$title7<- NULL
speeches2020c$title7_normalized<- NULL
speeches2020c$boldFalse<- NULL
speeches2020c$speaker.function <- ""
speeches2020c$date2 <- ""
speeches2020c$description <- ""
speeches2020c$pdfonly <- "FALSE"
speeches2020c$text <- speeches2020c$rawtext
speeches2020c$pdfonly <- as.logical(speeches2020c$pdfonly)
speeches2020c$description <- as.logical(speeches2020c$description)# Get all unique column names from both datasets

speeches2020c <- speeches2020c[, order(names(speeches2020c))]

# Reorder columns alphabetically in speeches2024combined
speeches2024combined <- speeches2024combined[, order(names(speeches2024combined))]

speeches2020c$title2 <- speeches2020c$context.1
speeches2020c$context.1 <- NULL

# Remove everything after the last number in speeches2020c$file
speeches2020c$file <- sub("([0-9]+)[^0-9]*$", "\\1", speeches2020c$file)

# Find rows in speeches2020c that do NOT have a match in speeches2024combined$file
speeches2020c_unique <- speeches2020c[!(speeches2020c$file %in% speeches2024combined$file), ]

# Merge both datasets
mergeddf <- rbind(speeches2024combined, speeches2020c_unique)
mergeddf <- mergeddf[!(mergeddf$month %in% c("2024-12", "2025-01")), ]

# Add row names as a new column called "row_id"
mergeddf$row_id <- rownames(mergeddf)

# Export mergeddf as a CSV file including the row names
write.csv(mergeddf, "mergeddf.csv", row.names = FALSE)

# Confirm the file was saved successfully
print("CSV file 'mergeddf.csv' has been saved successfully with row names as 'row_id'!")


# Print summary to verify
print(dim(mergeddf))  # Check number of rows and columns
print(head(mergeddf$file))  # Preview file column

# Count the number of speeches per year
speeches_by_year <- mergeddf %>%
  group_by(year) %>%
  summarise(num_speeches = n()) %>%
  arrange(year)

library(ggplot2)
ggplot(speeches_by_year, aes(x = year, y = num_speeches)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Number of Speeches per Year",
    x = "Year",
    y = "Number of Speeches"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

library(ggplot2)
library(extrafont)

# Ensure Times New Roman is available
 #font_import(pattern = "Times New Roman", prompt = FALSE) # Uncomment if needed
 loadfonts(device = "win")  # Use 'win' for Windows, 'pdf' for PDF output

# Create the plot
ggplot(speeches_by_year, aes(x = year, y = num_speeches)) +
  geom_col(fill = "steelblue") +
  theme_minimal(base_family = "Times New Roman") +  # Use Times New Roman
  labs(
    title = "Number of Speeches per Year",
    x = "Year",
    y = "Number of Speeches"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(family = "Times New Roman"),
    axis.title = element_text(family = "Times New Roman")
  ) +
  scale_x_continuous(
    breaks = seq(min(1985), max(2024), by = 5)  # Show ticks every 5 years
  )

# Save the plot as a vector graphic (SVG or PDF)
ggsave("speeches_per_year.svg", plot = p, width = 10, height = 6)
# Alternatively, for PDF:
# ggsave("speeches_per_year.pdf", plot = p, width = 10, height = 6, device = cairo_pdf)
