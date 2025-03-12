df <- mergeddf[is.na(mergeddf$date), ]

# Create a vector of the new date values as character strings
new_dates <- c("18.08.2022", "29.08.2023", "30.09.2023",
               "27.08.2024", "29.08.2024", "26.01.2024",
               "07.12.2020", "07.12.2020", "14.12.2020")

# Convert the character dates to Date objects using the format "%d.%m.%Y"
df$date <- as.Date(new_dates, format = "%d.%m.%Y")

na_indices <- is.na(mergeddf$date)

mergeddf <- safe

mergeddf$date2 <- mergeddf$date
mergeddf$date <- as.Date(mergeddf$date, format = "%Y-%m-%d")



mergeddf$date[na_indices] <- df$date[match(mergeddf$file[na_indices], df$file)]

df <- mergeddf[is.na(mergeddf$date), ]

# Convert to numeric (in case they are characters)
df$date2 <- as.numeric(as.character(df$date2))
# Convert numeric epoch (days) to Date, then format as "YYYY-mm-dd"
df$date2 <- format(as.Date(df$date2, origin = "1970-01-01"), "%Y-%m-%d")

df$date <- as.Date(df$date2, format = "%Y-%m-%d")

# Identify rows in mergeddf where date is NA
na_indices <- is.na(mergeddf$date)

# Find matching rows in df based on the file column
matches <- match(mergeddf$file[na_indices], df$file)

# Replace mergeddf$date with df$date and mergeddf$date2 with df$date2 for these matches
mergeddf$date[na_indices]  <- df$date[matches]
mergeddf$date2[na_indices] <- df$date2[matches]

write.csv(mergeddf, "C:/Users/User/Documents/mergeddf_withdates.csv")

unique_speakers <- unique(mergeddf$speaker.name)
print(unique_speakers)

df <- mergeddf[grepl("[;,]", mergeddf$speaker.name), ]
if (!require("writexl")) {
  install.packages("writexl")
  library(writexl)
}

# Export df as an Excel file
write_xlsx(df, "dfdoublespeakers.xlsx")
file.path(getwd(), "df.xlsx")

safe1 <- mergeddf

library(readxl)
dfnames <- read_excel("dfnames.xlsx")
View(dfnames)

# Ensure the file columns are characters
mergeddf$file <- as.character(mergeddf$file)
dfnames$file <- as.character(dfnames$file)

# Get the matching indices of mergeddf$file in dfnames$file
matching_indices <- match(mergeddf$file, dfnames$file)

# Replace speaker.name in mergeddf for those rows with a match
mergeddf$speaker.name[!is.na(matching_indices)] <- dfnames$speaker.name[matching_indices[!is.na(matching_indices)]]

unique_speakers <- unique(mergeddf$speaker.name)
print(unique_speakers)

comdf <- read_excel("CommissionersDatatryoutsex.xlsx")

# Normalize full names:
comdf$namenorm <- tolower(
  trimws(
    gsub("-", "", iconv(comdf$Name, from = "UTF-8", to = "ASCII//TRANSLIT"))
  )
)

# Normalize only the last word:
comdf$lastnamenorm <- sapply(comdf$Name, function(x) {
  # Remove accents and dashes for the full string first
  x_clean <- tolower(trimws(gsub("-", "", iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT"))))
  # Split the cleaned string into words and return the last one
  words <- strsplit(x_clean, "\\s+")[[1]]
  tail(words, 1)
})

#mergeddf <- safe3

# Ensure date columns are Date objects
mergeddf$date <- as.Date(mergeddf$date)
comdf$Start <- as.Date(comdf$Start)
comdf$End   <- as.Date(comdf$End)

# Ensure name columns are characters
mergeddf$speaker.name <- as.character(mergeddf$speaker.name)
comdf$Name      <- as.character(comdf$Name)
comdf$namenorm  <- as.character(comdf$namenorm)
comdf$lastnamenorm <- as.character(comdf$lastnamenorm)

# For each row in mergeddf, check if a matching commissioner is found in comdf
mergeddf$speakermatch <- sapply(1:nrow(mergeddf), function(i) {
  mdate <- mergeddf$date[i]
  mname <- mergeddf$speaker.name[i]
  
  # Subset comdf by date range
  candidates <- comdf[comdf$Start <= mdate & comdf$End >= mdate, ]
  
  # If there are no candidates, return FALSE immediately
  if (nrow(candidates) == 0) return(FALSE)
  
  # Check for a match in the following order:
  # 1. Exact match in Name
  if (any(candidates$Name == mname, na.rm = TRUE)) return(TRUE)
  
  # 2. Exact match in namenorm
  if (any(candidates$namenorm == mname, na.rm = TRUE)) return(TRUE)
  
  # 3. Exact match in lastnamenorm
  if (any(candidates$lastnamenorm == mname, na.rm = TRUE)) return(TRUE)
  
  # If no match was found, return FALSE
  return(FALSE)
})

sum(mergeddf$speakermatch == FALSE)


df <- mergeddf[mergeddf$speakermatch == FALSE, ]



# 1. Create speaker.name2: normalize speaker.name and keep only the last word
df$speaker.name2 <- sapply(df$speaker.name, function(x) {
  # Remove accents and hyphens, convert to lowercase, trim whitespace
  x_clean <- tolower(trimws(gsub("-", "", iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT"))))
  # Split into words and take the last one
  words <- strsplit(x_clean, "\\s+")[[1]]
  tail(words, 1)
})

# 2. Second round of matching using speaker.name2 and comdf$lastnamenorm
# Make sure date fields are Date objects
df$date <- as.Date(df$date)
comdf$Start <- as.Date(comdf$Start)
comdf$End   <- as.Date(comdf$End)

df$secondmatch <- sapply(1:nrow(df), function(i) {
  # Get the current row's date and normalized last name
  mdate <- df$date[i]
  mlast <- df$speaker.name2[i]
  
  # Subset comdf for rows where the date falls in the valid range
  candidates <- comdf[comdf$Start <= mdate & comdf$End >= mdate, ]
  
  # If no candidate exists, no match
  if (nrow(candidates) == 0) return(FALSE)
  
  # Return TRUE if any candidate's lastnamenorm matches the current speaker's last name
  any(candidates$lastnamenorm == mlast, na.rm = TRUE)
})

# Optional: Check the results
table(df$secondmatch)

sum(is.na(df$speaker.name) & df$secondmatch == FALSE)

df2 <- df[!is.na(df$speaker.name) & df$secondmatch == FALSE, ]

write_xlsx(df2, "namefix.xlsx")

df2$speaker.name2 <- gsub("kyriakides", "kyriakidou", df2$speaker.name2)
df2$speaker.name2 <- gsub("reevy", "mccreevy", df2$speaker.name2)
df2$speaker.name2 <- gsub("otocnik", "potocnik", df2$speaker.name2)
df2$speaker.name2 <- gsub("cretu", "cre?u", df2$speaker.name2)
df2$speaker.name2 <- gsub("ciolos", "ciolo?", df2$speaker.name2)
df2$speaker.name2 <- gsub("sharry", "macsharry", df2$speaker.name2)

df2$secondmatch <- sapply(1:nrow(df2), function(i) {
  # Get the current row's date and normalized last name
  mdate <- df2$date[i]
  mlast <- df2$speaker.name2[i]
  
  # Subset comdf for rows where the date falls in the valid range
  candidates <- comdf[comdf$Start <= mdate & comdf$End >= mdate, ]
  
  # If no candidate exists, no match
  if (nrow(candidates) == 0) return(FALSE)
  
  # Return TRUE if any candidate's lastnamenorm matches the current speaker's last name
  any(candidates$lastnamenorm == mlast, na.rm = TRUE)
})

sum(df2$secondmatch == FALSE)

df3 <- df2[!is.na(df2$speaker.name) & df2$secondmatch == FALSE, ]

write_xlsx(df2, "namefix.xlsx")

# Ensure dates are in Date format and name columns are characters
df2$date <- as.Date(df2$date)
comdf$Start <- as.Date(comdf$Start)
comdf$End   <- as.Date(comdf$End)

df2$speaker.name <- as.character(df2$speaker.name)
df2$speaker.name2 <- as.character(df2$speaker.name2)
comdf$lastnamenorm <- as.character(comdf$lastnamenorm)
comdf$Name <- as.character(comdf$Name)

# Loop over rows of df2 and replace speaker.name if a match is found in comdf
for (i in seq_len(nrow(df2))) {
  mdate <- df2$date[i]
  mlast <- df2$speaker.name2[i]
  
  # Subset comdf rows where the date falls within the range
  candidates <- comdf[comdf$Start <= mdate & comdf$End >= mdate, ]
  
  # Look for a match on the normalized last name
  match_index <- which(candidates$lastnamenorm == mlast)
  
  if (length(match_index) > 0) {
    # If a match is found, update df2$speaker.name with the comdf$Name of the first match
    df2$speaker.name[i] <- candidates$Name[match_index[1]]
  }
}

df2$speaker.name[df2$secondmatch == FALSE] <- NA



df$file <- as.character(df$file)
df2$file <- as.character(df2$file)

# Find matching indices
matches <- match(df$file, df2$file)

# Replace speaker.name in df with the corresponding value from df2 where a match is found
df$speaker.name[!is.na(matches)] <- df2$speaker.name[matches[!is.na(matches)]]

# Ensure df$date, comdf$Start and comdf$End are Date objects
df$date <- as.Date(df$date)
comdf$Start <- as.Date(comdf$Start)
comdf$End   <- as.Date(comdf$End)

# Ensure name columns are characters
df$speaker.name <- as.character(df$speaker.name)
comdf$Name <- as.character(comdf$Name)
comdf$lastnamenorm <- as.character(comdf$lastnamenorm)

# Create a normalized last-name column for df, similar to df2$speaker.name2
df$speaker.name2 <- sapply(df$speaker.name, function(x) {
  if (is.na(x)) return(NA)
  # Remove accents and hyphens, convert to lowercase, trim whitespace
  x_clean <- tolower(trimws(gsub("-", "", iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT"))))
  # Split the cleaned string into words and take the last word
  words <- strsplit(x_clean, "\\s+")[[1]]
  tail(words, 1)
})

# Loop over rows in df to replace speaker.name if a match is found in comdf
for (i in seq_len(nrow(df))) {
  # Only process rows with non-missing speaker.name
  if (!is.na(df$speaker.name[i])) {
    mdate <- df$date[i]
    mlast <- df$speaker.name2[i]
    
    # Subset comdf for rows where the date is within the valid range
    candidates <- comdf[comdf$Start <= mdate & comdf$End >= mdate, ]
    
    # Look for a match on the normalized last name
    match_index <- which(candidates$lastnamenorm == mlast)
    
    if (length(match_index) > 0) {
      # If a match is found, update df$speaker.name with the comdf$Name of the first match
      df$speaker.name[i] <- candidates$Name[match_index[1]]
    }
  }
}

# Ensure dates are in Date format and name columns are characters
mergeddf$date <- as.Date(mergeddf$date)
comdf$Start <- as.Date(comdf$Start)
comdf$End   <- as.Date(comdf$End)

mergeddf$speaker.name <- as.character(mergeddf$speaker.name)
comdf$Name <- as.character(comdf$Name)
comdf$namenorm <- as.character(comdf$namenorm)
comdf$lastnamenorm <- as.character(comdf$lastnamenorm)

# Process only rows where a match was already found (speakermatch == TRUE)
indices <- which(mergeddf$speakermatch == TRUE)

for (i in indices) {
  mdate <- mergeddf$date[i]
  mname <- mergeddf$speaker.name[i]
  
  # Subset comdf rows for which the mergeddf date is within the candidate's date range
  candidates <- comdf[comdf$Start <= mdate & comdf$End >= mdate, ]
  
  # If no candidates, skip this row (should not occur if speakermatch is TRUE)
  if (nrow(candidates) == 0) next
  
  # Try to match in the specified order:
  # 1. Check for an exact match in Name
  if (any(candidates$Name == mname, na.rm = TRUE)) {
    mergeddf$speaker.name[i] <- candidates$Name[which(candidates$Name == mname)[1]]
  } else if (any(candidates$namenorm == mname, na.rm = TRUE)) {
    # 2. Otherwise, check for a match in namenorm
    mergeddf$speaker.name[i] <- candidates$Name[which(candidates$namenorm == mname)[1]]
  } else if (any(candidates$lastnamenorm == mname, na.rm = TRUE)) {
    # 3. Otherwise, check for a match in lastnamenorm
    mergeddf$speaker.name[i] <- candidates$Name[which(candidates$lastnamenorm == mname)[1]]
  }
}

# Ensure the file columns are characters
mergeddf$file <- as.character(mergeddf$file)
df$file <- as.character(df$file)

# Identify rows in mergeddf with no match (speakermatch == FALSE)
no_match_indices <- which(mergeddf$speakermatch == FALSE)

# For these rows, find matching indices in df based on the file variable
matches <- match(mergeddf$file[no_match_indices], df$file)

# Replace mergeddf$speaker.name with df$speaker.name where a match is found
mergeddf$speaker.name[no_match_indices][!is.na(matches)] <- df$speaker.name[matches[!is.na(matches)]]

sum(is.na(mergeddf$speaker.name))

write.csv(mergeddf, "C:/Users/User/Documents/speechesmissingspeaker.csv")
write.csv(comdf, "C:/Users/User/Documents/speakerinfo.csv")
