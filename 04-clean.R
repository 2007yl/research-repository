# Import package
library(readxl)
library(dplyr)
library(rio)
# Read the data 
presurvey <- read_excel("cleandata/DSN Pre-Survey_numeric.xlsx", sheet = "clean_yl", skip = 1)  
postsurvey <-read_excel("cleandata/DSN Post-Survey_numeric.xlsx", sheet = "clean_yl",skip = 1)
#data structure
str(presurvey)
str(postsurvey)
# Replace empty cells with NA in presurvey dataset with base R
presurvey[] <- lapply(presurvey, function(x) {
  if (is.numeric(x) || is.integer(x)) {
    x[x == ""] <- NA
  } else if (is.character(x)) {
    x[x == ""] <- NA_character_
  } else if (is.factor(x)) {
    levels(x)[levels(x) == ""] <- NA
  }
  return(x)
})

# Replace empty cells with NA in postsurvey dataset with base R
postsurvey[] <- lapply(postsurvey, function(x) {
  if (is.numeric(x) || is.integer(x)) {
    x[x == ""] <- NA
  } else if (is.character(x)) {
    x[x == ""] <- NA_character_
  } else if (is.factor(x)) {
    levels(x)[levels(x) == ""] <- NA
  }
  return(x)
})
#check missing
check_na <- function(df) {
  # Calculate total number of NA values in the data frame
  total_na <- sum(is.na(df))
  
  # Calculate the number of NA values for each column
  na_per_column <- colSums(is.na(df))
  
  # Create a summary list
  na_summary <- list(
    total_na = total_na,
    na_per_column = na_per_column
  )
  
  return(na_summary)
}
check_na(presurvey)
check_na(postsurvey)


# Perform the matching
  matched_data <- merge(
    x = presurvey,
    y = postsurvey,
    by = "id",
    suffixes = c("_pre", "_post")
  )

library(rio)
export(matched_data, "matched_data.rds")
export(matched_data, "matched_data.csv")
  
#read rds
matched_data <- readRDS("matched_data.rds")

#07012024  add n=6 new survey results
ob_presurvey <- read_excel("cleandata/OB DSN Pre-Survey_062824.xlsx", skip = 1)  
ob_postsurvey <-read_excel("cleandata/OB DSN Post-Survey_062824.xlsx", skip = 1)
# Replace empty cells with NA in presurvey dataset with base R
ob_presurvey[] <- lapply(ob_presurvey, function(x) {
  if (is.numeric(x) || is.integer(x)) {
    x[x == ""] <- NA
  } else if (is.character(x)) {
    x[x == ""] <- NA_character_
  } else if (is.factor(x)) {
    levels(x)[levels(x) == ""] <- NA
  }
  return(x)
})

# Replace empty cells with NA in postsurvey dataset with base R
ob_postsurvey[] <- lapply(ob_postsurvey, function(x) {
  if (is.numeric(x) || is.integer(x)) {
    x[x == ""] <- NA
  } else if (is.character(x)) {
    x[x == ""] <- NA_character_
  } else if (is.factor(x)) {
    levels(x)[levels(x) == ""] <- NA
  }
  return(x)
})
# Perform the matching
matched_obdata <- merge(
  x = ob_presurvey,
  y = ob_postsurvey,
  by = "id",
  suffixes = c("_pre", "_post")
)
# Load dplyr for binding rows and manipulating data
library(dplyr)

# Ensure that both datasets have the same columns

# Find columns that are only in matched_data
extra_cols_in_matched_data <- setdiff(names(matched_data), names(matched_obdata))

# Add these columns to matched_obdata with NA values
matched_obdata[extra_cols_in_matched_data] <- NA

# Now both datasets have the same columns, but the ordering might be different. Make sure the order is the same.
matched_obdata <- matched_obdata[names(matched_data)]

# Convert "What is your age?_pre" in matched_obdata from character to numeric
matched_obdata$`What is your age?_pre` <- as.numeric(matched_obdata$`What is your age?_pre`)

# Bind the rows together
complete_data <- bind_rows(matched_data, matched_obdata)

# Check if the new data frame has 25 observations as expected
print(nrow(complete_data))   #n=25
export(complete_data, "cleandata/complete_data.rds")
export(complete_data, "cleandata/complete_data.csv")
#07/09/24   
#manually change pgy level of 6 new survey entries from "1" to "0" (class 2028).
