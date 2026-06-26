library(tidyverse)
library(gtsummary)
# add Pre: to pre-workshop questions
complete_data <- complete_data %>%
  rename(
    `Pre: Convey serious news about a patient's illness to the patient or family` = `Convey serious news about a patient's illness to the patient or family`,
    `Pre: Convey prognosis to a patient or family member` = `Convey prognosis to a patient or family member`,
    `Pre: Recognize emotion and respond with empathy to a patient or family member` = `Recognize emotion and respond with empathy to a patient or family member`,
    `Pre: Lead a family meeting` = `Lead a family meeting`,
    `Pre: Manage conflict that arises during a difficult discussion` = `Manage conflict that arises during a difficult discussion`,
    `Pre: Respond to patients who deny the seriousness of their illness` = `Respond to patients who deny the seriousness of their illness`,
    `Pre: Respond to patients who want treatments you believe are not indicated` = `Respond to patients who want treatments you believe are not indicated`,
    `Pre: Discuss religious or spiritual issues with a patient` = `Discuss religious or spiritual issues with a patient`,
    `Pre: Elicit a patient's personal values and preferences` = `Elicit a patient's personal values and preferences`
  )
# recode likert scale data 1-5 into binary 0,1
# Function to recode values using tidyverse
recode_values_tidy <- function(x) {
  if_else(x %in% 1:3, 0, if_else(x %in% 4:5, 1, NA_integer_))
}
# Recode the values for specified columns using dplyr and across
complete_data1 <- complete_data %>%
  mutate(across(
    contains("Pre:"),
    recode_values_tidy
  )) %>%
  mutate(across(
    contains("BEFORE TRAINING:"),
    recode_values_tidy
  )) |>
  mutate(across(
    contains("AFTER TRAINING:"),
    recode_values_tidy
  )) |>
  mutate(across(
    contains("How comfortable are you delivering serious news"),
    recode_values_tidy
  )) |>
  mutate(across(
    contains("how comfortable were you delivering serious news"),
    recode_values_tidy
  ))
#summarize data in n(%)
complete_data1 |>
  tbl_summary(digits = list(all_categorical() ~ c(0, 1)))
############compare pre(retro) and post################################
#McNemar's test
# Identify pairs of questions
paired_questions <- c("Convey serious news about a patient's illness to the patient or family", 
                      "Convey prognosis to a patient or family member",
                      "Recognize emotion and respond with empathy to a patient or family member",
                      "Lead a family meeting",
                      "Manage conflict that arises during a difficult discussion",
                      "Respond to patients who deny the seriousness of their illness",
                      "Respond to patients who want treatments you believe are not indicated"
                      # ... add other question stems here
)

# Loop through pairs and perform McNemar's tests
results <- vector("list", length = length(paired_questions))
names(results) <- paired_questions

for (question_stem in paired_questions) {
  before_col <- paste("BEFORE TRAINING:", question_stem)
  after_col <- paste("AFTER TRAINING:", question_stem)
  
  # Create a contingency table for the variables
  contingency_table <- table(complete_data1[[before_col]], complete_data1[[after_col]])
  print(contingency_table)
  
  # Run McNemar's test only if the contingency table is a 2x2 table
  if(is.matrix(contingency_table) && nrow(contingency_table) == 2 && ncol(contingency_table) == 2){
    mcnemar_result <- mcnemar.test(contingency_table)
    print(mcnemar_result)
    
    # Storing the result if the test is performed
    results[[question_stem]] <- mcnemar_result
  } else {
    print(paste("The contingency table for", question_stem, "is not suitable for McNemar's test."))
    # Store NA to indicate the test was not performed
    results[[question_stem]] <- NA
  }
}
# To see the results of the first test, if it was performed
if (!is.na(results[[1]])) {
  print(results[[1]])
} else {
  print("Result for the first question stem is not available because the test was not performed.")
}
# 3 out of 7 paired questions data not suitable for McNemar's test. 
#######################################################################################################
# Switch to Wilcoxon Signed-Rank Test
#automation
paired_questions <- c(
  "Convey serious news about a patient's illness to the patient or family",
  "Convey prognosis to a patient or family member",
  "Recognize emotion and respond with empathy to a patient or family member",
  "Lead a family meeting",
  "Manage conflict that arises during a difficult discussion",
  "Respond to patients who deny the seriousness of their illness",
  "Respond to patients who want treatments you believe are not indicated"
  # ... add other question stems here
)

# Initialize a list to store the results
wilcoxon_results <- list()

for (question_stem in paired_questions) {
  before_col <- paste("BEFORE TRAINING:", question_stem)
  after_col <- paste("AFTER TRAINING:", question_stem)
  
  # Select the scores for before and after training
  before_scores <- complete_data[[before_col]]
  after_scores <- complete_data[[after_col]]
  
  # Calculate median, Q1 and Q3 for before and after scores
  before_median <- median(before_scores, na.rm = TRUE)
  after_median <- median(after_scores, na.rm = TRUE)
  before_quantiles <- quantile(before_scores, probs = c(0.25, 0.75), na.rm = TRUE)
  after_quantiles <- quantile(after_scores, probs = c(0.25, 0.75), na.rm = TRUE)
  
  # Perform the Wilcoxon Signed-Rank Test
  test_result <- wilcox.test(before_scores, after_scores, paired = TRUE, exact = FALSE, correct = TRUE)
  
  # Summarize and store the results
  wilcoxon_results[[question_stem]] <- list(
    before_summary = summary(before_scores),
    after_summary = summary(after_scores),
    before_median_iqr = sprintf("Median (Q1, Q3): %s (%s, %s)", before_median, before_quantiles[1], before_quantiles[2]),
    after_median_iqr = sprintf("Median (Q1, Q3): %s (%s, %s)", after_median, after_quantiles[1], after_quantiles[2]),
    test_result = test_result
  )
  
  # Optional: Print the results for each test
  cat("Wilcoxon Signed-Rank Test for:", question_stem, "\n")
  cat("Before Training Median (Q1, Q3):", wilcoxon_results[[question_stem]]$before_median_iqr, "\n")
  cat("After Training Median (Q1, Q3):", wilcoxon_results[[question_stem]]$after_median_iqr, "\n")
  print(test_result)
  cat("\n")
}

# To access the summary for a particular question, for example:
wilcoxon_results[["Convey serious news about a patient's illness to the patient or family"]]
####################compare pre and post#######################
#McNemar's test
#automation
# Identify pairs of questions
paired_questions <- c("Convey serious news about a patient's illness to the patient or family", 
                      "Convey prognosis to a patient or family member",
                      "Recognize emotion and respond with empathy to a patient or family member",
                      "Lead a family meeting",
                      "Manage conflict that arises during a difficult discussion",
                      "Respond to patients who deny the seriousness of their illness",
                      "Respond to patients who want treatments you believe are not indicated"
                      # ... add other question stems here
)

# Loop through pairs and perform McNemar's tests
results <- vector("list", length = length(paired_questions))
names(results) <- paired_questions

for (question_stem in paired_questions) {
  before_col <- paste("Pre:", question_stem)
  after_col <- paste("AFTER TRAINING:", question_stem)
  
  # Create a contingency table for the variables
  contingency_table <- table(complete_data1[[before_col]], complete_data1[[after_col]])
  print(contingency_table)
  
  # Run McNemar's test only if the contingency table is a 2x2 table
  if(is.matrix(contingency_table) && nrow(contingency_table) == 2 && ncol(contingency_table) == 2){
    mcnemar_result <- mcnemar.test(contingency_table)
    print(mcnemar_result)
    
    # Storing the result if the test is performed
    results[[question_stem]] <- mcnemar_result
  } else {
    print(paste("The contingency table for", question_stem, "is not suitable for McNemar's test."))
    # Store NA to indicate the test was not performed
    results[[question_stem]] <- NA
  }
}

# To see the results of the first test, if it was performed
if (!is.na(results[[1]])) {
  print(results[[1]])
} else {
  print("Result for the first question stem is not available because the test was not performed.")
}
# 3 out of 7 paired questions data not suitable for McNemar's test. 
#######################################################################################################
# Switch to Wilcoxon Signed-Rank Test
paired_questions <- c(
  "Convey serious news about a patient's illness to the patient or family",
  "Convey prognosis to a patient or family member",
  "Recognize emotion and respond with empathy to a patient or family member",
  "Lead a family meeting",
  "Manage conflict that arises during a difficult discussion",
  "Respond to patients who deny the seriousness of their illness",
  "Respond to patients who want treatments you believe are not indicated"
  # ... add other question stems here
)

# Initialize a list to store the results
wilcoxon_results <- list()

for (question_stem in paired_questions) {
  before_col <- paste("Pre:", question_stem)
  after_col <- paste("AFTER TRAINING:", question_stem)
  
  # Select the scores for before and after training
  before_scores <- complete_data[[before_col]]
  after_scores <- complete_data[[after_col]]
  
  # Calculate median, Q1 and Q3 for before and after scores
  before_median <- median(before_scores, na.rm = TRUE)
  after_median <- median(after_scores, na.rm = TRUE)
  before_quantiles <- quantile(before_scores, probs = c(0.25, 0.75), na.rm = TRUE)
  after_quantiles <- quantile(after_scores, probs = c(0.25, 0.75), na.rm = TRUE)
  
  # Perform the Wilcoxon Signed-Rank Test
  test_result <- wilcox.test(before_scores, after_scores, paired = TRUE, exact = FALSE, correct = TRUE)
  
  # Summarize and store the results
  wilcoxon_results[[question_stem]] <- list(
    before_summary = summary(before_scores),
    after_summary = summary(after_scores),
    before_median_iqr = sprintf("Median (Q1, Q3): %s (%s, %s)", before_median, before_quantiles[1], before_quantiles[2]),
    after_median_iqr = sprintf("Median (Q1, Q3): %s (%s, %s)", after_median, after_quantiles[1], after_quantiles[2]),
    test_result = test_result
  )
  
  # Optional: Print the results for each test
  cat("Wilcoxon Signed-Rank Test for:", question_stem, "\n")
  cat("Before Training Median (Q1, Q3):", wilcoxon_results[[question_stem]]$before_median_iqr, "\n")
  cat("After Training Median (Q1, Q3):", wilcoxon_results[[question_stem]]$after_median_iqr, "\n")
  print(test_result)
  cat("\n")
}

# To access the summary for a particular question, for example:
wilcoxon_results[["Convey serious news about a patient's illness to the patient or family"]]

###################compare pre and retro#####################
#McNemar's test
# Loop through pairs and perform McNemar's tests
results <- vector("list", length = length(paired_questions))
names(results) <- paired_questions

for (question_stem in paired_questions) {
  before_col <- paste("Pre:", question_stem)
  retro_col <- paste("BEFORE TRAINING:", question_stem)
  
  # Create a contingency table for the variables
  contingency_table <- table(complete_data1[[before_col]], complete_data1[[retro_col]])
  print(contingency_table)
  
  # Run McNemar's test only if the contingency table is a 2x2 table
  if(is.matrix(contingency_table) && nrow(contingency_table) == 2 && ncol(contingency_table) == 2){
    mcnemar_result <- mcnemar.test(contingency_table)
    print(mcnemar_result)
    
    # Storing the result if the test is performed
    results[[question_stem]] <- mcnemar_result
  } else {
    print(paste("The contingency table for", question_stem, "is not suitable for McNemar's test."))
    # Store NA to indicate the test was not performed
    results[[question_stem]] <- NA
  }
}

# To see the results of the first test, if it was performed
if (!is.na(results[[1]])) {
  print(results[[1]])
} else {
  print("Result for the first question stem is not available because the test was not performed.")
}
# 1 out of 7 paired questions data not suitable for McNemar's test. 
#######################################################################################################
# Switch to Wilcoxon Signed-Rank Test
paired_questions <- c(
  "Convey serious news about a patient's illness to the patient or family",
  "Convey prognosis to a patient or family member",
  "Recognize emotion and respond with empathy to a patient or family member",
  "Lead a family meeting",
  "Manage conflict that arises during a difficult discussion",
  "Respond to patients who deny the seriousness of their illness",
  "Respond to patients who want treatments you believe are not indicated"
  # ... add other question stems here
)

# Initialize a list to store the results
wilcoxon_results <- list()

for (question_stem in paired_questions) {
  before_col <- paste("Pre:", question_stem)
  retro_col <- paste("BEFORE TRAINING:", question_stem)
  
  # Select the scores for before and after training
  before_scores <- complete_data[[before_col]]
  retro_scores <- complete_data[[retro_col]]
  
  # Calculate median, Q1 and Q3 for before and after scores
  before_median <- median(before_scores, na.rm = TRUE)
  retro_median <- median(retro_scores, na.rm = TRUE)
  before_quantiles <- quantile(before_scores, probs = c(0.25, 0.75), na.rm = TRUE)
  retro_quantiles <- quantile(retro_scores, probs = c(0.25, 0.75), na.rm = TRUE)
  
  # Perform the Wilcoxon Signed-Rank Test
  test_result <- wilcox.test(before_scores, retro_scores, paired = TRUE, exact = FALSE, correct = TRUE)
  
  # Summarize and store the results
  wilcoxon_results[[question_stem]] <- list(
    before_summary = summary(before_scores),
    retro_summary = summary(retro_scores),
    before_median_iqr = sprintf("Median (Q1, Q3): %s (%s, %s)", before_median, before_quantiles[1], before_quantiles[2]),
    retro_median_iqr = sprintf("Median (Q1, Q3): %s (%s, %s)", retro_median, retro_quantiles[1], retro_quantiles[2]),
    test_result = test_result
  )
  
  # Optional: Print the results for each test
  cat("Wilcoxon Signed-Rank Test for:", question_stem, "\n")
  cat("Before Training Median (Q1, Q3):", wilcoxon_results[[question_stem]]$before_median_iqr, "\n")
  cat("After Training Retro Median (Q1, Q3):", wilcoxon_results[[question_stem]]$retro_median_iqr, "\n")
  print(test_result)
  cat("\n")
}

# To access the summary for a particular question, for example:
wilcoxon_results[["Convey serious news about a patient's illness to the patient or family"]]



######################exact McNemar test#######################################
library(exact2x2) # Load the package for exact McNemar test
# Define pairs of pre- and post-questions
paired_questions <- c("b1r", "b2", "b3", "b4", "b5", "b6r", "b7", "b8", "b9", "b10r", "b11", "b12r", "b13r")

# Initialize a list to store results
results <- vector("list", length(paired_questions))
names(results) <- paired_questions

for (question_stem in paired_questions) {
  before_col <- paste("pre_", question_stem, sep = "")
  after_col <- paste("post_", question_stem, sep = "")
  
  # Create a contingency table for the before/after pairs
  contingency_table <- table(bonus1[[before_col]], bonus1[[after_col]])
  print(contingency_table)
  
  # Check if the table is a 2x2 matrix and has nonzero entries in the off-diagonal cells
  if(is.matrix(contingency_table) && all(dim(contingency_table) == c(2, 2)) &&
     all(diag(contingency_table) != sum(contingency_table))){
    # Perform McNemar's test
    mcnemar_result <- mcnemar.test(contingency_table)
    print(mcnemar_result)
    results[[question_stem]] <- mcnemar_result
  } else {
    cat("The contingency table for", question_stem, "is not suitable for McNemar's test.\n")
    results[[question_stem]] <- NA # Store NA to indicate the test was not performed
  }
}



################comfortable question####################
before_col <- "How comfortable are you delivering serious news to a patient?"
after_col <- "AFTER this training, how comfortable are you delivering serious news to a patient?"

# Create a contingency table for the variables
contingency_table <- table(complete_data1[[before_col]], complete_data1[[after_col]])
print(contingency_table)
# Run McNemar's test
mcnemar_result <- mcnemar.test(contingency_table)
print(mcnemar_result)
# if get error message (not a 2x2 contigency table), Add missing levels if necessary to ensure a 2x2 table
contingency_table <- addmargins(table(factor(postsurvey[[before_col]], levels = 0:1), 
                                      factor(postsurvey[[after_col]], levels = 0:1)), 1)

print(contingency_table)
if(is.matrix(contingency_table) && nrow(contingency_table) == ncol(contingency_table)){
  mcnemar_result <- mcnemar.test(contingency_table[-3, -3])  # Remove the margin (sum) row and column
  print(mcnemar_result)
} else {
  print("The contingency table is not suitable for McNemar's test.")
}

# Wilcoxon Signed-Rank Test
before_col <- "BEFORE this training, how comfortable were you delivering serious news to a patient?" 
after_col <- "AFTER this training, how comfortable are you delivering serious news to a patient?"
before_scores <- complete_data[[before_col]]
after_scores <- complete_data[[after_col]]
# Summary of 'before training' scores
summary(complete_data[[before_col]])
# Summary of 'after training' scores
summary(complete_data[[after_col]])
# Wilcoxon Signed-Rank Test
wilcoxon_result <- wilcox.test(before_scores, after_scores, 
                               paired = TRUE, # paired sample: before and after
                               exact = FALSE, # ties and zero differences exist
                               correct = TRUE) # Apply continuity correction that can be useful for discrete data like Likert scales.
print(wilcoxon_result)
