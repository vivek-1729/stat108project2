## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)

#' Clean and Merge Mouse Trial Data
#'
#' Reads and cleans the Excel file for the mouse vaccine trial, merges datasets,
#' and flags issues like significant weight loss.
#'
#' @param file_path Path to the Excel file.
#' @return A cleaned and merged data frame.
#' @import dplyr
#' @importFrom readxl read_excel
clean_mouse_data <- function(file_path) {
  # Load necessary libraries
  library(readxl)
  library(dplyr)

  # Read in sheets from the Excel file
  birth_data <- read_excel(file_path, sheet = "Birth")
  body_weight_data <- read_excel(file_path, sheet = "Body Weight")
  outcome_data <- read_excel(file_path, sheet = "Outcome")

  # Clean 'Birth' data
  birth_data <- birth_data %>%
    rename(Mouse_ID = `Mouse ID`, Birth_Date = `Birth Date`) %>%
    mutate(Birth_Date = as.Date(Birth_Date, format = "%Y-%m-%d"))

  # Clean 'Body Weight' data
  body_weight_data <- body_weight_data %>%
    rename(Mouse_ID = `Mouse ID`, Week = `Week`, Weight = `Body Weight`) %>%
    mutate(Weight = as.numeric(Weight)) %>%
    filter(!is.na(Weight))  # Remove rows with missing weight

  # Clean 'Outcome' data
  outcome_data <- outcome_data %>%
    rename(Mouse_ID = `Mouse ID`, Outcome = `Trial Outcome`) %>%
    mutate(Outcome = as.factor(Outcome))  # Convert to factor

  # Merge datasets
  full_data <- birth_data %>%
    inner_join(body_weight_data, by = "Mouse_ID") %>%
    inner_join(outcome_data, by = "Mouse_ID")

  # Add flags for weight loss > 20%
  full_data <- full_data %>%
    group_by(Mouse_ID) %>%
    mutate(Weight_Loss_Flag = ifelse(lag(Weight, default = first(Weight)) - Weight > 0.2 * lag(Weight, default = first(Weight)), TRUE, FALSE)) %>%
    ungroup()

  # Return cleaned dataset
  return(full_data)
}
