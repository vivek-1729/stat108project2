library(tidyverse)
library(stringdist)
#might need to install the package stringdist

data <- read_csv("data/data.csv")

# This function detects outliers, either using the mean (z-score) or median (IQR) approach
# You can determine the Z-score threshold or IQR multiple to determine if a value is an outlier
# The function returns a table showing the row number, outlier value, and threshold value

#Five functions
#1. Detect outliers: returns table of possible outliers
#2. na_summary: returns table summary of all the na values in the dataset
#3. visualize_outliers: pair plots numeric columns and visualizes possible outliers in red
#4. true_yardline: corrects for the way the NFL counts yardline (calculates true distance from the team's endzone to current position)
#5. check_misspelling: checks for misspellings based in a certain column based on common values for that column


detect_outliers <- function(data, column, method = "mean", threshold = 3) {
  # Check if the method is valid
  if (!method %in% c("mean", "median")) {
    stop("Invalid method. Use 'mean' for Z-score or 'median' for IQR.")
  }

  # Extract the column data
  column_data <- data[[column]]

  # Initialize an empty data frame for results
  results <- data.frame(
    Row = integer(),
    Value = numeric(),
    Threshold = numeric(),
    stringsAsFactors = FALSE
  )

  if (method == "mean") {
    # Calculate Z-scores
    mean_value <- mean(column_data, na.rm = TRUE)
    sd_value <- sd(column_data, na.rm = TRUE)
    z_scores <- (column_data - mean_value) / sd_value

    # Identify outliers
    outlier_indices <- which(abs(z_scores) > threshold)

    # Populate results
    results <- data.frame(
      Row = outlier_indices,
      Value = column_data[outlier_indices],
      Z_Score = z_scores[outlier_indices]
    )

  } else if (method == "median") {
    # Calculate IQR
    Q1 <- quantile(column_data, 0.25, na.rm = TRUE)
    Q3 <- quantile(column_data, 0.75, na.rm = TRUE)
    IQR_value <- Q3 - Q1

    # Identify outliers
    lower_bound <- Q1 - threshold * IQR_value
    upper_bound <- Q3 + threshold * IQR_value
    outlier_indices <- which(column_data < lower_bound | column_data > upper_bound)

    # Populate results
    distance_to_iqr <- ifelse(
      column_data[outlier_indices] < lower_bound,
      lower_bound - column_data[outlier_indices],
      column_data[outlier_indices] - upper_bound
    )
    results <- data.frame(
      Row = outlier_indices,
      Value = column_data[outlier_indices],
      Distance_to_Bound = distance_to_iqr
    )
  }

  return(results)
}
detect_outliers(data, "yardsToGo", "median")

#This function provides a summary of all the NA values in the dataframe
# It tells you the number of NA values in each column, the total percent of NA values vs the other values,
# the number of times when the NA in the column is "row-unique" (it is the only NA for its row), the mean of the column,
# and the standard deviation of the column

na_summary <- function(data) {
  # Ensure data is a data frame
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }

  # Initialize results as a list
  results <- list()

  # Loop through each column to calculate statistics
  for (col in names(data)) {
    col_data <- data[[col]]

    # Number of NAs in the column
    num_nas <- sum(is.na(col_data))

    # Percentage of NAs
    percentage_nas <- (num_nas / nrow(data)) * 100

    # Number of row-unique NAs
    row_unique_nas <- sum(rowSums(is.na(data)) == 1 & is.na(col_data))

    # Column mean and standard deviation
    if (is.numeric(col_data)) {
      col_mean <- as.character(mean(col_data, na.rm = TRUE))
      col_sd <- as.character(sd(col_data, na.rm = TRUE))
    } else {
      col_mean <- "not numeric"
      col_sd <- "not numeric"
    }

    # Append statistics for the column
    results[[col]] <- list(
      Column = col,
      Number_of_NAs = num_nas,
      Percentage_of_NAs = percentage_nas,
      Row_Unique_NAs = row_unique_nas,
      Column_Mean = col_mean,
      Column_SD = col_sd
    )
  }

  # Convert results list to a tibble
  results_tibble <- bind_rows(results)

  # Arrange by highest number of NAs
  results_tibble <- results_tibble %>%
    arrange(desc(Number_of_NAs))

  return(results_tibble)
}
na_summary(data)


#This functions enables you to visualize the numeric data
# It returns a plot that shows the distribution (histogram) of each of the columns
# it also computes all the outliers using the z-score and shows them in red

visualize_outliers <- function(data, ..., z_threshold = 3) {
  # Capture the column names passed as arguments
  columns <- enquos(...)

  # Check if at least two columns are provided
  if (length(columns) < 2) {
    stop("At least two columns must be specified.")
  }

  # Select the specified columns
  numeric_data <- data %>%
    select(!!!columns)

  # Ensure all specified columns are numeric
  if (!all(sapply(numeric_data, is.numeric))) {
    stop("All specified columns must be numeric.")
  }

  # Add an outlier flag for each column
  numeric_data <- numeric_data %>%
    mutate(across(
      everything(),
      ~ ifelse(abs((. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)) > z_threshold, "Outlier", "Normal"),
      .names = "outlier_{col}"
    ))

  # Get column names for looping
  column_names <- colnames(numeric_data)[seq_along(columns)]
  n <- length(column_names)

  # Initialize an empty matrix to store plots
  plots <- matrix(list(), n, n)

  # Generate plots
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) {
        # Diagonal: Histograms
        p <- ggplot(numeric_data, aes_string(x = column_names[i])) +
          geom_histogram(bins = 20, fill = "blue", alpha = 0.7) +
          theme_minimal() +
          labs(x = column_names[i], y = "Count") +
          theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
      } else {
        # Off-diagonal: Scatterplots with outlier coloring
        x_var <- column_names[j]
        y_var <- column_names[i]
        y_outlier_col <- paste0("outlier_", y_var)

        # Scatterplot with outliers in red
        p <- ggplot(numeric_data, aes_string(x = x_var, y = y_var)) +
          geom_point(aes(color = .data[[y_outlier_col]]), alpha = 0.7) +
          scale_color_manual(values = c("Outlier" = "red", "Normal" = "black")) +
          theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "none"
          )
          labs(x = x_var, y = y_var, color = "Legend")
      }
      # Store plot in the matrix
      plots[i, j] <- list(p)
    }
  }

  # Combine plots into a grid using patchwork
  plot_grid <- wrap_plots(plots, byrow = FALSE)

  return(plot_grid)
}
pair_scatterplots(data, "yardsToGo", "PassLength", "YardsAfterCatch")


#NFL data idiosyncrasy: the numbering only goes up to 50 yards and then switches when you get to opponents side.
#This computes the "true yardline"

true_yardline <- function(data, possession_col, yardline_side_col, yardline_number_col) {
  # Ensure required columns are in the data
  if (!all(c(possession_col, yardline_side_col, yardline_number_col) %in% colnames(data))) {
    stop("Specified columns do not exist in the data.")
  }

  # Calculate true yardline
  yardline <- data %>%
    mutate(
      yardline = ifelse(
        .data[[possession_col]] == .data[[yardline_side_col]],
        .data[[yardline_number_col]],        # Offensive team is on its own side
        100 - .data[[yardline_number_col]]  # Offensive team is on the opponent's side
      )
    ) %>%
    select(yardline) # Select only the "true_yardline" column

  return(as_tibble(yardline))
}
true_yardline(data, "possessionTeam", "yardlineSide", "yardlineNumber")



# checks mispellings by finding all the common values for a certain column. Then, if there is an uncommmon
# value in the column that looks similar to a common value, it flags it as a potential mispelling

check_misspellings <- function(data, column_name, frequency_threshold = 3, similarity_threshold = 0.8) {
  # Ensure the specified column exists
  if (!column_name %in% colnames(data)) {
    stop("Specified column does not exist in the data.")
  }

  # Calculate the frequency of each value in the column
  value_counts <- data %>%
    count(.data[[column_name]], name = "Frequency") %>%
    arrange(desc(Frequency))

  # Separate common and less common values
  common_values <- value_counts %>%
    filter(Frequency >= frequency_threshold) %>%
    pull(.data[[column_name]])

  less_common_values <- value_counts %>%
    filter(Frequency < frequency_threshold) %>%
    pull(.data[[column_name]])

  # Initialize a results data frame
  results <- data.frame(
    Original_Value = character(),
    Similar_To = character(),
    Similarity_Score = numeric(),
    stringsAsFactors = FALSE
  )

  # Compare less common values to common values
  for (less_common in less_common_values) {
    for (common in common_values) {
      # Calculate string similarity
      similarity <- stringdist::stringsim(less_common, common, method = "jw")

      # Skip comparison if similarity is NA
      if (!is.na(similarity) && similarity > similarity_threshold) {
        results <- rbind(results, data.frame(
          Original_Value = less_common,
          Similar_To = common,
          Similarity_Score = similarity,
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  # Return the results as a tibble
  return(as_tibble(results))
}
check_misspellings(data, "offenseFormation")
