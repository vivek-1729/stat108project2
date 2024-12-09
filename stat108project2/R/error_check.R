#' Spellcheck and Detect Swapped Data in "Tmt" Column
#'
#' Performs spellcheck on the "Tmt" column and flags rows with potential data swaps.
#'
#' @param birth_data A data frame containing the "Birth" sheet data.
#' @param expected_tmt A vector of valid treatment names or categories for spellchecking.
#' @return A data frame with additional flags for spellcheck and swapped data issues.
spellcheck_and_flag_tmt <- function(birth_data, expected_tmt) {
  library(dplyr)

  # Add a Spellcheck_Flag column
  birth_data <- birth_data %>%
    mutate(
      Spellcheck_Flag = ifelse(
        !tolower(Tmt) %in% tolower(expected_tmt), TRUE, FALSE
      )
    )

  # Add a Swap_Flag column
  birth_data <- birth_data %>%
    mutate(
      Swap_Flag = ifelse(
        !is.character(Tmt), TRUE, FALSE
      )
    )

  # Return the flagged data frame
  return(birth_data)
}
