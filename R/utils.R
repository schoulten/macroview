### Useful functions for cleaning and processing data ###


# Rolling operations ------------------------------------------------------

# Function to calculate the moving sum/mean/etc of the k periods

rolling <- function(col, foo, k) {

    timetk::slidify_vec(
      .x      = col,
      .f      = foo,
      .period = k,
      .align  = "right"
      )

  }



# Data wrangling for BCB inflation series ---------------------------------

# Function for data wrangling some data frames from BCB (imported with GetBCBData package)

clean_inflation_bcb <- function(df){

  # Must be called in this context:
  # df_clean <- df_raw %>%
  #   clean_inflation_bcb()

  df %>%
    janitor::clean_names() %>%
    dplyr::select(
      date     = ref_date,
      variable = series_name,
      value
      )

}



# Get max abs value from a DF ---------------------------------------------

max_abs <- function(x) {

  max(abs(x))

  }
