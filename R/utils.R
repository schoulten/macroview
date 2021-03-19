### Useful functions for cleaning and processing data ###



# Packages ----------------------------------------------------------------


# Install/load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  "zoo", 
  "lubridate",
  "janitor",
  "dplyr"
  )



# YoY Growth Rate ---------------------------------------------------------


# Function to calculate the growth rate of the last 12 months
# in relation to the same period of the previous year

yoy_growth_rate = function(col, round) { ### CHANGE THIS
  
  # Works within a dataframe
  
  ((zoo::rollsum(
    col, 
    k     = 12, 
    fill  = NA, 
    align = "right"
    ) /
      zoo::rollsum(
        dplyr::lag(col, 12), 
        k     = 12, 
        fill  = NA, 
        align = "right")-1)*100) %>%
    base::round(., digits = round)
  
}




# Accumulate last k values ------------------------------------------------


# Function to calculate the moving sum of the last k periods

accum_k = function(col, k) {
  
  # Works within a dataframe
  
  zoo::rollsum(x     = col,
               k     = k,
               fill  = NA,
               align = "right")
  
}

roll_sum_k <- function(col, k) {
  
  # Works within a dataframe
    timetk::slidify_vec(
      .x      = col,
      .f      = mean,
      .period = k,
      .align  = "right"
      )
  
  } 






# Format SIDRA's date column (monthly) ------------------------------------


# Function to format date time to "ymd" format (from lubridate's package)

ymd_sidra <- function(col){
  
  # Works within a dataframe
  
  format(lubridate::ymd(paste0(col, "01")), format = "%Y/%m/%d")
  
}




# Data wrangling for BCB inflation series ---------------------------------


# Function for data wrangling some data frames from BCB (imported with GetBCBData package)

clean_inflation_bcb <- function(df){
  
  # Must be called in this context:
  # df_clean <- df_raw %>%
  #   clean_inflation_bcb()
  
  df %>% 
    janitor::clean_names() %>%
    dplyr::mutate(date = format(ref_date, "%Y/%m/%d")) %>%
    dplyr::select(date, id = series_name, value)
  
}

