### International economy ###


# This R code provides the extraction and data wrangling of macroeconomic
# variables referring to International economy


# Packages ----------------------------------------------------------------


# Install/load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  "OECD",
  "tidyverse"
  )



# Parameters --------------------------------------------------------------


# Parameters used in the code to import, download or cleaning data


# List of parameters to get data from OECD API
api_oecd <- list(
  
  # Quarterly National Accounts (code to get dataset)
  dataset_qna = "QNA",
  
  # Main Economic Indicators (code to get  dataset)
  dataset_mei = "MEI",
  
  # Labour statistics (code to get dataset)
  dataset_stlabour = "STLABOUR",
  
  
  # GDP (list of parameters to filter the QNA dataset)
  filter_gdp = "AUS+CAN+CHL+FRA+DEU+ITA+JPN+MEX+ESP+TUR+USA+ARG+BRA+CHN+CRI+IND+RUS.B1_GE.GYSA.A+Q",
  
  # Unemployment (list of parameters to filter the STLABOUR dataset)
  filter_unemployment = "AUS+CAN+CHL+FRA+DEU+ITA+JPN+MEX+ESP+TUR+USA.LRHUTTTT.STSA.M",
  
  # Inflation (list of parameters to filter the MEI dataset)
  filter_inflation = "AUS+CAN+CHL+FRA+DEU+ITA+JPN+MEX+ESP+TUR+USA+ARG+BRA+CHN+CRI+IND+RUS+BRIICS.CPALTT01.GY.M",
  
  # Interest rate (list of parameters to filter the MEI dataset)
  filter_int_rate = "AUS+CAN+CHL+FRA+DEU+ITA+JPN+MEX+ESP+TUR+USA+ARG+BRA+CHN+CRI+IND+RUS+BRIICS.IRSTCB01+IRSTCI01.ST+STSA.M"
  
  )



# Import data -------------------------------------------------------------


# This section performs the import of data from OECD datasets (economic indicators for many countries)


# GDP
str_gdp <- get_data_structure(api_oecd$dataset_qna) # dataset structure information
raw_gdp_oecd <- get_dataset(
  dataset       = api_oecd$dataset_qna,
  filter        = list(api_oecd$filter_gdp),
  pre_formatted = TRUE
  )


# Unemployment Rate
str_unemployment <- get_data_structure(api_oecd$dataset_stlabour) # dataset structure information
raw_unemployment_oecd <- get_dataset(
  dataset       = api_oecd$dataset_stlabour,
  filter        = list(api_oecd$filter_unemployment),
  pre_formatted = TRUE
  )


# Inflation
str_inflation <- get_data_structure(api_oecd$dataset_mei) # dataset structure information
raw_inflation_oecd <- get_dataset(
  dataset       = api_oecd$dataset_mei,
  filter        = list(api_oecd$filter_inflation),
  pre_formatted = TRUE
  )


# Interest rate
raw_int_rate <- get_dataset(
  dataset       = api_oecd$dataset_mei,
  filter        = list(api_oecd$filter_int_rate),
  pre_formatted = TRUE
  )




# Data wrangling ----------------------------------------------------------


# This section performs data wrangling


# GDP
gdp_oecd <- raw_gdp_oecd %>%
  filter(FREQUENCY == "Q") %>%
  select(
    date     = obsTime, 
    location = LOCATION, 
    value    = obsValue, 
    status   = contains("OBS_STATUS")
    ) %>%
  left_join(
    str_gdp$LOCATION,
    by = c("location" = "id")
  ) %>%
  mutate(
    across(
      any_of("status"),
      ~recode(
        status,
        "E" = "Estimated value",
        "P" = "Provisional value"
        )
      )
    )


# Inflation
inflation_oecd <- raw_inflation_oecd %>%
  select(
    date     = obsTime, 
    location = LOCATION, 
    value    = obsValue, 
    status   = contains("OBS_STATUS")
  ) %>%
  left_join(
    str_inflation$LOCATION,
    by = c("location" = "id")
    ) %>%
  mutate(
    date = paste0(date, "-01") %>% str_replace_all("-", "/"),
    across(
      any_of("status"),
      ~recode(
        status,
        "E" = "Estimated value",
        "P" = "Provisional value"
        )
      )
    )


# Unemployment rate
unemployment_oecd <- raw_unemployment_oecd %>%
  select(
    date     = obsTime, 
    location = LOCATION, 
    value    = obsValue, 
    status   = contains("OBS_STATUS")
  ) %>%
  left_join(
    str_unemployment$LOCATION,
    by = c("location" = "id")
    ) %>%
  mutate(
    date = paste0(date, "-01") %>% str_replace_all("-", "/"),
    across(
      any_of("status"),
      ~recode(
        status,
        "E" = "Estimated value",
        "P" = "Provisional value",
        "B" = "Break"
        )
      )
    )


# Interest rate
int_rate_oecd <- raw_int_rate %>%
  filter(SUBJECT == "IRSTCI01") %>%
  select(
    date     = obsTime, 
    location = LOCATION, 
    value    = obsValue, 
    status   = contains("OBS_STATUS")
  ) %>%
  left_join(
    str_inflation$LOCATION,
    by = c("location" = "id")
  ) %>%
  mutate(
    date = paste0(date, "-01") %>% str_replace_all("-", "/"),
    across(
      any_of("status"),
      ~recode(
        status,
        "E" = "Estimated value",
        "P" = "Provisional value",
        "B" = "Break"
        )
      )
    )




# Save data ---------------------------------------------------------------


# Aggregate data
imported_data_international <- mget(ls(pattern = "raw_|api_|str_"))


# Remove unnecessary objects
rm(list  = c(lsf.str(), ls(pattern = "raw_|api_|str_")),  # remove function objects
   envir = .GlobalEnv)


# Save RDATA file
save.image(file = file.path(file.path("./data"), "international.Rdata"))

