#' ETL Monetary policy
#'
#' @encoding UTF-8
#' @import dplyr
#' @import tidyselect
#' @importFrom purrr map_dfr
#' @importFrom utils lsf.str
#' @importFrom stats na.omit update
#' @return RDATA
#' @export
#'
etl_monetary <- function(){

### Monetary policy ###     TODO: create retry for API wrappers


# This R code provides the extraction and data wrangling of macroeconomic
# variables referring to the Brazilian Monetary policy


# Packages ----------------------------------------------------------------


# Install/load packages
# if (!require("pacman")) install.packages("pacman")
# if (!require("meedr")) devtools::install_github("schoulten/meedr")
# pacman::p_load(
#   "tibbletime",
#   "tidyverse",
#   "GetBCBData",
#   "lubridate",
#   "zoo",
#   "sidrar",
#   "GetTDData",
#   "meedr",
#   "ipeadatar",
#   "tidyquant",
#   "timetk"
#   )



# Parameters --------------------------------------------------------------


# Parameters used in the code to import, download or cleaning data


# Load useful functions
# source("./R/utils.R")


# List of parameters to get data from Central Bank (API)
api_bcb <- dplyr::lst(

  # Interest Rate (short-term) - % p.y.
  api_interest_rate = c(
    "SELIC Target rate" = 432,  # Defined by COPOM/BCB - daily
    "CDI Rate"          = 4389, # CDI in annual terms (basis 252) - daily
    "SELIC Rate"        = 4189  # Selic accumulated in the month in annual terms (basis 252) - monthly
    ),

  # Inflation's market expectations for the next 12 months (IPCA indicator)
  api_inflation_expec = "IPCA",

  # SELIC annual market expectations
  api_selic_expec = "Selic",

  # Currency rates
  symbol = c("USD", "EUR", "ARS", "MXN", "CNY", "TRY", "RUB", "INR", "SAR", "ZAR") %>%
      purrr::set_names(.),

  currencies = tibble(
    currency = c(
      "US Dollar",
      "Euro",
      "Argentine Peso",
      "Mexican Peso",
      "Chinese Renminbi",
      "Turkish Lira",
      "Russian Ruble",
      "Indian Rupee",
      "Saudi Riyal",
      "South African Rand"
      ),
    symbol = symbol
    )

  )


# List of parameters to get data from IPEADATA (API)
api_ipeadata <- list(

  # EMBI+ Risk-Brasil - daily - JP Morgan
  api_embi = "JPM366_EMBI366",

  # Reference rate - swaps - DI fixed rate - 360 days - period average - monthly - B3
  api_swaps = "BMF12_SWAPDI36012",

  # Market expectations for inflation over the next 12 months - monthly mean
  api_inflation = "BM12_IPCAEXP1212"

  )


# List of parameters to get data from SIDRA/IBGE website
api_sidra <- list(

  # Consumer Price Index - IPCA
  api_ipca = "/t/1737/n1/all/v/2265/p/all/d/v2265%202"

  )


# List of parameters to get data from Yahoo Finance (API)
api_yahoo <- list(

  # Ibovespa index (B3)
  api_ibov = "^BVSP"

  )



# Import data -------------------------------------------------------------


# This section performs the import of data from different sources


# Interest Rate, short-term, % p.y.
raw_interest_rate <- GetBCBData::gbcbd_get_series(
  id          = api_bcb$api_interest_rate,
  first.date  = "2001-11-07",
  use.memoise = FALSE
  )

# Inflation's market expectations for the next 12 months
raw_inflation_expec <- meedr::get_inflation_12m(
  indicator   = api_bcb$api_inflation_expec,
  first_date  = NULL,
  smoothed    = "yes",
  use_memoise = FALSE
  )

# SELIC annual market expectations
raw_selic_expec <- meedr::get_annual(
  indicator      = api_bcb$api_selic_expec,
  first_date     = NULL,
  # detail         = "Fim do ano", DEPRECATED
  reference_date = format(Sys.Date(), "%Y"),
  use_memoise    = FALSE
  )

# Currency rates
raw_currency <- purrr::map_dfr(
  api_bcb$symbol,
  ~rbcb::get_currency(.x, as = "tibble", start_date = Sys.Date() - 3 * 365, end_date = Sys.Date()),
  .id = "symbol"
  )

# EMBI+ Risk-Brazil
raw_embi <- ipeadatar::ipeadata(api_ipeadata$api_embi)

# Swaps - DI fixed rate - 360 days
raw_swaps <- ipeadatar::ipeadata(api_ipeadata$api_swaps)

# Market expectations for inflation over the next 12 months - monthly mean
raw_inflation_next_12m <- ipeadatar::ipeadata(api_ipeadata$api_inflation)

# Current yield curve (ETTJ/Anbima)
raw_ettj <- GetTDData::get.yield.curve()

# Ibovespa index (B3)
raw_ibovespa <- quantmod::getSymbols(
  Symbols     = "^BVSP",
  src         = "yahoo",
  auto.assign = FALSE
  )

# Consumer Price Index - IPCA (accumulated variation in 12 months)
raw_ipca_12m <- sidrar::get_sidra(api = api_sidra$api_ipca) %>%
  select(
    date     = "M\u00eas (C\u00f3digo)",
    variable = "Vari\u00e1vel",
    value    = "Valor"
    )




# Data wrangling ----------------------------------------------------------


# This section performs data wrangling


# Interest Rate (SELIC target), short-term, % p.a.
selic <- raw_interest_rate %>%
  select(
    date = "ref.date",
    variable = "series.name",
    value
    ) %>%
  filter(
    date == max(date),
    variable == "SELIC Target rate"
    ) %>%
  mutate(value = paste0(value, "%"))


# Inflation expectations (IPCA)
inflation_expec <- raw_inflation_expec %>%
  filter(basis == 0) %>%
  select(
    date,
    "variable" = indicator,
    "value"    = mean
    ) %>%
  timetk::condense_period(
    .date_var = date,
    .period   = "1 month",
    .side     = "end"
    ) %>%
  mutate(
    date = format(date, "%Y-%m-01") %>% lubridate::ymd()
    )


# Short-term interest rates
interest_rate <- raw_interest_rate %>%
  select(
    date = "ref.date",
    variable = "series.name",
    value
    ) %>%
  group_by(variable) %>%
  timetk::condense_period(
    .date_var = date,
    .period   = "1 month",
    .side     = "end"
    ) %>%
  ungroup() %>%
  mutate(
    date = format(date, "%Y-%m-01") %>% lubridate::ymd()
    )


# Real interest rates (ex-ante and ex-post)
real_interest_rate <- bind_rows(

  # Ex-ante
  {
    inner_join(
      raw_swaps %>%
        dplyr::select("date", "swaps" = "value"),
      raw_inflation_expec %>%
        filter(basis == 0) %>%
        dplyr::group_by(date = lubridate::floor_date(`date`, unit = "months")) %>%
        dplyr::summarise(inflation_next_12m = mean(`median`)),
      by = "date"
      ) %>%
      mutate(
        value    = (((1 + (swaps / 100)) / (1 + (inflation_next_12m / 100))) - 1) * 100, # Fisher Equation
        variable = "Ex-ante"
        ) %>%
      select(date, variable, value)
    },

  # Ex-post
  {
    raw_ipca_12m %>%
      mutate(
        date = lubridate::ymd(paste0(date, "01"))
        ) %>%
      tidyr::drop_na() %>%
      select(
        date,
        ipca_12m = value
        ) %>%
      inner_join(
        raw_interest_rate %>%
          filter(series.name == "SELIC Rate") %>%
          select(
            date      = ref.date,
            selic_12m = value
            ),
        by = "date"
        ) %>%
      mutate(
        value    = (((1 + (selic_12m / 100)) / (1 + (ipca_12m / 100))) - 1) * 100, # Fisher Equation
        variable = "Ex-post") %>%
    select(date, variable, value)
    }

  ) %>%
  mutate(value = round(value, 2))


# Exchange rate (currency/R$)
currencies <- raw_currency %>%
  group_by(
    symbol,
    date = format(date, "%Y-%m-01")
    ) %>%
  summarise(
    value = mean(ask),
    .groups = "drop"
    ) %>%
  group_by(symbol) %>%
  mutate(
    mom  = (value / dplyr::lag(value) - 1) * 100,
    ytd  = (value / dplyr::lag(value, as.numeric(format(Sys.Date(), "%m"))-1) - 1) * 100,
    yoy  = (rolling(value, mean, 12) / rolling(dplyr::lag(value, 12), mean, 12) - 1) * 100,
    across(where(is.numeric), ~round(., 2)),
    spark = list(value)
    ) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  left_join(
    api_bcb$currencies,
    by = "symbol"
    ) %>%
  arrange(order(api_bcb$currencies$symbol)) %>%
  select(8, 3, 7, 4:6) %>%
  mutate(
    flag = c("us", "eu", "ar", "mx", "cn", "tr", "ru", "in", "sa", "za") %>%
      paste0("./inst/imgs/", ., ".png"), .before = currency
    )

# Footnote
curr_locale <- Sys.getlocale("LC_TIME")
withr::local_locale(c("LC_TIME" = "en_US.UTF-8"))
footnote_currency <- paste0(
  "Average monthly exchange rate, updated to ",
  format(max(raw_currency$date), "%B %d, %Y.")
  )
withr::local_locale(c("LC_TIME" = curr_locale))


# Annual market expectations for the current year's interest rate (Focus/BCB)
selic_expec <- raw_selic_expec %>%
  select(
    date,
    variable = indicator,
    reference_date,
    value = mean
    ) %>%
  timetk::condense_period(
    .date_var = date,
    .period   = "1 month",
    .side     = "end"
    ) %>%
  mutate(
    date     = format(date, "%Y-%m-01") %>% lubridate::ymd(),
    variable = recode(variable, "Meta para taxa over-selic" = "SELIC Target rate"),
    )


# Current yield curve (ETTJ/Anbima)
curr_locale <- Sys.getlocale("LC_TIME")
withr::local_locale(c("LC_TIME" = "en_US.UTF-8"))
ettj <- raw_ettj %>%
  filter(type == "real_return") %>%
  mutate(
    date_query    = format(current.date, "%B %d, %Y"),
    value         = round(value, 2),
    variable      = "Yield curve (ETTJ IPCA)"
    ) %>%
  select(
    date_ref = ref.date,
    business_days = n.biz.days,
    date_query,
    variable,
    value
    )
withr::local_locale(c("LC_TIME" = curr_locale))


# Ibovespa index (B3)
ibovespa <- raw_ibovespa %>%
  na.omit()


# EMBI+ Risk-Brasil - daily - JP Morgan
embi <- raw_embi %>%
  mutate(variable = "EMBI+ Risk-Brasil") %>%
  select(date, variable, value)




# Save data ---------------------------------------------------------------


if (0L %in% purrr::map_dbl(mget(ls()), length)) {

  stop("Some objects are zero in length.", call. = FALSE)

  } else
    {
    # Aggregate data
    imported_data_monetary <- mget(ls(pattern = "raw_|api_"))


    # Remove unnecessary objects
    rm(
      list  = c(lsf.str(), ls(pattern = "raw_|api_")),
      envir = environment()
      )


    # Save RDATA file
    save(
      list  = ls(),
      file  = file.path(file.path("./inst/extdata"), "monetary.Rdata"),
      envir = environment()
      )
    }

}
