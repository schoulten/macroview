### Monetary policy ###     TODO: create retry for API wrappers


# This R code provides the extraction and data wrangling of macroeconomic
# variables referring to the Brazilian Monetary policy


# Packages ----------------------------------------------------------------


# Install/load packages
if (!require("pacman")) install.packages("pacman")
if (!require("meedr")) devtools::install_github("schoulten/meedr")
pacman::p_load(
  "tibbletime",
  "tidyverse",
  "GetBCBData",
  "lubridate",
  "zoo",
  "sidrar",
  "GetTDData",
  "meedr",
  "ipeadatar",
  "tidyquant",
  "timetk"
  )


# Set the default language of date in R
Sys.setlocale("LC_TIME", "English")



# Parameters --------------------------------------------------------------


# Parameters used in the code to import, download or cleaning data


# Load useful functions
source("./R/utils.R")


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
  api_selic_expec = "Meta para taxa over-selic",

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
  detail         = "Fim do ano",
  reference_date = format(Sys.Date(), "%Y"),
  use_memoise    = FALSE
  )

# Currency rates
raw_currency <- map_dfr(
  api_bcb$symbol,
  ~rbcb::get_currency(.x, as = "tibble", start_date = Sys.Date() - 3 * 365, end_date = Sys.Date()),
  .id = "symbol"
  )

# EMBI+ Risco-Brasil
raw_embi <- ipeadatar::ipeadata(api_ipeadata$api_embi) # improve this (use try or purrr::insistently)

# Swaps - DI fixed rate - 360 days
raw_swaps <- ipeadatar::ipeadata(api_ipeadata$api_swaps)

# Market expectations for inflation over the next 12 months - monthly mean
raw_inflation_next_12m <- ipeadatar::ipeadata(api_ipeadata$api_inflation)

# Current yield curve (ETTJ/Anbima)
raw_ettj <- GetTDData::get.yield.curve()

# Ibovespa index (B3)
raw_ibovespa <- tidyquant::tq_get(
  api_yahoo$api_ibov,
  get  = "stock.prices",
  from = "2007-01-02"
  )

# Consumer Price Index - IPCA (accumulated variation in 12 months)
raw_ipca_12m <- sidrar::get_sidra(api = api_sidra$api_ipca) %>%
  select(
    date     = "Mês (Código)",
    variable = "Variável",
    value    = "Valor"
    )




# Data wrangling ----------------------------------------------------------


# This section performs data wrangling


# Interest Rate (SELIC target), short-term, % p.y.
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
  pull(value) %>%
  paste0("%")


# Inflation expectations (IPCA)
inflation_expec <- raw_inflation_expec %>%
  filter(basis == "0") %>%
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
    date_my = format(date, "%b %Y"),
    date    = format(date, "%Y/%m/%d"),
    .after  = "date"
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
    date_my = format(date, "%b %Y"),
    date    = format(date, "%Y/%m/%d"),
    .after  = "date"
    )


# Real interest rates (ex-ante and ex-post)
real_interest_rate <- bind_rows(

  # Ex-ante
  {
    inner_join(
      raw_swaps,
      raw_inflation_next_12m,
      by = "date"
      ) %>%
      select(
        date,
        swaps              = value.x,
        inflation_next_12m = value.y
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

  )


# Exchange rate (currency/R$)
currency <- raw_currency %>%
  group_by(
    symbol,
    date = format(date, "%Y-%m-01")
    ) %>%
  summarise(value = mean(ask)) %>%
  mutate( # functions from /R/utils.R
    mom  = (value / dplyr::lag(value) - 1) * 100,
    qyoy = (roll_sum_k(value, 3) / roll_sum_k(dplyr::lag(value, 12), 3) - 1) * 100,
    ytd  = (value / dplyr::lag(value, 12) - 1) * 100,
    yoy  = (roll_sum_k(value, 12) / roll_sum_k(dplyr::lag(value, 12), 12) - 1) * 100,
    across(where(is.numeric), ~round(., 2))
    ) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  left_join(
    api_bcb$currencies,
    by = c("symbol" = "symbol")
    ) %>%
  arrange(order(api_bcb$currencies$symbol)) %>%
  select(8, 3, 4:7) %>%
  rename_with(
    ~c("Currency",
       paste0("Exchange rate (", format(max(raw_currency$date), "%b %Y)")),
       "MoM %",
       "QoQ %",
       "YTD %",
       "YoY %"
       )
    )

# Footnote
footnote_currency <- paste0(
  "Note: average monthly exchange rate, updated to ",
  format(max(raw_currency$date), "%B %d, %Y.")
  )


# Annual market expectations for the current year's interest rate (Focus/BCB)
selic_expec <- raw_selic_expec %>%
  select(
    date,
    variable = indicator,
    detail,
    reference_date,
    value = mean
    ) %>%
  timetk::condense_period(
    .date_var = date,
    .period   = "1 month",
    .side     = "end"
    ) %>%
  mutate(
    date_my  = format(date, "%b %Y"),
    date     = format(date, "%Y/%m/01"),
    variable = recode(variable, "Meta para taxa over-selic" = "SELIC Target rate"),
    detail   = recode(detail, "Fim do ano" = "End of the year")
    )


# Current yield curve (ETTJ/Anbima)
ettj <- raw_ettj %>%
  filter(type == "real_return") %>%
  mutate(
    date_query    = format(current.date, "%B %d, %Y"),
    date_ref      = format(ref.date, "%Y/%m/%d"),
    date_ref_full = format(ref.date, "%B %d, %Y"),
    business_days = as.character(n.biz.days),
    value         = round(value, 2),
    variable      = "Yield curve (ETTJ IPCA)"
    ) %>%
  select(date_ref, business_days, date_ref_full, date_query, variable, value)


# Ibovespa index (B3)
ibovespa <- raw_ibovespa %>%
  timetk::condense_period(
    .date_var = date,
    .period   = "1 month",
    .side     = "end"
    ) %>%
  mutate(
    date_my = format(date, "%b %Y"),
    date = format(date, "%Y/%m/01")
    )


# EMBI+ Risk-Brasil - daily - JP Morgan
embi <- raw_embi %>%
  timetk::condense_period(
    .date_var = date,
    .period   = "1 month",
    .side     = "end"
    ) %>%
  mutate(
    date_my  = format(date, "%b %Y"),
    date     = format(date, "%Y/%m/01"),
    variable = "EMBI+ Risk-Brasil"
    ) %>%
  select(date, date_my, variable, value)




# Save data ---------------------------------------------------------------


# Aggregate data
imported_data_monetary <- mget(ls(pattern = "raw_|api_"))


# Remove unnecessary objects
rm(list  = c(lsf.str(), ls(pattern = "raw_|api_")),  # remove function objects
   envir = .GlobalEnv)


# Save RDATA file
save.image(file = file.path(file.path("./data"), "monetary.Rdata"))

