### Monetary policy ###


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
  "quantmod",
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
api_bcb <- list(
  
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
  
  # Currency rates (symbols)
  currencies = c("USD", "EUR", "ARS", "MXN", "CNY", "TRY", "RUB", "INR", "SAR", "ZAR") %>% 
      purrr::set_names(.)
  
  )


# List of parameters to get data from IPEADATA (API)
api_ipeadata <- list(
  
  # EMBI+ Risco-Brasil - daily - JP Morgan
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
raw_interest_rate <- gbcbd_get_series(
  id          = api_bcb$api_interest_rate,
  first.date  = "2001-11-07",
  use.memoise = FALSE
  )

# Inflation's market expectations for the next 12 months
raw_inflation_expec <- get_inflation_12m(
  indicator   = api_bcb$api_inflation_expec,
  first_date  = NULL,
  smoothed    = "yes",
  use_memoise = FALSE
  )

# SELIC annual market expectations
raw_selic_expec <- get_annual(
  indicator      = api_bcb$api_selic_expec,
  first_date     = NULL,
  detail         = "Fim do ano",
  reference_date = format(Sys.Date(), "%Y"),
  use_memoise    = FALSE
  )

# Currency rates
raw_currency <- map_dfr(
  api_bcb$currencies,
  ~rbcb::get_currency(.x, as = "tibble", start_date = Sys.Date()-3*365, end_date = Sys.Date()),
  .id = "currencies"
  )

# EMBI+ Risco-Brasil
raw_embi <- ipeadatar::ipeadata(api_ipeadata$api_embi)

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
raw_ipca_12m <- get_sidra(api = api_sidra$api_ipca) %>%
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
      drop_na() %>%
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



#### CONTINUE HERE...


# Tabela Mercado Cambial
moedas_nomes <- tibble(symbol = c("USD", "EUR", "ARS", "MXN", "CNY", "TRY", "RUB", "INR", "SAR", "ZAR"),
                       names = c('Dólar Americano', 'Euro', 'Peso Argentino', 'Peso Mexicano', 'Renminbi Chinês', 'Lira Turca', 'Rublo Russo', 'Rupia Indiana', 'Rial Saudita', 'Rand Sul-africano'))

moedas_nomes_tabela <- c("Moeda", paste0("Cotação (", format(tail(raw_currency[[2]][[1]]$date, 1), format = "%b/%y"), ")"), 'Mensal (%)', 'Trimestral (%)', 'Interanual (%)', '12 meses (%)')

moedas <- raw_currency %>%
  unnest(dados) %>%
  group_by(symbol) %>%
  tibbletime::as_tbl_time(index = date) %>%
  tibbletime::collapse_by(period = "monthly") %>%
  as_tibble() %>%
  group_by(symbol, date) %>%
  summarise(ask_monthly = mean(ask)) %>%
  mutate(previous = lag(ask_monthly, 12), 
         mom = (ask_monthly/lag(ask_monthly)-1)*100,
         qyoy = (rollsum(ask_monthly, 3, fill = "NA", align = "right") / 
                  rollsum(previous, 3, fill = "NA", align = "right")-1)*100,
         myoy = (ask_monthly/lag(ask_monthly, 12)-1)*100,
         yoy = (rollsum(ask_monthly, 12, fill = "NA", align = "right") / 
                  rollsum(previous, 12, fill = "NA", align = "right")-1)*100) %>%
  group_by(symbol) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate_if(is.numeric, round, 2) %>%
  left_join(moedas_nomes, by = "symbol") %>%
  arrange(order(moedas_nomes$symbol)) %>%
  select(9, 3, 5:8) %>%
  rename_all(~moedas_nomes_tabela)

moedas_footnote <- paste0("Nota: valores a partir de dados diários mensalizados, atualizado até ",
                         paste0(format(tail(raw_currency[[2]][[1]]$date, 1), format = "%d/%b/%Y")),
                         ".")


# Box Expectativas de Juros (Focus)
selic_expec <- raw_selic_expec %>%
  filter(indic_detail == "Fim do ano" & reference_year == format(Sys.Date(), "%Y")) %>%
  select(date, indic, indic_detail, reference_year, mean) %>%
  mutate(date = format(date, format = "%Y/%m/%d")) %>%
  group_by(month = month(date), year = year(date)) %>%
  slice(which.max(day(date))) %>%
  ungroup() %>%
  mutate(mes_ano = paste(month(date, label = TRUE), year(date), sep = " "),
         date = paste0(format(as.Date(date), format = "%Y/%m"), "/27")) %>%
  select(-month, -year)


# Box ETTJ
ettj <- raw_ettj %>%
  mutate(data_consulta = paste(day(as.Date(current.date)),
                               month(as.Date(current.date), label = TRUE),
                               year(as.Date(current.date)), sep = " "),
         data_ref = format(as.Date(ref.date), format = "%Y/%m/%d"),
         data_dmy = paste(day(as.Date(data_ref)),
                          month(as.Date(data_ref), label = TRUE),
                          year(as.Date(data_ref)), sep = " "),
         n.biz.days = as.character(n.biz.days),
         value = round(value, 2),
         id = "ETTJ IPCA") %>%
  filter(type == "real_return") %>%
  select(data_consulta, data_ref, valor = value, dias_uteis = n.biz.days, id, data_dmy)
  

# Box Ibovespa
ibov <- tibble(periodo = as.Date(time(raw_ibovespa)),
               pontos = as.numeric(raw_ibovespa$BVSP.Close),
               id = "IBOVESPA") %>%
  mutate(periodo = format(periodo, format = "%Y/%m/%d")) %>%
  group_by(month = month(periodo),
           year = year(periodo)) %>%
  slice(which.max(day(periodo))) %>%
  ungroup() %>%
  mutate(mes_ano = paste(month(periodo, label = TRUE), year(periodo), sep = " "),
         periodo = paste0(format(as.Date(periodo), format = "%Y/%m"), "/27", sep = "")) %>%
  select(-month, -year)


# Box Risco-País (EMBI+)
embi <- raw_embi %>%
  mutate(data = format(data, format = "%Y/%m/%d"),
         id = "EMBI+ Risco-Brasil") %>%
  group_by(month = month(data), year = year(data)) %>%
  slice(which.max(day(data))) %>%
  ungroup() %>%
  mutate(mes_ano = paste(month(data, label = TRUE), year(data), sep = " "),
         data = paste0(format(as.Date(data), format = "%Y/%m"), "/27", sep = "")) %>%
  select(-month, -year)



# Exportar dados ----------------------------------------------------------

diretorio <- file.path("../data")
save.image(file = file.path(diretorio, "monetaria.Rdata"))
