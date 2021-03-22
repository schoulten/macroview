#' ETL Economic activity
#'
#' @encoding UTF-8
#' @import dplyr tidyr
#' @return RDATA
#' @export
#'
etl_econ_activity <- function(){

### Economic activity ###


# This R code provides the extraction and data wrangling of macroeconomic
# variables referring to the Brazilian economic activity.


# Packages ----------------------------------------------------------------


# Install/load packages
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(
#   "tidyverse",
#   "sidrar",
#   "readxl",
#   "lubridate",
#   "GetBCBData",
#   "zoo",
#   "mgsub",
#   "janitor"
#   )



# Parameters --------------------------------------------------------------


# Parameters used in the code to import, download or cleaning data


# Load useful functions
# source("./R/utils.R")


# List of URLs to get data from different sources
url_list <- list(

  # ICVA (spreadsheet data of the Cielo Index)
  url_icva = "https://apicatalog.mziq.com/filemanager/v2/d/4d1ebe73-b068-4443-992a-3d72d573238c/3e864198-0b72-c970-1771-80cd8c338a30?origin=2",

  # Vehicle Production (spreadsheet data from ANFAVEA)
  url_anfavea = "http://www.anfavea.com.br/docs/SeriesTemporais_Autoveiculos.xlsm"

)


# List of parameters to get data from SIDRA/IBGE website
api_sidra <- list(

  # GDP (percent changes)
  api_gdp = "/t/5932/n1/all/v/all/p/all/c11255/90687,90691,90696,90707,93404,93405,93406,93407,93408/d/v6561%201,v6562%201,v6563%201,v6564%201",

  # GDP (R$ million, current prices)
  api_gdp_brl = "/t/1846/n1/all/v/all/p/all/c11255/90687,90691,90696,90707,93404,93405,93406,93407,93408/d/v585%200",

  # PMC (retail trade from IBGE)
  api_pmc = "/t/3416/n1/all/v/all/p/all/c11046/90668/d/v564%201,v565%201",

  # PMC (expanded retail trade from IBGE)
  api_pmc_expanded = "/t/3417/n1/all/v/all/p/all/c11046/90668/d/v1186%201,v1190%201",

  # PMS (Monthly Service Survey from IBGE)
  api_pms = "/t/6442/n1/all/v/all/p/all/c11046/90668/d/v8676%201,v8677%201",

  # PIM (Monthly Industrial Survey from IBGE - YoY rate of change)
  api_pim = "/t/3653/n1/all/v/3139/p/all/c544/129314,129315,129316,129338/d/v3139%201"

  )


# List of parameters to get data from Central Bank
api_bcb <- list(

  # Installed Capacity Utilization Level (NUCI/FGV)
  api_nuci = c(
    "Installed Capacity Utilization Level" = 28561
    ),

  # IBC-Br (economic activity index from BCB)
  api_ibc = c(
    "Brasil"       = 24364,
    "Norte"        = 25407,
    "Nordeste"     = 25389,
    "Centro-Oeste" = 25382,
    "Sudeste"      = 25395,
    "Sul"          = 25403
    )

  )



# Import data -------------------------------------------------------------


# This section performs the import of data from different sources


# GDP growth (rate of change of the quarterly volume index from IBGE)
raw_gdp <- sidrar::get_sidra(api = api_sidra$api_gdp) %>%
  select(
    date     = "Trimestre (Código)",
    variable = "Variável",
    sector   = "Setores e subsetores",
    measure  = "Unidade de Medida",
    value    = "Valor"
  )


# GDP (values at current prices from IBGE)
raw_gdp_cur_prices <- sidrar::get_sidra(api = api_sidra$api_gdp_brl) %>%
  select(
    date    = "Trimestre (Código)",
    sector  = "Setores e subsetores",
    measure = "Unidade de Medida",
    value   = "Valor"
  )


# PMC (retail trade from IBGE)
raw_pmc <- sidrar::get_sidra(api = api_sidra$api_pmc) %>%
  select(
    date     = "Mês (Código)",
    variable = "Variável",
    measure  = "Unidade de Medida",
    value    = "Valor"
  )


# PMC (expanded retail trade from IBGE)
raw_pmc_expanded <- sidrar::get_sidra(api = api_sidra$api_pmc_expanded) %>%
  select(
    date     = "Mês (Código)",
    variable = "Variável",
    measure  = "Unidade de Medida",
    value    = "Valor"
  )


# PMS (Monthly Service Survey from IBGE)
raw_pms <- sidrar::get_sidra(api = api_sidra$api_pms) %>%
  select(
    date     = "Mês (Código)",
    variable = "Variável",
    measure  = "Unidade de Medida",
    value    = "Valor"
  )


# PIM (Monthly Industrial Survey from IBGE - YoY rate of change)
raw_pim <- sidrar::get_sidra(api = api_sidra$api_pim) %>%
  select(
    date     = "Mês (Código)",
    variable = "Seções e atividades industriais (CNAE 2.0)",
    measure  = "Unidade de Medida",
    value    = "Valor"
  )


# ICVA (Cielo)
download.file(
  url      = url_list$url_icva,
  destfile = "./data/icva.xlsx",
  mode     = "wb"
  )
raw_icva <- readxl::read_excel("./data/icva.xlsx")


# Vehicle Production (ANFAVEA)
download.file(
  url      = url_list$url_anfavea,
  destfile = file.path("./data", basename(url_list$url_anfavea)),
  mode     = "wb"
  )
raw_vehicle <- readxl::read_excel(
  path = file.path("./data", basename(url_list$url_anfavea)),
  skip = 4
  )


# Installed Capacity Utilization Level (NUCI/FGV)
raw_nuci <- GetBCBData::gbcbd_get_series(
  id          = api_bcb$api_nuci,
  first.date  = "2001-01-01",
  use.memoise = FALSE
  )


# IBC-Br (economic activity index from Central Bank of Brazil)
raw_ibc <- GetBCBData::gbcbd_get_series(
  id          = api_bcb$api_ibc,
  first.date  = "2003-01-27",
  use.memoise = FALSE
  )



# Data wrangling ----------------------------------------------------------


# This section performs data wrangling


# GDP Growth (percent changes)
gdp <- raw_gdp %>%
  mutate(
    variable = recode(
      variable,
      "Taxa trimestral (em relação ao mesmo período do ano anterior)" = "Year over Year (%)",
      "Taxa acumulada em quatro trimestres (em relação ao mesmo período do ano anterior)" = "Accumulated in 4 Quarters (%)",
      "Taxa acumulada ao longo do ano (em relação ao mesmo período do ano anterior)" = "Year to Date (%)",
      "Taxa trimestre contra trimestre imediatamente anterior" = "Quarter over Quarter (%)"
    ),
    sector = recode(
      sector,
      "Agropecuária - total"                        = "Agriculture",
      "Indústria - total"                           = "Industry",
      "Serviços - total"                            = "Services",
      "PIB a preços de mercado"                     = "GDP",
      "Despesa de consumo das famílias"             = "Private consumption",
      "Despesa de consumo da administração pública" = "Government spending",
      "Formação bruta de capital fixo"              = "Investment",
      "Exportação de bens e serviços"               = "Exports",
      "Importação de bens e serviços (-)"           = "Imports"
    ),
    date = lubridate::parse_date_time(
      date,
      orders = "Yq"
      ) %>%
      lubridate::quarter(with_year = TRUE)
  ) %>%
  arrange(date)


# GDP Growth (input for valueBox)
gdp_growth <- gdp %>%
  filter(
    sector   == "GDP" &
    variable == "Accumulated in 4 Quarters (%)"
  ) %>%
  filter(date == last(date))


# GDP, Current Prices (R$ trillions)
gdp_cur_prices <- raw_gdp_cur_prices %>%
  filter(sector == "PIB a preços de mercado") %>%
  arrange(date) %>%
  slice_tail(n = 4) %>%
  summarise(
    value = (sum(value) / 1e+06) %>% round(2)
    )


# Cielo Retail Index
icva <- raw_icva %>%
  rename_with(~c("date", "nominal", "nominal_sa", "real", "real_sa")) %>%
  mutate(
    value = real_sa*100, # convert to % format
    date  = lubridate::as_date(date),
    ) %>%
  filter(date == last(date)) %>%
  select(date, value)


# Vehicle Production
vehicle <- raw_vehicle %>%
  janitor::clean_names() %>%
  select(
    date  = x1,
    value = producao_5
    ) %>%
  mutate(date = lubridate::as_date(date)) %>%
  dplyr::na_if(0) %>%
  tidyr::drop_na()


# Installed Capacity Utilization Level (NUCI/FGV)
nuci <- raw_nuci %>%
  janitor::clean_names() %>%
  rename_with(~c("date", "value"), 1:2) %>%
  filter(date == last(date))


# Table of GDP measures

# Levels for factor column
levels_gdp <- c(
  "GDP",
  "Agriculture",
  "Industry",
  "Services",
  "Private consumption",
  "Government spending",
  "Investment",
  "Exports",
  "Imports"
  )

# Footnote
footnote_gdp <- gdp %>%
  slice_tail(n = 1) %>%
  mutate(
    note = paste0(
      "Note: updated until ",
      stringr::str_replace(date, "(\\d{4}).(\\d{1}$)", "\\1 Q\\2"),
      "."
      )
    ) %>%
  pull(note)


# Create the table
gdp_measures <- gdp %>%
  filter(date == max(date)) %>%
  pivot_wider(
    id_cols     = sector,
    names_from  = variable,
    values_from = value
    ) %>%
  relocate(1, 5, 2, 4, 3) %>%
  mutate(
    sector = factor(
      sector,
      levels = levels_gdp
      )
    ) %>%
  rename("Indicator" = sector) %>%
  arrange(Indicator)


# GDP growth by sectors
gdp_growth_sector <- gdp %>%
  filter(
    variable == "Accumulated in 4 Quarters (%)",
    sector %in% c(
      "Agriculture",
      "Industry",
      "Services",
      "GDP"
      )
    ) %>%
  select(date, sector, value)


# IBC-Br growth by region
ibc_growth <- raw_ibc %>%
  select(
    date     = ref.date,
    variable = series.name,
    value
    ) %>%
  group_by(variable) %>%
  mutate(
    yoy = yoy_growth_rate( # function from /R/utils.R
      col   = value,
      round = 2
      ),
    date = as.character(
      format(date, "%Y/%m/%d")
      )
  ) %>%
  tidyr::drop_na()


# PMC growth
pmc <- bind_rows(
  raw_pmc_expanded,
  raw_pmc
  ) %>%
  mutate(
    date     = ymd_sidra(date), # function from /R/utils.R
    variable = recode(
      variable,
      "Índice de volume de vendas no comércio varejista ampliado"          = "Retail sales volume expanded",
      "Índice de receita nominal de vendas no comércio varejista ampliado" = "Retail sales revenue expanded",
      "Índice de volume de vendas no comércio varejista"                   = "Retail sales volume",
      "Índice de receita nominal de vendas no comércio varejista"          = "Retail sales revenue"
      )
    ) %>%
  tidyr::drop_na()


# PMS growth
pms <- raw_pms %>%
  mutate(
    variable = recode(
      variable,
      "Índice de receita nominal de serviços" = "Revenue",
      "Índice de volume de serviços"          = "Volume"
      ),
    date     = ymd_sidra(date) # function from /R/utils.R
    ) %>%
  tidyr::drop_na()


# PIM growth
pim <- raw_pim %>%
  mutate(
    variable = recode(
      variable,
      "1 Indústria geral"                                               = "General industry",
      "2 Indústrias extrativas"                                         = "Extractive industries",
      "3 Indústrias de transformação"                                   = "Manufacturing industries",
      "3.29 Fabricação de veículos automotores, reboques e carrocerias" = "Manufacture of motor vehicles"
      ),
    date = ymd_sidra(date) # function from /R/utils.R
  ) %>%
  tidyr::drop_na()




# Save data ---------------------------------------------------------------


# Aggregate data
imported_data_eco_activity <- mget(ls(pattern = "raw_|api_|url_"))


# Remove unnecessary objects
rm(
  list  = c(lsf.str(), ls(pattern = "raw_|api_|url_")),
  envir = environment()
  )


# Save RDATA file
save(
  list  = ls(),
  file  = file.path(file.path("./data"), "econ_activity.Rdata"),
  envir = environment()
  )

}
