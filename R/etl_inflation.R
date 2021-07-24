#' ETL Inflation
#'
#' @encoding UTF-8
#' @import dplyr
#' @importFrom utils lsf.str
#' @return RDATA
#' @export
#'
etl_inflation <- function(){

### Inflation ###


# This R code provides the extraction and data wrangling of macroeconomic
# variables referring to the Brazilian inflation.


# Packages ----------------------------------------------------------------


# Install/load packages
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(
#   "sidrar",
#   "tidyverse",
#   "lubridate",
#   "GetBCBData",
#   "janitor"
#   )


# Set the default language of date in R
Sys.setlocale("LC_TIME", "English")



# Parameters --------------------------------------------------------------


# Parameters used in the code to import, download or cleaning data


# Load useful functions
# source("./R/utils.R")


# List of parameters to get data from SIDRA/IBGE website
api_sidra <- list(

  # Consumer Price Index - IPCA (parameters from SIDRA/IBGE website)
  api_ipca = "/t/1737/n1/all/v/63,69,2263,2264,2265/p/all/d/v63%202,v69%202,v2263%202,v2264%202,v2265%202",

    # Consumer Price Index (groups) - IPCA (parameters from SIDRA/IBGE website)
  api_ipca_groups = "/t/7060/n1/all/v/63,69/p/all/c315/7169,7170,7445,7486,7558,7625,7660,7712,7766,7786/d/v63%202,v69%202",

    # Consumer Price Index by Metropolitan Region - IPCA (parameters from SIDRA/IBGE website)
  api_ipca_region = "/t/7060/n7/all/v/63/p/all/c315/7169/d/v63%202"

  )


# List of parameters to get data from Central Bank
api_bcb <- list(

  # Inflation-targeting
  api_ipca_target = c(
    "Inflation target" = 13521
    ),

  # Diffusion index - Consumer Price Index (IPCA)
  api_ipca_diffusion = c(
    "Diffusion index (CPI)" = 21379
    ),

  # CPI cores - IPCA
  api_ipca_cores = c(
    "IPCA-EX0" = 11427,
    "IPCA-EX1" = 16121,
    "IPCA-DP"  = 16122,
    "IPCA-MA"  = 11426,
    "IPCA-MS"  = 4466
    ),

  # General price index (IGP/FGV)
  api_igp = c(
    "IGP-M"  = 189,
    "IGP-DI" = 190,
    "IGP-10" = 7447
    )

  )



# Import data -------------------------------------------------------------


# This section performs the import of data from different sources


# Inflation-targeting
raw_ipca_target <- GetBCBData::gbcbd_get_series(
  id          = api_bcb$api_ipca_target,
  first.date  = "1999-01-01",
  use.memoise = FALSE
  )


# Consumer Price Index - IPCA
raw_ipca <- sidrar::get_sidra(api = api_sidra$api_ipca)


# Consumer Price Index by groups - IPCA
raw_ipca_groups <- sidrar::get_sidra(api = api_sidra$api_ipca_groups) %>%
  select(
    date     = "M\u00eas (C\u00f3digo)",
    variable = "Vari\u00e1vel",
    group    = "Geral, grupo, subgrupo, item e subitem",
    value    = "Valor"
    )


# Consumer Price Index by Metropolitan Region - IPCA
raw_ipca_region <- sidrar::get_sidra(api = api_sidra$api_ipca_region) %>%
  select(
    date     = "M\u00eas (C\u00f3digo)",
    variable = "Vari\u00e1vel",
    region   = "Regi\u00e3o Metropolitana",
    value    = "Valor"
    )


# Diffusion index - Consumer Price Index (IPCA)
raw_ipca_diffusion <- GetBCBData::gbcbd_get_series(
  id          = api_bcb$api_ipca_diffusion,
  first.date  = "2001-01-01",
  use.memoise = FALSE
  )


# CPI cores - IPCA
raw_ipca_cores <- GetBCBData::gbcbd_get_series(
  id          = api_bcb$api_ipca_cores,
  use.memoise = FALSE
  )


# General price index (IGP/FGV)
raw_igp <- GetBCBData::gbcbd_get_series(
  id          = api_bcb$api_igp,
  first.date  = "1994-09-01",
  use.memoise = FALSE
  )




# Data wrangling ----------------------------------------------------------


# This section performs data wrangling


# Consumer Price Index - IPCA
ipca <- raw_ipca %>%
  select(
    date     = "M\u00eas (C\u00f3digo)",
    variable = "Vari\u00e1vel",
    value    = "Valor"
    ) %>%
  mutate(
    variable = recode(
      variable,
      "IPCA - Varia\u00e7\u00e3o mensal"                = "Month over Month (%)",
      "IPCA - Varia\u00e7\u00e3o acumulada em 12 meses" = "Year over Year (%)",
      "IPCA - Varia\u00e7\u00e3o acumulada no ano"      = "Year to Date (%)",
      "IPCA - Varia\u00e7\u00e3o acumulada em 3 meses"  = "Quarter to Date (%)",
      "IPCA - Varia\u00e7\u00e3o acumulada em 6 meses"  = "Semester to Date (%)"
      ),
    date = lubridate::ym(date)
    ) %>%
  tidyr::drop_na()


# Consumer Price Index by groups - IPCA
ipca_groups <- raw_ipca_groups %>%
  filter(date == max(date)) %>%
  mutate(
    Group = recode(
      group,
      "\u00cdndice geral"           = "CPI (IPCA)",
      "1.Alimenta\u00e7\u00e3o e bebidas"     = "Food and drink",
      "2.Habita\u00e7\u00e3o"                 = "Housing",
      "3.Artigos de resid\u00eancia"     = "Residence articles",
      "4.Vestu\u00e1rio"                 = "Clothing",
      "5.Transportes"               = "Transportation",
      "6.Sa\u00fade e cuidados pessoais" = "Health care",
      "7.Despesas pessoais"         = "Personal expenses",
      "8.Educa\u00e7\u00e3o"                  = "Education",
      "9.Comunica\u00e7\u00e3o"               = "Communication"
      ),
    variable = recode(
      variable,
      "IPCA - Varia\u00e7\u00e3o mensal"           = "MoM",
      "IPCA - Varia\u00e7\u00e3o acumulada no ano" = "YTD"
      )
    ) %>%
  select(-date) %>%
  pivot_wider(
    names_from  = "variable",
    values_from = "value",
    id_cols     = "Group"
    )


# Footnote for Consumer Price Index by groups - IPCA
footnote_ipca_groups <- raw_ipca_groups %>%
  filter(date == max(date)) %>%
  slice_tail(n = 1) %>%
  mutate(
    date = paste0(
      "Note: data for ",
      format(lubridate::ym(date), "%B %Y"),
      "."
      )
    ) %>%
  pull(date)


# Consumer Price Index by Metropolitan Region - IPCA
ipca_region <- raw_ipca_region %>%
  mutate(
    date = lubridate::ym(date),
    variable = recode(
      variable,
      "IPCA - Varia\u00e7\u00e3o mensal" = "Monthly percentage change"
      )
    ) %>%
  filter(region %in% c(
    "Bel\u00e9m - PA", "Fortaleza - CE", "Rio de Janeiro - RJ",
    "S\u00e3o Paulo - SP", "Curitiba - PR", "Porto Alegre - RS")
    )


# Consumer Price Index (Year over Year (%)) - IPCA
ipca_yoy <- ipca %>%
  filter(
    date == max(date) & variable == "Year over Year (%)"
    ) %>%
  mutate(value = paste0(value, "%"))


# Inflation-targeting (current year)
inflation_target <- raw_ipca_target %>%
  janitor::clean_names() %>%
  mutate(date = format(ref_date, "%Y")) %>%
  filter(date == format(Sys.time(), "%Y")) %>%
  select(date, id = series_name, value)


# Consumer Price Index (trend) - IPCA
ipca_trend <- ipca %>%
  filter(
    variable %in% c("Month over Month (%)", "Year over Year (%)")
    ) %>%
  pivot_wider(
    id_cols = date,
    names_from = variable,
    values_from = value
    ) %>%
  tidyr::drop_na()


# Diffusion index - Consumer Price Index (IPCA)
ipca_diffusion <- raw_ipca_diffusion %>%
  clean_inflation_bcb() # function from /R/utils.R


# CPI cores - IPCA
ipca_cores <- raw_ipca_cores %>%
  clean_inflation_bcb() # function from /R/utils.R


# General price index (IGP/FGV)
igp <- raw_igp %>%
  clean_inflation_bcb() # function from /R/utils.R




# Save data ---------------------------------------------------------------


if (0L %in% purrr::map_dbl(mget(ls()), length)) {

  stop("Some objects are zero in length.", call. = FALSE)

  } else
    {
    # Aggregate data
    imported_data_inflation <- mget(ls(pattern = "raw_|api_"))


    # Remove unnecessary objects
    rm(
      list  = c(lsf.str(), ls(pattern = "raw_|api_")),
      envir = environment()
      )


    # Save RDATA file
    save(
      list  = ls(),
      file  = file.path(file.path("./inst/extdata"), "inflation.Rdata"),
      envir = environment()
      )
    }

}
