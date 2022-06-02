#' ETL Fiscal Policy
#'
#' @encoding UTF-8
#' @import dplyr
#' @importFrom utils lsf.str
#' @return RDATA
#' @export
#'
etl_fiscal <- function(){

### Fiscal Policy ###


# This R code provides the extraction and data wrangling of macroeconomic
# variables referring to the Brazilian fiscal policy.


# Packages ----------------------------------------------------------------


# Install/load packages
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(
#   "readxl",
#   "tidyverse",
#   "sidrar",
#   "GetBCBData",
#   "zoo",
#   "janitor",
#   "rio",
#   "deflateBR",
#   "lubridate"
#   )



# Parameters --------------------------------------------------------------


# Parameters used in the code to import, download or cleaning data


# Load useful functions
# source("./R/utils.R")


# Disable scientific notation
options(scipen = 999, digits = 15)


# List of URLs to get data from different sources
url_list <- list(

  # Central Government Primary Balance (URL to download spreadsheet data from National Treasury)
  url_treasury = "http://sisweb.tesouro.gov.br/apex/cosis/thot/link/rtn/serie-historica?conteudo=cdn",


  # General government net and gross debt (URL to download spreadsheet data from Central Bank)
  url_debt = "https://www.bcb.gov.br/content/estatisticas/Documents/Tabelas_especiais/Divggnp.xls",


  # Federal Public Debt stock (URL to download spreadsheet data from National Treasury)
  url_debt_stock = paste0(
    "https://www.tesourotransparente.gov.br/ckan/dataset/0998f610-bc25-4ce3-b32c",
    "-a873447500c2/resource/0402cb77-5e4c-4414-966f-0e87d802a29a/download/2.1.xlsx"
    ),


  # Debt Risk Rating History (URL to download spreadsheet data from National Treasury)
  url_rating = "https://sisweb.tesouro.gov.br/apex/f?p=2810:2::CSV:NO:RP::"

  )


# List of parameters to get data from Central Bank
api_bcb <- list(

  # Inflation-targeting
  api_gdp_12m = c("Accumulated GDP in the last 12 months" = 4382)

  )


# Central Government Fiscal Balance (accounts)
balance_accounts <- tibble(
  group_1 = c(
    rep("Total Revenue", 20),
    rep("Transfers by Revenue Sharing", 7),
    rep("Total Expenditure", 24)
    ),

  group_2 = c(
    rep("Revenues Collected by the Federal Revenue Office", 10),
    "Fiscal Incentives",
    "Net Social Security Revenues",
    rep("Revenues Not Collected by the Federal Revenue Office", 8),
    "FPM / FPE / IPI-EE",
    rep("Constitutional Funds", 2),
    "Education-Salary (social contribution for education)",
    "Exploitation of Natural Reosurces",
    "CIDE - Fuels",
    "Others",
    "Social Security Benefits",
    "Payroll",
    rep("Other Compulsory Expenses", 20),
    rep("Executive Branch Expenses Subject to Financial Programming", 2)
    ),

  group_3 = c(
    "Import Tax",
    "Industrialized Products Tax (IPI)",
    "Income tax (IR)",
    "Tax on Credit Operations, Exchange and Insurance (IOF)",
    "Contribution to Social Security Financing (COFINS)",
    "Contribution to the Social Integration Program and Civil Service Asset Formation Program (PIS/Pasep)",
    "Social Contribution on Net Corporate Profits (CSLL)",
    "Provisional Contribution on Financial Operations (CPMF)",
    "Contribution on Intervention in the Economic Domain (CIDE) - Fuels",
    "Other",
    "Fiscal Incentives",
    "Net Social Security Revenues",
    "Concessions and Permissions",
    "Dividends",
    "Contribution to Civil Service Social Security (CPSS)",
    "Exploitation of Natural Resources",
    "Own Revenues and from agreements",
    "Education-Salary (social contribution for education)",
    "FGTS Complement (LC n\u00ba 110/01)",
    "Other Revenues",
    "FPM / FPE / IPI-EE",
    "Total Transfer",
    "Funds Surplus",
    "Education-Salary (social contribution for education)",
    "Exploitation of Natural Reosurces",
    "CIDE - Fuels",
    "Other",
    "Social Security Benefits",
    "Payroll",
    "Salary Allowance and Unemployment Benefit",
    "Amnestied Workers",
    "Financial support to states and Municipalities",
    "Reparations and Special Legislation Benefits",
    "Assistance Benefits (LOAS/RMV)",
    "FGTS Complement (LC n\u00ba 110/01)",
    "Extraordinary credits (excluding PAC)",
    "Compensation to the Social Security Fund (RGPS) due to the payroll tax reduction",
    "Bills and Coins Manufacturing",
    "Fundeb (Federal Complementation)",
    "Federal District (DF) Contitucional Fund (Current and Capital)",
    "Legislative/Judiciary/Public Prosecutor/Public Defendant (Current and Capital)",
    "Kandir Law (LC n\u00ba 87/96 e 102/00) and FEX",
    "Judicial Remedies (Current and Capital)",
    "Subsidies, Grants and Proagro",
    "ANA (National Water Agency) Transfers",
    "ANEEL (Electric Energy National Agency) Transfers and Fines",
    "FIES primary impact (Student Funding)",
    "Electoral Campaign Funding",
    "Others",
    "Compulsory Expenses with Cash Control",
    "Discretionary"

    # "Regional Development Funds for the Amazon (FDA) and the Northeast (FDNE)",
    # "Contingency Reserve",
    # "Reimbursement States/Municipalities Fossil Fuels",
    #
    # "Covenants",
    # "Donations"
    )
  )



# Import data -------------------------------------------------------------


# This section performs the import of data from different sources


# Central Government Primary Balance
raw_treasury <- rio::import(
  file      = url_list$url_treasury,
  format    = "xlsx",
  sheet     = "1.1-A",
  col_names = FALSE,
  skip      = 5,
  n_max     = 68
  ) %>%
  t() %>%
  dplyr::as_tibble() %>%
  janitor::clean_names()


# Accumulated GDP in the last 12 months - Current values (R$ million)
raw_gdp_monthly <- GetBCBData::gbcbd_get_series(
  id          = api_bcb$api_gdp_12m,
  first.date  = "1997-01-01",
  use.memoise = FALSE
  )


# General government net and gross debt
raw_debt <- rio::import(
  file      = url_list$url_debt,
  format    = "xls",
  sheet     = "R$ milh\u00f5es",
  skip      = 8,
  col_names = FALSE,
  n_max     = 48
  ) %>%
  janitor::clean_names()


# Federal Public Debt stock (R$ billion)
raw_debt_stock <- rio::import(
  file      = url_list$url_debt_stock,
  format    = "xlsx",
  skip      = 4,
  col_names = FALSE
  )


# Debt Risk Rating History
raw_rating <- rio::import(
  file   = url_list$url_rating,
  format = "csv",
  )




# Data wrangling ----------------------------------------------------------


# This section performs data wrangling


# Central Government Primary Balance
treasury <- raw_treasury %>%
  janitor::row_to_names(
    row_number = 1,
    remove_row = TRUE
    ) %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    dplyr::across(where(is.character), as.numeric),
    date = seq(
      from   = as.Date("1997-01-01"),
      length = nrow(raw_treasury)-1,
      by     = "months"
      )
    ) %>%
  tidyr::drop_na()


# Central Government Primary Balance (accumulated in 12 months + GDP)
treasury_accum_12m <- treasury %>%
  mutate(
    across(!date, ~rolling(., sum, k = 12)) # function from /R/utils.R
    ) %>%
  left_join(
    raw_gdp_monthly %>%
      select(date = ref.date, gdp_accum_12m = value),
    by = "date"
    ) %>%
  tidyr::drop_na()


# Central Government Primary Balance accumulated in 12 months (% GDP)
treasury_accum_12m_gdp <- treasury_accum_12m %>%
  mutate(
    across(!c(69:70), ~(. / gdp_accum_12m * 100))
    )


# Primary Deficit accumulated in 12 months (% GDP)
primary_deficit <- treasury_accum_12m_gdp %>%
  select(date, value = 66) %>%
  mutate(
    value = round(value, 2),
    variable = "Primary Deficit"
    )


# Revenues and spending
revenue_spending <- treasury_accum_12m_gdp %>%
  select(
    date,
    `Net Revenue`       = 33,
    `Total Expenditure` = 34
    ) %>%
  pivot_longer(
    cols      = -date,
    names_to  = "variable",
    values_to = "value"
    ) %>%
  mutate(value = round(value, 2))


# Detailed revenues and spending for the last period
revenue_spending_detail <- treasury_accum_12m %>%
  slice_tail(n = 1) %>%
  select(-c(1:2,15,24,26,33:34,37,58,61:68,70)) %>%
  pivot_longer(
    cols      = -date,
    names_to  = "variable",
    values_to = "value"
    ) %>%
  select(-variable) %>%
  bind_cols(balance_accounts) %>%
  mutate(
    date = format(lubridate::ymd(date), "%B, %Y"),
    value = round(value, 2)
    )


# General government net and gross debt
public_debt <- raw_debt %>%
  mutate(across(!1, as.numeric))


# General government net and gross debt (deflated)
public_debt_deflated <- as_tibble(
  t(public_debt),
  rownames = "row_names"
  ) %>%
  janitor::row_to_names(1) %>%
  janitor::clean_names() %>%
  select(!1) %>%
  mutate(
    date = seq(
      from   = as.Date("2006-12-01"),
      length = nrow(.),
      by     = "months"
      ),
    across(!date, as.numeric),
    across(
      !date,
      ~deflateBR::deflate(
        nominal_values = .x,
        nominal_dates  = date,
        real_date      = format(last(date), "%m/%Y"),
        index          = "ipca"
        )
      )
    )


# Single Account balance
single_account <- public_debt_deflated[,c(31,49)] %>%
  mutate(
    value    =  (disponibilidades_do_governo_federal_no_bacen * -1) %>% round(2),
    variable = "Single Account balance"
    ) %>%
  select(!1)


# Federal Public Debt stock (R$ billion deflated)
withr::local_locale(c("LC_TIME" = "pt_BR.utf8"))
debt_stock <- t(raw_debt_stock[c(1,3),-1]) %>%
  dplyr::as_tibble() %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    date     = lubridate::myd(paste0(x1, "/01")),
    value    = as.numeric(x3),
    variable = "Debt Stock"
    ) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(
    value = deflateBR::deflate(
      nominal_values = value,
      nominal_dates  = date,
      real_date      = format(dplyr::last(date) - months(1), "%m/%Y"),
      index          = "ipca"
      ) %>% round(2)
    ) %>%
    dplyr::select(date, value, variable) %>%
    dplyr::filter(date >= "2006-12-01")


# Debt Risk Rating History
rating  <- raw_rating %>%
  rename_with(~c("Last update", "Agency", "Foreign currency", "Local currency", "Action")) %>%
  mutate(
    `Last update` = lubridate::dmy(`Last update`)
    )


# Government Securities Portfolio
gov_portfolio <- t(raw_debt_stock) %>%
  as_tibble() %>%
  select(1, 7:10, 12:15) %>%
  janitor::row_to_names(row_number = 1) %>%
  rename(
    "date"             = 1,
    "Securitized Debt" = "D\u00edvida Securitizada",
    "Others"           = "Demais"
    ) %>%
  mutate(
    date = lubridate::myd(paste0(date, "/01")),
    date_my = format(date, "%B, %Y"),
    across(2:9, as.numeric) %>% round(2)
    ) %>%
  filter(date == max(date)) %>%
  pivot_longer(
    cols      = -c(date, date_my),
    names_to  = "variable",
    values_to = "value"
    )




# Save data ---------------------------------------------------------------


if (0L %in% purrr::map_dbl(mget(ls()), length)) {

  stop("Some objects are zero in length.", call. = FALSE)

  } else
    {
    # Aggregate data
    imported_data_fiscal <- mget(ls(pattern = "raw_|api_|url_"))


    # Remove unnecessary objects
    rm(
      list  = c(lsf.str(), ls(pattern = "raw_|api_|url_")),
      envir = environment()
      )


    # Save RDATA file
    save(
      list  = ls(),
      file  = file.path(file.path("./inst/extdata"), "fiscal.Rdata"),
      envir = environment()
      )
    }

}
