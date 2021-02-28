### Fiscal Policy ###


# This R code provides the extraction and data wrangling of macroeconomic
# variables referring to the Brazilian fiscal policy.


# Packages ----------------------------------------------------------------


# Install/load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  "readxl", 
  "tidyverse",
  "sidrar",
  "GetBCBData",
  "zoo",
  "janitor", 
  "rio",
  "deflateBR", 
  "lubridate"
  )


# Set the default language of date in R
Sys.setlocale("LC_TIME", "English")



# Parameters --------------------------------------------------------------


# Parameters used in the code to import, download or cleaning data


# Load useful functions
source("./R/utils.R")


# Disable scientific notation
options(scipen = 999)


# List of URLs to get data from different sources
url_list <- list(
  
  # Central Government Primary Balance (URL to download spreadsheet data from National Treasury)
  url_treasury = "http://sisweb.tesouro.gov.br/apex/cosis/thot/link/rtn/serie-historica?conteudo=cdn",
  
  
  # General government net and gross debt (URL to download spreadsheet data from Central Bank)
  url_debt = "https://www.bcb.gov.br/content/estatisticas/Documents/Tabelas_especiais/Divggnp.xls",
  
  
  # Federal Public Debt stock (URL to download spreadsheet data from National Treasury)
  url_debt_stock = "https://www.tesourotransparente.gov.br/ckan/dataset/0998f610-bc25-4ce3-b32c-a873447500c2/
resource/0402cb77-5e4c-4414-966f-0e87d802a29a/download/2.1.xlsx",
  
  
  # Debt Risk Rating History (URL to download spreadsheet data from National Treasury)
  url_rating = "https://sisweb.tesouro.gov.br/apex/f?p=2810:2::CSV:NO:RP::"
  
  )


# List of parameters to get data from SIDRA/IBGE website
api_sidra <- list(
  
  # Consumer Price Index - IPCA
  api_ipca_index = "/t/1737/n1/all/v/2266/p/all/d/v2266%2013"
  
  )


# List of parameters to get data from Central Bank
api_bcb <- list(
  
  # Inflation-targeting
  api_gdp_12m = c("Accumulated GDP in the last 12 months" = 4382)
  
  )
  

# Central Government Fiscal Balance (accounts)
balance_accounts <- tibble(
  group_1 = c(
    rep("Total Revenue", 21),
    rep("Transfers by Revenue Sharing", 7),
    rep("Total Expenditure", 29)
    ),
                     
  group_2 = c(
    rep("Revenues Collected by the Federal Revenue Office", 10),
    "Fiscal Incentives",
    "Net Social Security Revenues",
    rep("Revenues Not Collected by the Federal Revenue Office", 9),
    "FPM / FPE / IPI-EE",
    rep("Constitutional Funds", 2),
    "Education-Salary (social contribution for education)",
    "Exploitation of Natural Reosurces", 
    "CIDE - Fuels",
    "Other", 
    "Social Security Benefits",
    "Payroll", 
    rep("Other Compulsory Expenses", 25),
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
    "FGTS Complement (LC nº 110/01)",
    "Assets Operations",
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
    "Financial aid to Energy Development Account (CDE)",
    "Reparations and Special Legislation Benefits",
    "Assistance Benefits (LOAS/RMV)",
    "FGTS Complement (LC nº 110/01)",
    "Extraordinary credits (excluding PAC)",
    "Compensation to the Social Security Fund (RGPS) due to the payroll tax reduction",
    "Covenants", 
    "Donations", 
    "Bills and Coins Manufacturing", 
    "Fundef/Fundeb (Federal Complementation)",
    "Federal District (DF) Contitucional Fund (Current and Capital)",
    "Regional Development Funds for the Amazon (FDA) and the Northeast (FDNE)",
    "Legislative/Judiciary/Public Prosecutor/Public Defendant (Current and Capital)",
    "Kandir Law (LC nº 87/96 e 102/00) and FEX",
    "Contingency Reserve",
    "Reimbursement States/Municipalities Fossil Fuels",
    "Judicial Remedies (Current and Capital)",
    "Subsidies and Grants",
    "ANA (National Water Agency) Transfers", 
    "ANEEL (Electric Energy National Agency) Transfers and Fines", 
    "FIES primary impact (Student Funding)",
    "Electoral Campaign Funding", 
    "Compulsory Expenses with Cash Control", 
    "Discretionary"
    )
  )



# Import data -------------------------------------------------------------


# This section performs the import of data from different sources


# Central Government Primary Balance
download.file(
  url      = url_list$url_treasury,
  destfile = "./data/treasury.xlsx",
  mode     = "wb"
  )
raw_treasury <- read_xlsx(
  path      = "./data/treasury.xlsx",
  sheet     = "1.1-A",
  col_names = FALSE,
  skip      = 5,
  n_max     = 74
  ) %>%
  t() %>%
  as_tibble() %>% 
  clean_names()


# Accumulated GDP in the last 12 months - Current values (R$ million)
raw_gdp_monthly <- gbcbd_get_series(
  id         = api_bcb$api_gdp_12m,
  first.date = "1997-01-01"
  )


# General government net and gross debt
download.file(
  url      = url_list$url_debt,
  destfile = "./data/debt.xls",
  mode     = "wb"
  )
raw_debt <- read_excel(
  path      = "./data/debt.xls",
  sheet     = "R$ milhões",
  skip      = 8,
  col_names = FALSE,
  n_max     = 48
  ) %>%
  clean_names()


# Consumer Price Index (IPCA/IBGE)
raw_ipca_index <- get_sidra(api = api_sidra$api_ipca_index)$Valor %>% 
  ts(
    start     = c(1979, 12), 
    frequency = 12
    ) %>%
  window(start = c(2006, 12))


# Federal Public Debt stock (R$ billion)
download.file(
  url      = url_list$url_debt_stock,
  destfile = "./data/dpf.xlsx",
  mode     = "wb"
  )
raw_debt_stock <- read_excel(
  path      = "./data/dpf.xlsx",
  skip      = 4,
  col_names = FALSE
  )


# Debt Risk Rating History
download.file(
  url      = url_list$url_rating,
  destfile = "./data/rating.csv",
  mode     = "wb"
  )
raw_rating <- import("./data/rating.csv")




# Data wrangling ----------------------------------------------------------


# This section performs data wrangling


# Central Government Primary Balance
treasury <- raw_treasury %>%
  row_to_names(
    row_number = 1,
    remove_row = TRUE
    ) %>%
  mutate(
    across(where(is.character), as.numeric),
    date = seq(
      from   = as.Date("1997/01/01"),
      length = nrow(raw_treasury)-1,
      by     = "months"
      ) %>% format("%Y/%m/%d")
    ) %>%
  drop_na()


# Central Government Primary Balance (accumulated in 12 months + GDP)
treasury_accum_12m <- treasury %>%
  mutate(
    across(!date, ~accum_k(., k = 12)) # function from /R/utils.R
    ) %>%
  left_join(
    raw_gdp_monthly %>%
      mutate(date = format(ref.date, "%Y/%m/%d")) %>%
      select(date, gdp_accum_12m = value),
    by = "date"
    ) %>%
  drop_na()


# Central Government Primary Balance accumulated in 12 months (% GDP)
treasury_accum_12m_gdp <- treasury_accum_12m %>%
  mutate(
    across(!c(75:76), ~(. / gdp_accum_12m*100))
    )


# Primary Deficit accumulated in 12 months (% GDP)
primary_deficit <- treasury_accum_12m_gdp %>%
  select(date, value = 68) %>%
  mutate(id = "Primary Deficit")


# Revenues and spending
revenue_spending <- treasury_accum_12m_gdp %>%
  select(
    date, 
    `Net Revenue`       = 34, 
    `Total Expenditure` = 35
    ) %>%
  pivot_longer(
    cols      = -date, 
    names_to  = "variable", 
    values_to = "value"
    )


# Detailed revenues and spending for the last period
revenue_spending_detail <- treasury_accum_12m %>%
  slice_tail(n = 1) %>%
  select(-c(1,2,15,25,27,34,35,38,64,67:74,76)) %>%
  pivot_longer(
    cols      = -date, 
    names_to  = "variable", 
    values_to = "value"
    ) %>%
  select(-variable) %>%
  bind_cols(balance_accounts) %>%
  mutate(date = as.yearmon(date, format = "%Y/%m/%d"))


# General government net and gross debt
public_debt <- raw_debt %>%
  mutate(across(!1, as.numeric))


# General government net and gross debt (deflated)
public_debt_deflated <- as_tibble(
  t(public_debt), 
  rownames = "row_names"
  ) %>%
  row_to_names(1) %>%
  clean_names() %>%
  select(!1) %>%
  mutate(
    date = seq(
      from   = as.Date("2006/12/01"), 
      length = nrow(.), 
      by     = "months"
      ),
    across(!date, as.numeric),
    across(
      !date,
      ~deflate(
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
    value    =  disponibilidades_do_governo_federal_no_bacen*-1,
    date     = format(date, "%Y/%m/%d"),
    variable = "Single Account balance"
    ) %>%
  select(!1)


# Federal Public Debt stock (R$ billion deflated)
debt_stock <- as_tibble(
  t(raw_debt_stock[c(1,3),-1])
  ) %>%
  mutate(
    date     = myd(
      paste0(V1, "/01"),
      locale = "Portuguese_Brazil.1252"
      ),
    value    = as.numeric(V2),
    variable = "Debt Stock",
    across(
      value,
      ~deflate(
        nominal_values = .x, 
        nominal_dates  = date, 
        real_date      = format(last(date), "%m/%Y"), 
        index          = "ipca"
        )
      ),
    date     = format(date, "%Y/%m/%d")) %>%
  select(date, value, variable) %>%
  filter(date >= "2006/12/01")


# Debt Risk Rating History
rating  <- raw_rating %>%
  rename_with(~c("Last update", "Agency", "Foreign currency", "Local currency", "Action")) %>% 
  mutate(
    `Foreign currency` = gsub("\\-", "--", `Foreign currency`),
    `Local currency`   = gsub("\\-", "--", `Local currency`)
    )


# Government Securities Portfolio
gov_portfolio <- t(raw_debt_stock) %>%
  as_tibble() %>%
  select(1, 7:10, 12:15) %>%
  row_to_names(row_number = 1) %>%
  rename("date" = 1) %>%
  mutate(
    date = myd(
      paste0(date, "/01"),
      locale = "Portuguese_Brazil.1252"
      )
    ) %>% 
  pivot_longer(
    cols      = -date, 
    names_to  = "id", 
    values_to = "value"
    ) %>%
  mutate(
    value   = as.numeric(value)*1000,
    date_my = paste(
      month(date, label = TRUE), 
      year(date), 
      sep = " "
      )
    )




# Save data ---------------------------------------------------------------


# Aggregate data
imported_data_fiscal <- mget(ls(pattern = "raw_|api_|url_"))


# Remove unnecessary objects
rm(list  = c(lsf.str(), ls(pattern = "raw_|api_|url_")),  # remove function objects
   envir = .GlobalEnv)


# Save RDATA file
save.image(file = file.path(file.path("./data"), "fiscal.Rdata"))

