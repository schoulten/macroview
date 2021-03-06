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

yoy_growth_rate = function(col, round) {
  
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




# Get BCB Expectations data (from Olinda/BCB) -----------------------------


# Function to extract data from Olinda/BCB "ExpectativasMercadoAnuais"

bcb <- function (
  indicator      = NULL, 
  detail         = NULL, 
  first_date     = Sys.Date() - 10 * 365, 
  last_date      = Sys.Date(),
  reference_date = NULL
  ){
  
  # Available indicators
  valid_indicator <- c(
    "Balança Comercial", 
    "Balanço de Pagamentos",
    "Fiscal", 
    "IGP-DI", 
    "IGP-M",
    "INPC", 
    "IPA-DI", 
    "IPA-M", 
    "IPCA", 
    "IPCA-15", 
    "IPC-FIPE",
    "Preços administrados por contrato e monitorados", 
    "Produção industrial",
    "PIB Agropecuária", 
    "PIB Industrial", 
    "PIB Serviços", 
    "PIB Total",
    "Meta para taxa over-selic",
    "Taxa de câmbio"
    )
  
  
  # Check if input "indicator" is valid
  if (missing(indicator) | !all(indicator %in% valid_indicator) | is.null(indicator)) {
    stop("\nArgument 'indicator' is not valid or missing. Check your inputs.", call. = FALSE)
  } else indicator
  
  
  # Available indicator details
  valid_detail <- c(
    "Balança Comercial / Exportações",
    "Balança Comercial / Importações",
    "Balança Comercial / Saldo",
    "Balanço de Pagamentos / Conta corrente",
    "Balanço de Pagamentos / Investimento direto no país",
    "Fiscal / Resultado Primário",
    "Fiscal / Resultado Nominal",
    "Fiscal / Dívida líquida do setor público",
    "Meta para taxa over-selic / Fim do ano",
    "Meta para taxa over-selic / Média do ano"
  )
  
  
  # Check if input "detail" is valid and get detail input (or NULL) if is valid
  if (!is.null(detail) && !is.na(detail)) {
    if ((class(detail) != "character")) {
      stop("\nArgument 'detail' is not valid. Check your inputs.", call. = FALSE)
    } else if 
    (!all(paste0(indicator, " / ", detail) %in% valid_detail)) {
      stop("\nArgument 'detail' is not valid. Check your inputs.", call. = FALSE)
    }
  } else if
  ((length(detail) > 0) && is.na(detail)) {
    detail <- NULL
  } else detail
   
  
  
  # Check if first_date argument is valid
  first_date <- try(as.Date(first_date), silent = TRUE)
  if (length(first_date) <= 0 || is.na(first_date)) {first_date = NULL}
  if (class(first_date) %in% "try-error") {
    stop("\nArgument 'first_date' is not a valid date.", call. = FALSE)
  }
  if (missing(first_date)) {
    first_date = Sys.Date() - 10 * 365
  } else first_date
  
  
  # Check if last_date argument is valid
  last_date <- try(as.Date(last_date), silent = TRUE)
  if (length(last_date) <= 0 || is.na(last_date)) {last_date = NULL}
  if (class(last_date) %in% "try-error") {
    stop("\nArgument 'last_date' is not a valid date.", call. = FALSE)
  }
  if (missing(last_date)) {
    last_date = Sys.Date() - 10 * 365
  } else last_date
  
  
  # Check if first_date > Sys.Date()
  if ((length(first_date) > 0) && first_date > Sys.Date()) {
    stop("\nIt seems that 'first_date' > current date. Check your inputs.", call. = FALSE)
  }
  
  
  # Check if last_date < first_date
  if ((length(first_date) > 0) && last_date < first_date) {
    stop("\nIt seems that 'last_date' < first_date Check your inputs.", call. = FALSE)
  }
  
  
  # Check if reference date is valid
  if (!is.null(reference_date) && !is.na(reference_date)) {
    if ((class(reference_date) != "character")) {
      stop("\nArgument 'reference_date' is not valid. Check your inputs.", call. = FALSE)
    } else if 
    (nchar(reference_date) == 4L & grepl("[[:digit:]]+$", reference_date)) {
      reference_date <- as.character(reference_date)
    } else if
    (nchar(reference_date) == 9L & (grepl("(\\d{4})([[:punct:]]{1})(\\d{4}$)", reference_date)) == TRUE) {
      first_year = substr(reference_date, start = 1L, stop = 4L)
      last_year = substr(reference_date, start = 6L, stop = 9L)
    } else
      stop("\nArgument 'reference_date' is not valid. Check yout inputs.", call. = FALSE)
  } else if
  (is.na(reference_date) && (length(reference_date) > 0)) {
    reference_date <- NULL
  } else reference_date
  
  
  # Build args string
  foo_args <- paste0(
    sprintf("Indicador eq '%s'", indicator),
    sprintf(" and IndicadorDetalhe eq '%s'", detail),
    sprintf(" and Data ge '%s'", first_date),
    sprintf(" and Data le '%s'", last_date),
    sprintf(" and DataReferencia eq '%s'", reference_date)
  )
  
  # Build URL
  odata_url <- sprintf(
    paste0(
      "https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais",
      "?$filter=%s", "&$orderby=Data desc&$format=json"),
    foo_args
  )
  
  # Fetching data
  df <- try(
    suppressWarnings(jsonlite::fromJSON(readLines(odata_url))$value),
    silent = TRUE
    )
  if (class(df) == "try-error") {
    stop("\nError in fetching data\n ",
         conditionMessage(attr(df, "condition")),
         call. = FALSE
         )
  } else
    message(paste0("\nFound ", nrow(df), " observations"), appendLF = FALSE)
  df <- dplyr::rename_with(df, ~c("indic", "indic_detail", "date", "reference_year", 
                                  "mean", "median", "sd", "coefvar", "min", "max", "respondents", 
                                  "base"))
    return(df)
}


### evaluate

df=bcb(indicator   = "Fiscal",
       detail         = NULL,
       first_date     = "2021-01-01", 
       last_date      = "2021-03-03")

df_rbcb = rbcb::get_annual_market_expectations(
  indic = "Fiscal",
  start_date = "2021-01-01",
  end_date = "2021-03-03"
)



performance=microbenchmark(
  {bcb(indicator   = "Meta para taxa over-selic",
       detail         = NULL,
       first_date     = "2021-01-01", 
       last_date      = "2021-03-03")},
  {rbcb::get_annual_market_expectations(
    indic = "Meta para taxa over-selic",
    start_date = "2021-01-01",
    end_date = "2021-03-03")},
  times = 10
)


indicator   = "Fiscal"
detail         = "Resultado Nominal"
first_date     = "2021-01-01"
last_date      = "2021-03-03"
reference_date = "2025"

foo_url <- httr::modify_url(
  "https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais",
  query = list(
    `$filter` = foo_args, 
    format  = "json", 
    orderby = "Data desc", 
    select  = "Indicador,IndicadorDetalhe,Data,DataReferencia,Media,Mediana,DesvioPadrao,CoeficienteVariacao,Minimo,Maximo,numeroRespondentes,baseCalculo"
  )
)




bcb(detail = "teste")
bcb(indicator = "Fiscal")
bcb(indicator = "Fiscalasas")
bcb(indicator = "Fiscal", detail = NULL)
bcb(indicator = "Fiscal", detail = "Resultado Nominal")
bcb(indicator = "Fiscal", detail = "DFSFSDFS")
bcb(indicator = NULL, detail = "DFSFSDFS")
bcb(indicator = NA, detail = "DFSFSDFS")
bcb(detail = "Resultado Nominal")
bcb(detail = "DFSFSDFS")

bcb(indicator = "Fiscal", first_date = "20210302")
bcb(indicator = "Fiscal", first_date = "54564")
bcb(indicator = "Fiscal", first_date = "2021-03-02")
bcb(indicator = "Fiscal", first_date = "2021-03-33")
bcb(indicator = "Fiscal", first_date = "2021-33-02")
bcb(indicator = "Fiscal", first_date = "2021/03/02")
bcb(indicator = "Fiscal", first_date = "teste")

bcb(indicator = "Fiscal", last_date = "20210302", first_date = "20210302")
bcb(indicator = "Fiscal", last_date = "20210302")
bcb(indicator = "Fiscal", last_date = "2021-03-02")
bcb(indicator = "Fiscal", last_date = "2021-33-02")
bcb(indicator = "Fiscal", last_date = "2021/03/02")
bcb(indicator = "Fiscal", last_date = "2019/03/02")
bcb(indicator = "Fiscal", last_date = "2019/03/33")
bcb(indicator = "Fiscal", last_date = "2019/03/0a")
bcb(indicator = "Fiscal", last_date = "2tes")

bcb(indicator = "Fiscal", first_date = "2021-03-02", last_date = "2021-03-02")
bcb(indicator = "Fiscal", first_date = "2021-03-02", last_date = "2021-02-02")
bcb(indicator = "Fiscal", first_date = "2021-05-02", last_date = "2021-03-02")
bcb(indicator = "Fiscal", first_date = "2021-05-02", last_date = "2021-06-02")
bcb(indicator = "Fiscal", first_date = "2021-05-02", last_date = "20210602")
bcb(indicator = "Fiscal", first_date = "2021-05-02")
bcb(indicator = "Fiscal", first_date = NULL, last_date = "2021-06-02")
bcb(indicator = "Fiscal", first_date = NA, last_date = "2021-06-02")


bcb(indicator = "Fiscal", reference_date = 2021)
bcb(indicator = "Fiscal", reference_date = "2021")
bcb(indicator = "Fiscal", reference_date = "ssddSDS")
bcb(indicator = "Fiscal", reference_date = "20255")

bcb(indicator = "Fiscal", reference_date = 2021:2025)
bcb(indicator = "Fiscal", reference_date = "2021:20255")
bcb(indicator = "Fiscal", reference_date = "2021:20d5")
bcb(indicator = "Fiscal", reference_date = "2021:2025")
bcb(indicator = "Fiscal", reference_date = NULL)
bcb(indicator = "Fiscal", reference_date = NA)





bcb(indicator      = "Fiscal",
    detail         = "teste",
    first_date     = "2021-01-01", 
    last_date      = "2021-03-03",
    reference_date = NULL)

bcb(indicator      = "Fiscal",
    detail         = NULL,
    first_date     = "2021-01-01", 
    last_date      = "2021-03-03",
    reference_date = NULL)

bcb(indicator      = "Fiscal",
    detail         = NA,
    first_date     = "2021-01-01", 
    last_date      = "2021-03-03",
    reference_date = NULL)