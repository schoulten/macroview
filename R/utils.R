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
  if (missing(indicator) | !all(indicator %in% valid_indicator)) {
    stop("\nArgument 'indicator' is not valid or missing. Check your inputs.", call. = FALSE)
  } 
  
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
  if (!is.null(detail) & !all(paste0(indicator, " / ", detail) %in% valid_detail)) {
    stop("\nArgument 'detail' is not valid. Check yout inputs.", call. = FALSE)
  }
  
  # Convert first_date argument to class "Date"
  tryCatch(
    (first_date <- as.Date(first_date)),
    error = function(f) {NA}
    )

  # Convert last_date argument to class "Date"
  tryCatch(
    (last_date <- as.Date(last_date)),
    error = function(l) {NA}
    )
  
  # Check class of first_date / last_date argument
  if ((class(first_date) != "Date") | (class(last_date) != "Date")) {
    stop("\nArgument 'first_date' and/or 'last_date' is not a valid date.", call. = FALSE)
  }
  
  # Check if first_date > Sys.Date()
  if (first_date > Sys.Date()) {
    stop("\nIt seems that 'first_date' > current date. Check your inputs.", call. = FALSE)
  }
  
  # Check if last_date < first_date
  if (last_date < first_date) {
    stop("\nIt seems that 'last_date' < 'first_date'. Check your inputs.", call. = FALSE)
  }
  
  
  # Reference date
  if (!is.null(reference_date)) {
    
    if ((class(reference_date) != "character")) {
      stop("\nArgument 'reference_date' is not valid. Check yout inputs.", call. = FALSE)
    } else if 
    (nchar(reference_date) == 4 & grepl("[[:digit:]]+$", reference_date)) {
      reference_date <- as.character(reference_date)
    } else if
    (nchar(reference_date) == 9 & (grepl("(\\d{4})([[:punct:]]{1})(\\d{4}$)", reference_date)) == TRUE) {
      first_year = substr(reference_date, start = 1, stop = 4)
      last_year = substr(reference_date, start = 6, stop = 9)
    } else
      stop("\nArgument 'reference_date' is not valid. Check yout inputs.", call. = FALSE)
    
  }
    

  
}


filter_indicator <- paste0(sprintf("Indicador eq '%s'", indicator))
filter_detail <- paste0(sprintf(" and IndicadorDetalhe eq '%s'", detail))
filter_fd <- if (!is.null(last_date)) {
  paste0(sprintf("Data le '%s'", end_date))
} else NULL
filter_ld


odata_url <- sprintf(
  paste0("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais",
         "?$filter=%s",
         "/dados?formato=json&", "dataInicial=%s&", "dataFinal=%s"),
  id, 
  format(first.date, "%d/%m/%Y"), 
  format(last.date, "%d/%m/%Y"))






urrrl="https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$filter=Indicador%20eq%20'Meta%20para%20taxa%20over-selic'%20and%20IndicadorDetalhe%20eq%20'Fim%20do%20ano'%20and%20DataReferencia%20eq%20'2021'%20and%20Data%20ge%20'2021-01-01'%20and%20Data%20le%20'2021-01-29'&$orderby=Data%20desc&$format=text/html"


  url <- annual_market_expectations_url(indicator, start_date, 
                                        end_date, ...)
  
  
  indic_filter <- paste(sprintf("Indicador eq '%s'", indic),  collapse = " or ")
  
  indic_filter <- paste0("(", indic_filter, ")")
  
  
  sd_filter <- if (!is.null(start_date)) {
    sprintf("Data ge '%s'", start_date)
  } else NULL
  
  ed_filter <- if (!is.null(end_date)) {
    sprintf("Data le '%s'", end_date)
  } else NULL
  
  filter__ <- paste(c(indic_filter, sd_filter, ed_filter),  collapse = " and ")
  
  
  text_ <- .get_series(url)
  data_ <- jsonlite::fromJSON(text_)
  df_ <- tibble::as_tibble(data_$value)
  names(df_) <- c("indicator", "indicator_detail", "date", "reference_year", 
                  "mean", "median", "sd", "coefvar", "min", "max", "respondents", 
                  "base")
  df_$date <- as.Date(df_$date)
  df_





### evaluate

bcb(detail = "teste")
bcb(indicator = "Fiscal")
bcb(indicator = "Fiscalasas")
bcb(indicator = "Fiscal", detail = NULL)
bcb(indicator = "Fiscal", detail = "Resultado Nominal")
bcb(indicator = "Fiscal", detail = "DFSFSDFS")
bcb(detail = "Resultado Nominal")
bcb(detail = "DFSFSDFS")

bcb(indicator = "Fiscal", first_date = "20210302")
bcb(indicator = "Fiscal", first_date = "2021-03-02")
bcb(indicator = "Fiscal", first_date = "2021/03/02")

bcb(indicator = "Fiscal", last_date = "20210302", first_date = "20210302")
bcb(indicator = "Fiscal", last_date = "20210302")
bcb(indicator = "Fiscal", last_date = "2021-03-02")
bcb(indicator = "Fiscal", last_date = "2021/03/02")

bcb(indicator = "Fiscal", first_date = "2021-03-02", last_date = "2021-03-02")
bcb(indicator = "Fiscal", first_date = "2021-03-02", last_date = "2021-02-02")
bcb(indicator = "Fiscal", first_date = "2021-05-02", last_date = "2021-03-02")
bcb(indicator = "Fiscal", first_date = "2021-05-02", last_date = "2021-06-02")
bcb(indicator = "Fiscal", first_date = "2021-05-02")


bcb(indicator = "Fiscal", reference_date = 2021)
bcb(indicator = "Fiscal", reference_date = "2021")
bcb(indicator = "Fiscal", reference_date = "ssddSDS")
bcb(indicator = "Fiscal", reference_date = "20255")

bcb(indicator = "Fiscal", reference_date = 2021:2025)
bcb(indicator = "Fiscal", reference_date = "2021:20255")
bcb(indicator = "Fiscal", reference_date = "2021:20d5")
bcb(indicator = "Fiscal", reference_date = "2021:2025")
