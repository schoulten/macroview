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

bcb <- function (indicator, detail = NULL, first_date = Sys.Date() - 10 * 365, last_date = Sys.Date(), ...){
  
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
  if (!all(indicator %in% valid_indicator)) {
    stop("Argument 'indicator' is not valid. Check your inputs.")
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
    "Meta para taxa over-seli / Fim do ano",
    "Meta para taxa over-seli / Média do ano"
  )
  
  # Check if input "detail" is valid and get detail input (or NULL) if is valid
  if (!is.null(detail) & !all(paste0(indicator, " / ", detail) %in% valid_detail)) {
    stop("Argument 'detail' is not valid. Check yout inputs.")
  }
  
  # Check class of first_date argument
  tryCatch(
    (first_date <- as.Date(first_date)),
    error = function(e) {print("Argument 'first_date' is not a valid date.")}
    )
  if (class(first_date) != "Date") {
    stop("Argument 'first_date' is not a valid date.")
  }
  
  # Check class of last_date argument
  last_date <- as.Date(last_date)
  if (class(last_date) != "Date") {
    stop("Argument 'last_date' is not a valid date.")
  }
  
  # Check if last_date < first_date
  if (last_date < first_date) {
    stop("It seems that 'last_date' < 'first_date'. Check your inputs.")
  }
  
  # Check if first_date > Sys.Date()
  if (first_date > Sys.Date()) {
    stop("It seems that 'first_date' > current date. Check your inputs.")
  }

  
  filter_indicator <- paste0(sprintf("Indicador eq '%s'", indicator))
  filter_detail <- paste0(sprintf(" and IndicadorDetalhe eq '%s'", detail))

}





bcb(indicator = "Fiscal")
bcb(indicator = "Fiscalasas")
bcb(indicator = "Fiscal", detail = NULL)
bcb(indicator = "Fiscal", detail = "Resultado Nominal")
bcb(indicator = "Fiscal", detail = "DFSFSDFS")

bcb(indicator = "Fiscal", first_date = "20210302")
bcb(indicator = "Fiscal", first_date = "2021-03-02")
bcb(indicator = "Fiscal", first_date = "2021/03/02")

teste = bcb(indicator = "Fiscal", last_date = "teste")
bcb(indicator = "Fiscal", last_date = "2021-03-02")
bcb(indicator = "Fiscal", last_date = "2021/03/02")





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
  
  httr::modify_url("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais", 
                   query = list(`$filter` = filter__, `$format` = "application/json", 
                                `$orderby` = "Data desc", `$select` = "Indicador,IndicadorDetalhe,Data,DataReferencia,Media,Mediana,DesvioPadrao,CoeficienteVariacao,Minimo,Maximo,numeroRespondentes,baseCalculo", 
                                ...))
  
  
  
  
  
  
  
  text_ <- .get_series(url)
  data_ <- jsonlite::fromJSON(text_)
  df_ <- tibble::as_tibble(data_$value)
  names(df_) <- c("indicator", "indicator_detail", "date", "reference_year", 
                  "mean", "median", "sd", "coefvar", "min", "max", "respondents", 
                  "base")
  df_$date <- as.Date(df_$date)
  df_
}

get_bcb_olinda <- function(
  
  
)