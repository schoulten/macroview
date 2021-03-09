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

get_annual <- function (
  indicator      = NULL, # Single character or a character vector
  detail         = NULL, # Single character or NULL/NA
  first_date     = Sys.Date() - 2*365, # String, format: "YYYY-mm-dd" or "YYYY/mm/dd" or NULL/NA
  last_date      = Sys.Date(), # String, format: "YYYY-mm-dd" or "YYYY/mm/dd" or NULL/NA
  reference_date = NULL, # Single character of lenght == 4 or NULL/NA
  be_quiet       = FALSE, # Logical
  use_memoise    = TRUE, # Logical
  do_parallel    = FALSE # Logical
  ){
  # Available indicators
  valid_indicator <- c(
    "Balança Comercial", "Balanço de Pagamentos", "Fiscal", "IGP-DI",
    "IGP-M", "INPC", "IPA-DI", "IPA-M", "IPCA", "IPCA-15", "IPC-FIPE",
    "Preços administrados por contrato e monitorados", "Produção industrial",
    "PIB Agropecuária", "PIB Industrial", "PIB Serviços", "PIB Total",
    "Meta para taxa over-selic", "Taxa de câmbio"
    )
  
  # Check if input "indicator" is valid
  if (missing(indicator) | !all(indicator %in% valid_indicator) | is.null(indicator)) {
    stop("\nArgument 'indicator' is not valid or missing. Check your inputs.", call. = FALSE)
  } else indicator
  
  # Available indicator details
  valid_detail <- c(
    "Balança Comercial / Exportações", "Balança Comercial / Importações",
    "Balança Comercial / Saldo", "Balanço de Pagamentos / Conta corrente",
    "Balanço de Pagamentos / Investimento direto no país",
    "Fiscal / Resultado Primário", "Fiscal / Resultado Nominal",
    "Fiscal / Dívida líquida do setor público", "Meta para taxa over-selic / Fim do ano",
    "Meta para taxa over-selic / Média do ano"
  )
  
  # Check if input "detail" is valid and get detail input (or NULL) if is valid
  if (!is.null(detail) && !is.na(detail)) {
    if ((class(detail) != "character")) {
      stop("\nArgument 'detail' is not valid. Check your inputs.", call. = FALSE)
    } else if 
    (!all(paste0(indicator, " / ", detail) %in% valid_detail)) {
      stop("\nArgument 'detail' is not valid. Check your inputs.", call. = FALSE)
    } else if
    (length(detail) > 1) {
      stop("\nArgument 'detail' is not valid. Check your inputs.", call. = FALSE)
    } else
      detail
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
  if (missing(first_date)) {first_date = Sys.Date() - 10 * 365} else
    first_date
  
  # Check if last_date argument is valid
  last_date <- try(as.Date(last_date), silent = TRUE)
  if (length(last_date) <= 0 || is.na(last_date)) {last_date = NULL}
  if (class(last_date) %in% "try-error") {
    stop("\nArgument 'last_date' is not a valid date.", call. = FALSE)
  }
  if (missing(last_date)) {last_date = Sys.Date() - 10 * 365} else
    last_date
  
  # Check if first_date > Sys.Date()
  if ((length(first_date) > 0) && first_date > Sys.Date()) {
    stop("\nIt seems that 'first_date' > current date. Check your inputs.", call. = FALSE)
  }
  
  # Check if last_date < first_date
  if ((length(first_date) > 0) && last_date < first_date) {
    stop("\nIt seems that 'last_date' < first_date. Check your inputs.", call. = FALSE)
  }
  
    # Check if reference date is valid
  if (!is.null(reference_date) && !is.na(reference_date)) {
    if ((class(reference_date) != "character")) {
      stop("\nArgument 'reference_date' is not valid. Check your inputs.", call. = FALSE)
    } else if 
    (nchar(reference_date) == 4L & grepl("[[:digit:]]+$", reference_date)) {
      reference_date <- as.character(reference_date)
    } else
      stop("\nArgument 'reference_date' is not valid. Check yout inputs.", call. = FALSE)
  } else if
  (is.na(reference_date) && (length(reference_date) > 0)) {reference_date <- NULL} else
    reference_date
  
  # Build args string
  foo_args <- paste0(
    paste0("(", paste(sprintf("Indicador eq '%s'", indicator), collapse = " or ", sep = ""), ")"),
    sprintf(" and IndicadorDetalhe eq '%s'", detail),
    sprintf(" and Data ge '%s'", first_date),
    sprintf(" and Data le '%s'", last_date),
    sprintf(" and DataReferencia eq '%s'", reference_date)
  )
  
  # Build URL
  odata_url <- list(
    httr::modify_url(
      "https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais",
      query = list(
        `$filter`  = foo_args, 
        `$format`  = "json", 
        `$orderby` = "Data desc"
        )
      )
    )
  
  # Fetching data function
  if ((class(use_memoise) != "logical") || (is.na(use_memoise))) {
    stop("\nArgument 'use_memoise' must be logical. Check your inputs.", call. = FALSE)
  } else
    memoising <- function(use_memoise, cache_dir) {
      if (use_memoise) {
        foo_memoise <- memoise::memoise(f = jsonlite::fromJSON, cache = cache_dir)
      } else
        foo_memoise <- jsonlite::fromJSON
  }
  
  from_bcb <- memoising(
    use_memoise = use_memoise,
    cache_dir   = memoise::cache_filesystem("./cache_bcb")
    )
  
  # Fetching data
  if (!do_parallel) {
    
    # Message to display
    if ((class(be_quiet) != "logical") || (is.na(be_quiet))) {
      stop("\nArgument 'be_quiet' must be logical. Check your inputs.", call. = FALSE)
    } else if
    (be_quiet) {message("", appendLF = FALSE)
    } else {
      message(
        paste0("\nFetching [", paste(indicator, collapse = ", "), "] data ", "from BCB-Olinda... \n"),
        appendLF = FALSE
      )
    }
    
    df <- try(
      suppressWarnings(purrr::pmap(.l = odata_url, .f = from_bcb)[[1]][["value"]]),
      silent = TRUE
    )
  } else {
    formals_parallel <- formals(future::plan())
    used_workers <- formals_parallel$workers
    available_cores <- future::availableCores()
    if (be_quiet) {message("", appendLF = FALSE)
    } else
      message(
        paste0("\nRunning parallel with ", used_workers, " cores (", available_cores, " available)\n"),
        appendLF = TRUE
        )
    msg <- utils::capture.output(future::plan())
    flag <- grepl("sequential", msg)[1]
    if (flag) {
      stop(paste0(
        "When using do_parallel = TRUE, you need to call future::plan() to configure your parallel settings.\n", 
        "A suggestion, write the following lines just before:\n\n", 
        "future::plan(future::multisession, workers = floor(future::availableCores()/2))", "\n\n",
        "Notice it will use half of your available cores so that your OS has some room to breathe."),
        call. = FALSE
        )
    }
    
    # Message to display
    if ((class(be_quiet) != "logical") || (is.na(be_quiet))) {
      stop("\nArgument 'be_quiet' must be logical. Check your inputs.", call. = FALSE)
    } else if
    (be_quiet) {message("", appendLF = FALSE)}
    else {
      message(
        paste0("\nFetching [", paste(indicator, collapse = ", "), "] data ", "from BCB-Olinda... \n"),
        appendLF = FALSE
      )
    }
    
    df <- try(
      suppressWarnings(furrr::future_pmap(.l = odata_url, .f = from_bcb)[[1]][["value"]]),
      silent = TRUE
    )
  }
  
  if (class(df) == "try-error") {
    stop("\nError in fetching data: ", conditionMessage(attr(df, "condition")),
         call. = FALSE
         )
  } else if
  (purrr::is_empty(df)) {
    stop(
      paste0(
      "\nIt seems that there is no data available. Possibly, the last available data is earlier than that defined in one of these arguments:
      \n1. 'first_date'", "\n2. 'reference_date'"
      ),
      call. = FALSE
      )
  } else if
  (be_quiet) {message("", appendLF = FALSE)}
  else
    message(paste0("\nFound ", nrow(df), " observations!\n"), appendLF = FALSE)
  
  df <- dplyr::rename_with(
    dplyr::as_tibble(df), 
    ~c("indicator", "detail", "date", "reference_date", "mean",
       "median", "sd", "coef_var", "min", "max", "n_respondents", "basis")
    )
  df <- dplyr::mutate(df, date = as.Date(date, format = "%Y-%m-%d"))
  
  return(df)
}



### evaluate
tictoc::tic()
df = get_annual(indicator      = c("PIB Total", "Fiscal"),
         first_date     = "2018-01-01",
         use_memoise    = FALSE,
         do_parallel    = FALSE)
tictoc::toc()

# {rbcb}
tictoc::tic()
df_rbcb = rbcb::get_annual_market_expectations(
  indic = c("PIB Total", "Fiscal"),
  start_date     = "2018-01-01")
tictoc::toc()





indicator      = c("PIB Total", "Fiscal")
detail         = NULL
first_date     = "2018-01-01"
last_date      = "2018-01-31"
be_quiet       = FALSE
reference_date = NULL
use_memoise    = FALSE



get_annual(detail = "teste")
get_annual(indicator = "Fiscal")
get_annual(indicator = "Fiscalasas")
get_annual(indicator = "Fiscal", detail = NULL)
get_annual(indicator = "Fiscal", detail = "Resultado Nominal")
get_annual(indicator = "Fiscal", detail = "DFSFSDFS")
get_annual(indicator = NULL, detail = "DFSFSDFS")
get_annual(indicator = NA, detail = "DFSFSDFS")
get_annual(indicator = "Fiscal", detail = "Resultado Nominal", first_date = "2021-01-30")
get_annual(detail = "DFSFSDFS")
get_annual(indicator = "Fiscal", detail = NA)

get_annual(indicator = "Fiscal", first_date = "20210302")
get_annual(indicator = "Fiscal", first_date = "54564")
get_annual(indicator = c("Fiscal", "IPCA"), first_date = "2019-03-08", do_parallel = TRUE, use_memoise = FALSE)
get_annual(indicator = c("Fiscal", "as"), first_date = "2021-03-02", do_parallel = TRUE, use_memoise = FALSE)
get_annual(indicator = "Fiscal", first_date = "2021-03-33")
get_annual(indicator = "Fiscal", first_date = "2021-33-02")
get_annual(indicator = "Fiscal", first_date = "2021/03/02")
get_annual(indicator = "Fiscal", first_date = "teste")

get_annual(indicator = "Fiscal", last_date = "20210302", first_date = "20210302")
get_annual(indicator = "Fiscal", last_date = "20210302")
get_annual(indicator = "Fiscal", last_date = "2021-03-02")
get_annual(indicator = "Fiscal", last_date = "2021-33-02")
get_annual(indicator = "Fiscal", last_date = "2021/03/02")
get_annual(indicator = "Fiscal", last_date = "2019/03/02")
get_annual(indicator = "Fiscal", last_date = "2019/03/33")
get_annual(indicator = "Fiscal", last_date = "2019/03/0a")
get_annual(indicator = "Fiscal", last_date = "2tes")

get_annual(indicator = "Fiscal", first_date = "2021-03-02", last_date = "2021-03-02")
get_annual(indicator = c("Fiscal", "IPC-FIPE", "IPCA"), first_date = "2021-01-02", last_date = "2021-02-02")
get_annual(indicator = "Fiscal", first_date = "2021-05-02", last_date = "2021-03-02")
get_annual(indicator = "IPC-FIPE", first_date = "2021-05-02", last_date = "2021-06-02")
get_annual(indicator = "Fiscal", first_date = "2021-05-02", last_date = "20210602")
get_annual(indicator = "Fiscal", first_date = "2021-05-02")
get_annual(indicator = "IPC-FIPE", first_date = NULL, last_date = "2021-06-02")
get_annual(indicator = "IPC-FIPE", first_date = NA, last_date = "2021-06-02") %>% dplyr::arrange(date)


get_annual(indicator = "Fiscal", reference_date = 2021)
get_annual(indicator = "Fiscal", reference_date = "2050", do_parallel = TRUE)
get_annual(indicator = "Fiscal", reference_date = "ssddSDS")
get_annual(indicator = "Fiscal", reference_date = "20255")

get_annual(indicator = "Fiscal", reference_date = 2021:2025)
get_annual(indicator = "Fiscal", reference_date = "2021:20255")
get_annual(indicator = "Fiscal", reference_date = "2021:20d5")
get_annual(indicator = "Fiscal", reference_date = "2021:2025")
get_annual(indicator = "IPC-FIPE", reference_date = NULL)
get_annual(indicator = "IPC-FIPE", reference_date = NA, do_parallel = TRUE, use_memoise = FALSE, first_date = "2021-02-01")

