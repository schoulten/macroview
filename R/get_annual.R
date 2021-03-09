function (
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