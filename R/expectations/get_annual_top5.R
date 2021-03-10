get_annual_top5 <- function (
  indicator      = NULL, # Single character or a character vector
  detail         = NULL, # Single character or NULL/NA
  first_date     = Sys.Date() - 2*365, # String, format: "YYYY-mm-dd" or "YYYY/mm/dd" or NULL/NA
  last_date      = Sys.Date(), # String, format: "YYYY-mm-dd" or "YYYY/mm/dd" or NULL/NA
  reference_date = NULL, # Single character of lenght == 4 or NULL/NA
  calc_type      = c("short", "medium", "long"), # Single character or a character vector
  be_quiet       = FALSE, # Logical
  use_memoise    = TRUE, # Logical
  do_parallel    = FALSE # Logical
){
  # Available indicators
  valid_indicator <- c("IGP-DI", "IGP-M", "IPCA", "Meta para taxa over-selic", "Taxa de câmbio")
  
  # Check if input "indicator" is valid
  if (missing(indicator) | !all(indicator %in% valid_indicator) | is.null(indicator)) {
    stop("\nArgument 'indicator' is not valid or missing. Check your inputs.", call. = FALSE)
  } else indicator
  
  # Available indicator details
  valid_detail <- c(
    "Meta para taxa over-selic / Fim do ano",
    "Meta para taxa over-selic / Média do ano",
    "Taxa de câmbio / Fim do ano",
    "Taxa de câmbio / Média do ano"
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
  if (!is.null(last_date)) {
    if ((length(first_date) > 0) && last_date < first_date) {
      stop("\nIt seems that 'last_date' < first_date. Check your inputs.", call. = FALSE)
    }
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
  
  # Check if calc_type input is valid
  if (!is.null(calc_type) && !is.na(calc_type) && length(calc_type) > 0) {
    if ((class(calc_type) != "character")) {
      stop("\nArgument 'calc_type' is not valid. Check your inputs.", call. = FALSE)
    } else if
    (!all(calc_type %in% c("short", "medium", "long"))){
      stop("\nArgument 'calc_type' is not valid. Check your inputs.", call. = FALSE)
    } else {
      calc_type <- dplyr::recode(
        as.character(calc_type),
        "short"  = "C",
        "medium" = "M",
        "long"   = "L"
      )
    }
  } else if
  (is.na(calc_type) && (length(calc_type) > 0) | is.null(calc_type)) {calc_type <- NULL} else {
    stop("\nArgument 'calc_type' is not valid. Check your inputs.", call. = FALSE)
  }
  
  # Check class of do_parallel argument
  if ((class(do_parallel) != "logical") || (is.na(do_parallel))) {
    stop("\nArgument 'do_parallel' must be logical. Check your inputs.", call. = FALSE)
  } else if
  
  # Check class of be_quiet argument
  ((class(be_quiet) != "logical") || (is.na(be_quiet))) {
    stop("\nArgument 'be_quiet' must be logical. Check your inputs.", call. = FALSE)
  }
  
  # Build args string
  foo_args <- paste0(
    paste0("(", paste(sprintf("Indicador eq '%s'", indicator), collapse = " or ", sep = ""), ")"),
    sprintf(" and IndicadorDetalhe eq '%s'", detail),
    sprintf(" and Data ge '%s'", first_date),
    sprintf(" and Data le '%s'", last_date),
    sprintf(" and DataReferencia eq '%s'", reference_date),
    {if (!is.null(calc_type)) {
      paste0(" and (", paste(sprintf("tipoCalculo eq '%s'", calc_type), collapse = " or ", sep = ""), ")") 
    }}
  )
  
  # Build URL
  odata_url <- list(
    httr::modify_url(
      "https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoTop5Anuais",
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
    
    # Display message
    if (be_quiet) {message("", appendLF = FALSE)} else {
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
    } else if
    (be_quiet) {message("", appendLF = FALSE)} else
      message(
        paste0("\nRunning parallel with ", used_workers, " cores (", available_cores, " available)\n",
               "\nFetching [", paste(indicator, collapse = ", "), "] data ", "from BCB-Olinda... \n"),
        appendLF = TRUE
      )
    
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
  
  # Convert as_tibble()  
  df <- dplyr::rename_with(
    dplyr::as_tibble(df), 
    ~c("indicator", "detail", "date", "reference_date", "calc_type",
       "mean", "median", "sd", "coef_var", "min", "max")
  )
  df <- dplyr::mutate(
    df,
    date = as.Date(date, format = "%Y-%m-%d"),
    calc_type = recode(calc_type, "C" = "S") # short-term calculation type
    )
  
  return(df)
}
