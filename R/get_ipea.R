#' Download series from IPEADATA
#'
#' @param code IPEADATA code
#' @param silent Logical
#'
#' @return Tibble
#' @export
#'
get_ipea <- function(code, silent = FALSE) {

  ft_message(
    paste0("\nFetching ", "[", code, "]", " from IPEADATA...\n"),
    silent
  )

  raw_data <- httr::GET(
    sprintf("http://ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='%s')", code)
  )

  tbl_data <- httr::content(raw_data)[[2]] %>%
    dplyr::bind_rows() %>%
    dplyr::rename(
      "code" = `SERCODIGO`,
      "date" = `VALDATA`,
      "value" = `VALVALOR`,
      "uname" = `NIVNOME`,
      "tcode" = `TERCODIGO`
      ) %>%
    dplyr::mutate(
      date = lubridate::as_date(date),
      uname = as.factor(uname),
      tcode = as.integer(tcode)
    )

  return(tbl_data)

}
