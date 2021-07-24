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

  raw_data <- jsonlite::fromJSON(
    sprintf("http://ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='%s')", code),
    flatten = TRUE
  )[[2]]

  tbl_data <- raw_data %>%
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
