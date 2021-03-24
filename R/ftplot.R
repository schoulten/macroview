#' Easy plot highcharter personal theme
#'
#' @param hc
#' @param title
#' @param subtitle
#' @param source
#' @param range_n
#' @param navigator
#'
#' @return
#' @export
#'
#' @examples
ftplot <- function(
  hc,
  title,
  subtitle,
  source,
  range_n,
  navigator = FALSE
  )
  {
  hc %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_credits(enabled = TRUE, text = paste0("Source: ", source), style = list(fontSize = "12px")) %>%
    hc_xAxis(title = FALSE) %>%
    hc_yAxis(labels = list(format = "{value}%"), title = FALSE,
             plotLines = list(
               list(
                 value = 0,
                 color = "#1a1a1a",
                 width = 2
                 )
               )
             ) %>%
    hc_tooltip(shared = TRUE) %>%
    hc_add_theme(theme_fortietwo) %>%
    hc_navigator(enabled = navigator)
  }
