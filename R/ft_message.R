#' Messages
#'
#' @param text String
#' @param silent Logical
#'
#' @return Message in console
#' @export
#'
ft_message <- function (text, silent)
{
  if (silent) {
    message("", appendLF = FALSE)
  }
  else {
    message(text, appendLF = FALSE)
  }
}
