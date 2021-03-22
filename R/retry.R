#' Retry an expression
#'
#' @param foo The expression
#' @param tries Number of tries
#'
#' @return Output of expression or error
#' @export
#'
retry <- function(foo, tries) {
  message(paste0("Trying expression \nRemaining tries:", tries, "\n\n"))
  withRestarts(
    tryCatch(
      { foo },
      error = function(e) { invokeRestart("restart") }
    ),
    restart = function() {
      message("Retrying...")
      stopifnot(tries > 0)
      retry(foo, tries-1)
    }
  )
}
