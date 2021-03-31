#' Create sparkline in gt object
#'
#' @param table_data tibble
#' @param plot_col plot_col
#' @param data_col data_col
#'
#' @return spartline col
#' @export
gt_sparkline <- function(table_data, plot_col, data_col){

  data_in = purrr::pluck(table_data, "_data", data_col)

  gt::text_transform(
    data      = table_data,
    locations = gt::cells_body(columns = gt::vars({{plot_col}})),
    fn        = function(x){
      sparkline_plot <- purrr::map(
        data_in, ~sparkline::spk_chr(values = .x, chartRangeMin = 0)
        )
      purrr::map(sparkline_plot, gt::html)
      }
    )
  }
