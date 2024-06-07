#' Convert Data Frame Rows to Named Lists
#'
#' This function takes a data frame, converts each row into a named list,
#' and returns a list of these named lists.
#'
#' @param df A data frame to be converted.
#'
#' @return A list where each element is a named list created from a row of the input data frame.
#'
#' @examples
#' library(shiny)
#' named_list <- convert_df_to_named_lists(iris)
#' print(named_list[[1]])
convert_df_to_named_lists <- function(df) {
  apply(df, MARGIN = 1, list) |>
    lapply(function(x) {
      x |>
        unlist() |>
        as.list()
    })
}
