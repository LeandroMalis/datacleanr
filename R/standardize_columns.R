#' Standardize Columns
#'
#' This function standardizes numeric columns in a dataset.
#' @param data A data frame.
#' @return A data frame with numeric columns standardized.
#' @examples
#' data <- data.frame(a = c(1, 2, 3, 4), b = c(2, 3, 4, 5))
#' standardize_columns(data)
#' @export
standardize_columns <- function(data) {
  for (col in colnames(data)) {
    if (is.numeric(data[[col]])) {
      data[[col]] <- scale(data[[col]])
    }
  }
  return(data)
}
