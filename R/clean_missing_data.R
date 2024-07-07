#' Clean Missing Data
#'
#' This function handles missing values in a dataset by either removing them or filling them using a specified method.
#' @param data A data frame.
#' @param method A character string specifying the method to handle missing values. Options are "remove" or "mean".
#' @return A data frame with missing values handled.
#' @examples
#' data <- data.frame(a = c(1, 2, NA, 4), b = c(NA, 2, 3, 4))
#' clean_missing_data(data, method = "mean")
#' @export
clean_missing_data <- function(data, method = "remove") {
  if (method == "remove") {
    data <- na.omit(data)
  } else if (method == "mean") {
    for (col in colnames(data)) {
      if (is.numeric(data[[col]])) {
        data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
      }
    }
  } else {
    stop("Method not recognized. Use 'remove' or 'mean'.")
  }
  return(data)
}
