#' Sampling function for bootstrap
#'
#' A function to randomly split a data frame into training and test samples.
#'
#' @param .data a data frame
#' @param .test_rate the (approximate) portion of the data allocated to test data
#'
#' @return a list of two dataframes, the training and test dataset
#' @export

split_data <- function(.data, .test_rate = 0.1) {
    n <- nrow(.data)
    svec <- sample(c("train", "test"), n, replace = TRUE, prob = c(1 - .test_rate, .test_rate))
    split(.data, svec)
}