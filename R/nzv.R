#' Remove Zero variance Columns from a data frame
#'
#' The predicate function \code{zero_variance} checks to see if the variance of a column
#' is lower than a pre-defined threshold. The function \code{nzv} applies the predicate
#' to a data frame and drops the columns that fail the test.
#'
#' @param .data A data frame
#' @param .col A column of a data frame
#' @param .thresh A minimal theshold for non-zero variance
#'
#' @return A data frame with the zero variance columns removed
#'
#' @export

nzv <- function(.data, .target = "target", .thresh = 1e-4) {
    y <- .data[,.target, drop = FALSE]
    .data[.target] <- NULL
    id <- map_lgl(.data, Negate(zero_variance), .thresh)
    cbind(.data[id], y)
}

#' @describeIn nzv
#'
#' Predicate function to see if a column is zero variance
#'
#' @export

zero_variance <- function(.col, .thresh) {
    var(.col) < .thresh
}