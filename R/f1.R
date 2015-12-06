#' Calculate the precision of a predictor
#'
#' @param fit the fitted values of a predictor
#' @param the original target
#'
#' @return A numeric scalar
#'
#' @export

precision <- function(fit, actual) {
    sum((fit == 1) & (fit == actual)) / sum(fit)
}

#' Calculate the recall of a predictor
#'
#' @param fit the fitted values of a predictor
#' @param the original target
#'
#' @return A numeric scalar
#'
#' @export

recall <- function(fit, actual) {
    sum((fit == 1) & (fit == actual)) / sum(actual)
}

#' Calculate the F1 score of a predictor
#'
#' @param prec the precision of a predictor
#' @param rec the recall of a predictor
#'
#' @return A numeric scalar
#'
#' @export

f1_score <- function(prec, rec) {
    2 * (prec * rec) / (prec + rec)
}
