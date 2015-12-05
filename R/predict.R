#' Create predictions from an LDA model
#'
#' @param object An object of class \code{lda}
#' @param newdata An optional data frame to use in the prediction
#'
#' @return A list containing the predicted values and classes
#' @export

predict.lda <- function(object, newdata = NULL) {
    # Set up the data
    if (is.null(newdata)) newdata <- object$x

    # Get model parameters
    a <- object$a
    b <- object$b

    # Generate predictions
    preds <- apply(newdata, 1, function(.x) crossprod(a, .x) + b)
    list(fit = as.numeric(preds > 0), preds = preds)
}

#' Create predictions from a Naive Bayes model
#'
#' @param object An object of class \code{lda}
#' @param newdata An optional data frame to use in the prediction
#'
#' @return A list containing the predicted values and classes
#' @export

predict.nb <- function(object, newdata) {
    # Set up the data
    if (is.null(newdata)) newdata <- object$x

    # Get model parameters
    mus <- object$mus
    sigmas <- object$sigmas
    p_rat <- object$p_rat

    # Generate predictions
    preds <- log(p_rat) + apply(newdata, 1, log_odds, mus, sigmas)
    list(fit = as.numeric(preds > 0), preds = preds)
}