#' Score a model with Precision, Recall and F1
#'
#' @param .object a model object
#' @param newdata an optional new data frame for generating predictions; otherwise score fit on training data
#' @param actual an optional vector for the true values; used for assessing predictions on test data
#' @param ... other parameters passed for scoring
#'
#' @return A list with the Precision, Recall and F1
#' @export

score_model <- function(.object, newdata = NULL, actual = NULL, ...) {
    UseMethod("score_model")
}

#' @describeIn score_model
#' Generate scores for lda models
#'
#' @export

score_model.lda <- function(.object, newdata = NULL, actual = NULL) {
    # Get fitted values
    fit <- predict(.object, newdata)$fit

    # Get actuals
    if (is.null(actual)) actual <- .object$actual

    # Find precision and recall
    prec <- precision(fit, actual)
    rec <- recall(fit, actual)

    # Return results
    list(precision = prec, recall = rec, f1 = f1_score(prec, rec))
}

#' @describeIn score_model
#' Generate scores for naive bayes models
#'
#' @export

score_model.nb <- function(.object, newdata = NULL, actual = NULL) {
    # Get fitted values
    fit <- predict(.object, newdata)$fit

    # Get actuals
    if (is.null(actual)) actual <- .object$actual

    # Find precision and recall
    prec <- precision(fit, actual)
    rec <- recall(fit, actual)

    # Return results
    list(precision = prec, recall = rec, f1 = f1_score(prec, rec))
}

#' @describeIn score_model
#' Generate scores for models fit in \code{caret}
#'
#' @export

score_model.train <- function(.object, newdata = NULL, actual = NULL) {
    # Get fitted values
    fit <- predict(.object, newdata) %>% as.character %>% as.numeric

    # Get actuals
    if (is.null(actual)) {
        actual <- .object$trainingData$.outcome %>% as.character %>% as.numeric
    }

    # Find precision and recall
    prec <- precision(fit, actual)
    rec <- recall(fit, actual)

    # Return results
    list(precision = prec, recall = rec, f1 = f1_score(prec, rec))
}