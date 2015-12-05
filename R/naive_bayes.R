#' Fit a Naive Bayes Classifier
#'
#' @param formula An R formual specifying the Naive Bayes Classifier
#' @param data A data frame to fit the model
#'
#' @details This version of a naive bayes classifier generates predictions through the
#' log odds of an observation belonging to either class 1 or 0. We can the classifier
#' "naive" because we calculate the odds assuming that each column is independent.
#'
#' Our classification rule follows:
#'
#' \deqn{f(x) = 1 \mathrm{ if } \log \frac{P(Y = 1 | X = x)}{P(Y = 0 | X = x)} }
#' \deqn{= \log \frac{p_1}{p_0} + \sum_{j = 1}^{p} \left[ \frac{1}{2} \log \frac{\sigma_{0j}^2}{\sigma_{1j}^2} - \frac{(x_j - \mu_{1j})^2}{2 \sigma_{1j}^2} + \frac{(x_j - \mu_{0j})^2}{2 \sigma_{0j}^2}\right]}
#'
#' The \code{log_odds} function makes it much easier to apply this formula across the entire data frame.
#'
#' @return A list with the class \code{n} containing the following:
#' \describe{
#'      \item{actual}{the original target values}
#'      \item{x}{the original model data matrix}
#'      \item{fit}{the class values}
#'      \item{preds}{the values of the linear combination}
#'      \item{mus}{a list of the group means, each item is a vector}
#'      \item{sigmas}{the list of the group variances, each item is a vector}
#'      \item{p_rat}{the ratio of prior rates}
#' }
#'
#' @export

naive_bayes <- function(formula, data) {
    # Set up the model frame from the formula
    mf <- model.frame(formula, data)
    x <- mf[-1]
    y <- mf[[1]]

    # Assertions
    stopifnot(length(unique(y)) == 2)

    # Model parameters
    n <- length(y)
    ns <- map(split(y, y), length)
    ps <- map(ns, ~ .x / n)
    p_rat <- reduce_right(ps, `/`)

    # Group means and variances
    groups <- split(x, y)
    mus <- at_depth(groups, 1, map_dbl, mean)
    sigmas <- at_depth(groups, 1, map_dbl, var)

    # Calculate the log odds
    preds <- log(p_rat) + apply(x, 1, log_odds, mus, sigmas)

    # Return
    results <- list(actual = y,
                    x = x,
                    fit = as.numeric(preds > 0),
                    preds = preds,
                    mus = mus,
                    sigmas = sigmas,
                    p_rat = p_rat)

    structure(results, class = "nb")
}


#' @describeIn naive_bayes
#' Calculate Log odds for normally distributed data (a support function)
#'
#' @param x the data
#' @param mus the group means (as vectors)
#' @param sigmas the group varainces (as vectors)
#'
#' @return A numeric scalar
#' @export

log_odds <- function(x, mus, sigmas) {
    var_rat <- reduce(sigmas, `/`)
    class_dist <- map2(mus, sigmas, ~ (x - .x)^2 / .y)
    class_mat <- map_call(class_dist, cbind)
    sum(.5 * log(var_rat) + class_mat %*% c(.5, -.5))
}
