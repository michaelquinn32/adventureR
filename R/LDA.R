#' Linear Discriminant Analysis
#'
#' Fit a classifier using a generalization of Fischer's linear discriminant.
#'
#' The result is found using a relatively simple formula where the components of a discriminant hyper
#' plane are found separately. This makes it easier to generate predictions later.
#'
#' \deqn{f(x) = 1 \mathrm{ if } a \cdot x + b > 0}
#' \deqn{a = \Sigma^{-1} \mu_1 - \mu_0}
#' \deqn{b = -\frac{1}{2} \mu_1 ' \Sigma^{-1} \mu_1 +
#'          \frac{1}{2} \mu_0 ' \Sigma^{-1} \mu_0 +
#'          \log \frac{p_1}{p_0}}
#'
#' @param formula An R formual specifying the LDA
#' @param data An R data frame
#'
#' @return A list with the following items
#' \describe{
#'      \item{actual}{the original target values}
#'      \item{x}{the original model data matrix}
#'      \item{fit}{the class values}
#'      \item{preds}{the values of the linear combination}
#'      \item{a}{the LDA coefficients}
#'      \item{b}{the LDA threshold, or intercept}
#' }
#'
#' @export

LDA <- function(formula, data) {
    # Set up the model frame from the formula
    mf <- model.frame(formula, data)
    x <- mf[-1]
    y <- mf[[1]]

    # Checking and fixing the type of y
    if (is.factor(y)) y <- to_number(y)

    # Assertions
    stopifnot(length(unique(y)) == 2)

    # Model params
    n <- length(y)
    ns <- map(split(y, y), length)
    ps <- map(ns, ~ .x / n)
    p_rat <- reduce_right(ps, `/`)

    # Group means
    groups <- split(x, y)
    mus <- at_depth(groups, 1, map_dbl, mean)

    # Sigma hat
    centered <- map2(groups, mus, sweep, MARGIN = 2)
    cps <- map(centered, ~ crossprod(as.matrix(.x)))
    sigma_hat <- 1/ (n - 2) * reduce(cps, `+`)
    prec_hat <- solve(sigma_hat)

    # LDA Parameters
    qfs <- map_dbl(mus, ~ quadform(prec_hat, .x))
    a <- prec_hat %*% reduce_right(mus, `-`)
    b <- crossprod(qfs, c(.5, -.5)) + log(p_rat)

    # Predictions
    preds <- apply(x, 1, function(.x) crossprod(a, .x) + b)

    # Return
    results <- list(actual = y, x = x, fit = as.numeric(preds > 0), preds = preds, a = a, b = b)
    structure(results, class = "LDA")
}

#' @describeIn LDA
#' Quickly process quadratic forms (support for LDA)
#'
#' @param X a matrix
#' @param a a vector of suitable dimension
#'
#' @return A numeric scalar
#' @export

quadform <- function(X, a) {
    crossprod(a, X %*% a)
}
