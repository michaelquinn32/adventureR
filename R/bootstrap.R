#' Experimenting with sampling and calculating confidence intervals in bootstrapping
#'
#' When bootstrapping, we are traditionally instructed to generate a sample equal in
#' size to the entire dataset. We can shortcut this method, as long as we are willing to
#' make certain adjustments when calculating confidence intervals. Two alternative
#' sampling methods are tested here, one with replacement and the other without.
#'
#' The primary functions simulate data for estimating bootstrapped CIs for the mean of a
#' normally-distributed vector. They accept ranges for the simulation parameters. These
#' are described in the parameters. The experiment is generic in that you can pass it
#' multiple different functions for calculating confidence intervals in a list. When
#' these functions have different parameters, it is usually necessary to have \code{...}
#' in all of their arguments.
#'
#' The simulation is split across one function to generate parameters and iterators,
#' and a second function that executes a single iteration. That function also relies
#' on a generic bootstrap simulator.
#'
#' @param funs a list of functions for calculating confidence intervals
#' @param nrange the length of the vector
#' @param rrange the portion of the vector's length used in each subsample
#' @param mus the mean of the sampling distribution
#' @param sigmas the variance of the sampling distribution
#' @param bsi_range the number of bootstrap iterations
#' @param length.out the number of parameters drawn from the range
#' @param niter the number of repetitions for each parameter combination
#'
#' @return
#' The function \code{experiment} generates the test, which results in a data frame
#' that contains:
#'
#' \itemize{
#'      \item{the confidence interval method}
#'      \item{whether replacement was used in sampling}
#'      \item{the upper and lower limits of the confidence interval}
#'      \item{the combination of sampling parmaters}
#'      \item{the bias of the upper and lower CI limits}
#'      \item{the sum of the square biases}
#' }
#'
#' @examples
#' \dontrun{
#'  red_se <- function(x, probs = c(.025, .975), n, b, mu) {
#'      se <- sd(x) * sqrt(b / n)
#'      qnorm(probs, mu, se)
#'  }
#'
#'  funs <- list(se = red_se)
#'  test <- bootstrap_experiment(funs, length.out = 2, niter = 2)
#' }
#'
#'  @export

bootstrap_experiment <- function(funs,
                                 nrange = c(1000, 10000),
                                 rrange = c(.25, .75),
                                 mus = c(0, 4.5),
                                 sigmas = c(1, 5.5),
                                 bsi_range = c(1000, 10000),
                                 length.out = 5,
                                 niter = 5) {

    # Either a single shared length for each range of params, or individual lengths
    if (length(length.out == 1)) length.out <- rep.int(length.out, 5)

    # Create ranges of values
    params <- list(n = nrange, r = rrange, mu = mus, sigma = sigmas, bsi = bsi_range)
    ranges <- map2(params, length.out, ~ seq(.x[1], .x[2], length.out = .y))
    ranges$i <- seq_len(niter)
    ranges$replace <- c(TRUE, FALSE)

    # Create all test combinations
    combinations <- cross_n(ranges)

    # Execute all tests in parallel
    tests <- map(combinations, compare_methods, funs)
    out <- bind_rows(tests, .id = "test")
    mutate(out, bias.lwr = (lwr - true.lwr), bias.upr = upr - true.upr,
           bias.sqr = bias.lwr^2 + bias.upr^2)
}

#' @describeIn bootstrap_experiment One iteration of bootstrap experiment
#' @export

compare_methods <- function(param, funs) {
    # Params
    n <- param$n
    b <- n * param$r
    nms <- c("lwr", "upr")

    # Simulated vector
    x <- rnorm(n, param$mu, param$sigma)

    # Get the true standard error and quantile
    mu <- mean(x)
    se <- sd(x) / sqrt(n)
    true <- map(c(lwr = .025, upr = .975), qnorm, mu, se)

    # Simulation
    sim <- bs_sim(x, param$bsi, replace = param$replace, funs, n = n, b = b, mu = mu)

    # Output
    param_cbm <- c(param, true = true)
    param_rep <- do.call(rbind.data.frame, rerun(length(sim[[1]]), param_cbm))
    cbind(sim, param_rep)
}

#' @describeIn bootstrap_experiment A generic bootstrap estimator
#' @export
bs_sim <- function(x, bsi, replace, funs, n, b, ..., probs = c(0.025, 0.975)) {
    # Generate random sample estimates
    ids <- rerun(bsi, sample.int(n, size = b, replace = replace))
    estimates <- map_dbl(ids, ~ mean(x[.x]))

    # Calculate confidence intervals
    all_args <- list(list(x = estimates, probs = probs, n = n, b = b, ...))
    ints <- invoke_map(funs, all_args)

    # Generate results
    res <- invoke(rbind.data.frame, ints)
    res_nmd <- set_names(res, c('lwr', 'upr'))
    nmd <- add_rownames(res_nmd, var = 'method')
    nmd$replace <- replace
    nmd
}

#' @describeIn bootstrap_experiment Adjusting standard errors for smaller samples
#' @export

red_se <- function(x, probs = c(.025, .975), n, b, mu) {
    se <- sd(x) * sqrt(b / n)
    qnorm(probs, mu, se)
}

#' @describeIn bootstrap_experiment Subsamping quantiles (using differences)
#' @export

ss_quantiles <- function(x, probs = c(.025, .975), n, b, mu) {
    z.star <- sqrt(b) * (x - mu)
    qntls <- quantile(z.star, probs[2:1])
    mu - qntls / sqrt(n)
}

#' @describeIn bootstrap_experiment Subsamping quantiles (with empirical distribution function)
#' @export

quantile_ev <- function(x, probs = c(.025, .975), n, b, mu, int = c(-500, 500)) {
    vapply(probs, qntl_solve, numeric(1), x, mu, b, n, int)
}

qntl_solve <- function(q, ss_stat, samp_stat, b, n, int) {
    root <- uniroot(emp_dist, int, q, ss_stat, samp_stat, b)$root
    samp_stat + root / sqrt(n)
}

emp_dist <- function(t, q, ss_stat, samp_stat, b) {
    id <- sqrt(b) * (ss_stat - samp_stat) <= t
    mean(id) - q
}

#' @describeIn  bootstrap_experiment Another confidence interval function
se_ci <- function(x, probs = c(.025, .975), ...) {
    qnorm(probs, mean(x), sd(x))
}

#' @describeIn bootstrap_experiment A curried version of quantile to catch unnecessary arguments
qntl <- function(x, probs = c(.025, .975), ...) {
    quantile(x, probs)
}