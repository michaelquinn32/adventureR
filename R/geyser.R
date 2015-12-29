#' A Simple Geyser simulation
#'
#' This function simulates the geyser scenario described in
#' \href{http://fivethirtyeight.com/tag/the-riddler/}{FiveThirtyEight's Riddler}. The
#' simulation is built from several support functions. They are all documented together.
#'
#' @details
#' The problem states that you arrive at a national park, knowing that three geysers erupt
#' at fixed intervals. You do not know when these intervals begin. What are the probabilites
#' that each geyser erupts first? The blog post for this function solved the problem
#' analytically. This code simulates the scenario to arrive at a solution.
#'
#' @param rates A named \code{list} of eruption rates. The names identify the geysers,
#'      and the values identify the number of hours between eruptions
#' @param n The number of iterations in a simulation
#' @param timeframe The time window for the eruptions; default is maximum rate
#'
#' @return A \code{geyser} objection containing the following:
#'  \describe{
#'      \item{counts}{a data frame tracking which geyser erupts first each simulation}
#'      \item{frequencies}{a data frame that counts relative rates: count / n simulations}
#'      \item{first_eruption}{a data frame of the time until each geyser's first eruption}
#'      \item{n}{the number of simulations}
#'      \item{rates}{the list of rates passed as an argument}
#'  }
#'
#' @examples
#' my_geysers <- geyser(a = 2, b = 4, c = 6)
#' simulate(my_geysers, 10)
#'
#' @export

geyser <- function(...) {
    ls <- list(...)

    # Allow for list or ... input
    if (is.recursive(ls[[1]])) rates <- unlist(ls, recursive = FALSE)
    else rates <- ls

    # Assertions
    ## No nested lists
    if (is.recursive(rates[[1]])) stop("Cannot handle nested lists")

    ## More than two geysers
    if (length(rates) < 2) stop("The number of geysers should be >= 2")

    ## Correct names
    if (is.null(names(rates))) stop("The list of eruption names must be named")
    if (anyDuplicated(names(rates)) > 0) stop("The names of the geysers must be unique")

    structure(rates, class = "geyser")
}

#' @rdname geyser
#' @export

simulate.geyser <- function(object, n, timeframe = NULL, seed = NULL) {
    # Basic assertions about n
    stopifnot(length(n) == 1)

    # Set the timeframe
    if (is.null(timeframe)) timeframe <- max(flatten_dbl(object))

    # Set the seed
    if (is.null(seed)) set.seed(seed)

    # Generate the simulated geyser eruption times
    simulations <- rerun(n, geysers(object, timeframe))

    # Find time until the first eruption of each geyser
    first <-  map2(simulations, runif(n, 0, timeframe), first_eruption)

    # Get the name of the first geyser to erupt
    first_geyser <- map(first, ~ compose(names, which.min)(.x))

    # Set the names
    nms <- set_names(names(object), names(object))

    # Count the number of eruptions for each geyser over n simulation
    counts <- count_seq(first_geyser, nms)

    # Get the frequency of eruptions
    freq <- map2(counts, seq_len(n), ~ .x / .y)

    # Create an index
    index <- seq_len(n)

    # Output results
    results <- list(counts = data.frame(n = index, do.call(rbind, counts)),
                    frequencies = data.frame(n = index, do.call(rbind, freq)),
                    first_eruption = data.frame(n = index, do.call(rbind, first)),
                    n = n,
                    rates = unclass(object))

    structure(results, class = "simulate.geyser")
}


#' @describeIn geyser Simulate the eruption of a single geyser
#' @export

eruptions <- function(rate, timeframe) {
    # Given a rate, create a random start time
    start <- runif(1, 0, rate)

    # Produce the sequence of eruptions over the timeframe (plus 1 to get all cases)
    intervals <- rep(rate, timeframe / rate + 1)
    Reduce(`+`, intervals, init = start, accumulate = TRUE)
}


#' @describeIn geyser Simulate multiple geysers, given rates
#' @export

geysers <- function(rates, timeframe) {
    # Create a list of eruptions for multiple geysers
    map(rates, eruptions, timeframe)
}


#' @describeIn geyser Find the time to each geyser's first eruption
#' @export

first_eruption <- function(geysers, arrival) {
    wait_times <- map(geysers, ~ .x - arrival)
    map_dbl(wait_times, ~ detect(.x, is.positive))
}


#' @describeIn geyser Count the number of times a level appears in a vector
#' @export

count_v <- function(level, vector) {
    sum(vector == level)
}


#' @describeIn geyser Given a vector and a set of levels, count how often
#'      each level appears
#' @export

count_seq <- function(vector, levels) {
    n <- length(vector)
    ids <- map(seq_len(n), ~ seq_len(.x))
    map(ids, function(id) map_int(levels, count_v, vector[id]))
}


#' @rdname geyser
#' @export

summary.simulate.geyser <- function(object, nwindows = 11) {
    # Get indexing value for windows, showing simulation evolution
    id <- seq(0, object$n, length.out = nwindows)

    # Combine counts and freqs
    combined <- merge(object$counts, object$frequencies,
                      by = "n", suffixes = c(".count", ".freq"))

    # Get final sample proportion (estimate probability) and se
    final <- object$frequencies[object$n, -1]
    se_prop <- map_dbl(final, ~ sqrt(.x * (1 - .x)/ object$n))

    # Get expected waiting times and standard errors
    expected <- map_dbl(object$first_eruption[-1], mean)
    se_expected <- map_dbl(object$first_eruption[-1], ~ sd(.x) / sqrt(object$n))

    results <- list(n = object$n,
                    rates = object$rates,
                    windows = combined[id, ],
                    final = flatten_dbl(final),
                    se_prop = se_prop,
                    expected = expected,
                    se_expected = se_expected,
                    nwindows = nwindows)

    structure(results, class = "summary.geyser")
}


#' @rdname geyser
#' @export

print.summary.geyser <- function(object) {
    cat("\nA geyser simulation with the following parameters:\n")
    cat("\nNumber of eruptions, per hour:\n")
    print(as.data.frame(object$rates), row.names = FALSE)
    cat("\nNumber of simulations:\n ")
    cat(object$n)
    cat("\n\nResults:\n Over time, number of first eruptions and relative frequency:\n\n")
    print(object$windows, row.names = FALSE, digits = 4)
    cat("\nRelative proportions in final states, standard errors and\napproximate 95% confidence intervals:\n\n")

    results <- data.frame("Geyser" = names(object$rates),
                          "Final Rate" = object$final,
                          "Std. Error" = object$se_prop,
                          setNames(sample_ci(object$final, object$se_prop), c("Lower", "Upper")),
                          check.names = FALSE)

    print(results, row.names = FALSE, digits = 4)

    cat("\nExpected wait times for first eruption (in hours), \nstandard errors and approximate 95% confidence intervals:\n\n")

    wait_times <- data.frame("Geyser"= names(object$rates),
                             "Expected Wait" = object$expected,
                             "Std. Error" = object$se_expected,
                             setNames(sample_ci(object$expected, object$se_expected), c("Lower", "Upper")),
                             check.names = FALSE)

    print(wait_times, row.names = FALSE, digits = 4)
}


#' @rdname geyser
#'
#' @usage
#' ## S3 method for class 'geyser'
#' autoplot(object)
#'
#' @export

autoplot.simulate.geyser <- function(object, title = "Geyser simulation results, with approximate 95% CI") {
    # Generate bounds of confidence intervals
    n <- seq_len(object$n)
    se <- map(object$frequencies[-1], ~ sqrt(.x * (1 - .x)/ n))
    upr <- object$frequencies[-1] + 1.96 * se
    lwr <- object$frequencies[-1] - 1.96 * se

    # Combine results
    out <- data.frame(n = n,
                      rate = object$frequencies[-1],
                      upr = upr,
                      lwr = lwr)


    # Reshape data for plotting
    gathered <- gather(out, variable, rate, -n)
    separated <- separate(gathered, variable, c("stat", "geyser"))
    final <- spread(separated, stat, rate)

    # Make plot
    ggplot(final, aes(x = n)) +
        geom_line(aes(y = rate, group = geyser)) +
        geom_ribbon(aes(ymin = lwr, ymax = upr, fill = geyser), alpha = .55) +
        scale_fill_brewer(palette = "Set1") +
        ylim(0, 1) +
        ggtitle(title)
}
