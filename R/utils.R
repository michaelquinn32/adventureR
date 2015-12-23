# Convert a factor to a numeric
to_number <- function(.factor) {
    compose(as.numeric, as.character)(.factor)
}

# Make a confidence interval for an estimate
sample_ci <- function(estimate, se, conf.level = .95) {
    list(lwr = estimate + qnorm((1 - conf.level)/2, sd = se),
         upr = estimate + qnorm((1 + conf.level)/2, sd = se))
}

# A predicate function to see if a value is greater than 0
is.positive <- function(x) {
    x > 0
}