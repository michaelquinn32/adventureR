# Convert a factor to a numeric

to_number <- function(.factor) {
    compose(as.numeric, as.character)(.factor)
}