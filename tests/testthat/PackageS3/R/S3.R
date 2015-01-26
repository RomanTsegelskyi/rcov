#' @export
test <- function(x, ...){
    UseMethod('test', x)
}

#' @export
test.numeric <- function(x, ...) {
    cat("Numeric version called")
}

#' @export
test.logical <- function(x, ...) {
    cat("Logical version is called")
}

#' @export
test.default <- function(x, ...) {
    cat("Default version is called")
}