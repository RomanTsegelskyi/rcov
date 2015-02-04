#' @export
testS3 <- function(x, ...){
    UseMethod('testS3', x)
}

#' @export
testS3.numeric <- function(x, ...) {
    cat("Numeric version called\n")
}

#' @export
testS3.logical <- function(x, ...) {
    cat("Logical version is called\n")
}

#' @export
testS3.default <- function(x, ...) {
    cat("Default version is called\n")
}