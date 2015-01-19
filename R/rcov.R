#' Replace function body for coverage measurement
#'
#' Decorate function body to be able to measure coverage
#' @param f function object
#' @export
MonitorCoverage <- function(f) {
    cache$k <- 1
    cache$function.name <- deparse(substitute(f))
    b <- MonitorCoverageHelper(as.list(body(f))[-1])
    fr <- function(...){}
    formals(fr) <- formals(f)
    body(fr) <- as.call(c(as.name('{'), b))
    assign(cache$function.name, vector(length=(cache$k - 1)), envir = cov.cache)
    assign(cache$function.name, fr, envir = .GlobalEnv) 
    rm('k', envir = cache)
    rm('function.name', envir = cache)
}

#' Generic pander method
#'
#' Prints an R object in Pandoc's markdown.
#' @param stmt.list statement
#' @param ... optional parameters passed to special methods and/or raw \code{pandoc.*} functions
#' @return By default this function outputs (see: \code{cat}) the result. If you would want to catch the result instead, then call the function ending in \code{.return}.
#' @export
MonitorCoverageHelper <- function(stmt.list) {
    for (i in 1:(length(stmt.list))) {
        if (DecStatement(stmt.list[[i]])) {
            decl <- unlist(MonitorCoverageHelper(as.list(stmt.list[[i]])[-1]))
            stmt.list[[i]] <- as.call(c(stmt.list[[i]][[1]], decl))
        } else {
            stmt.list[[i]] <- as.call(c(as.name("{"), 
                                parse(text=sprintf("cov.cache$%s[%d] <- TRUE", cache$function.name, cache$k)), 
                                stmt.list[[i]]))
            cache$k <- cache$k + 1
        }
    }
    stmt.list
}

#' Replace function body for coverage measurement
#'
#' Decorate function body to be able to measure coverage
#' @param f function object
#' @export
DecStatement <- function(stmt){
    if (length(stmt) <= 1)
        return(FALSE)
    if (deparse(stmt[[1]]) %in% c('if', 'while', 'switch', '{'))
        return(TRUE)
    FALSE
}

#' Replace function body for coverage measurement
#'
#' Decorate function body to be able to measure coverage
#' @param f function object
#' @export
ReportCoverageInfo <- function(){
    if (is.null(cache$cov.data)) {
        cov.data <- data.frame(function.name = character(0), stmt = integer(0), mstmt = integer(0), cov = numeric(0))
    } else {
        cov.data <- cache$cov.data
    }
    for(func in ls(cov.cache)) {
        cov.data <- rbind(cov.data, list(func, 
                                             length(cov.cache[[func]]), 
                                             length(cov.cache[[func]]) - sum(cov.cache[[func]]), 
                                             sum(cov.cache[[func]])/length(cov.cache[[func]])))    
    }
    cov.data
}
