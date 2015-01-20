#' Replace function body for coverage measurement
#'
#' Decorate function body to be able to measure coverage
#' @param f function object
#' @export
MonitorCoverage <- function(func) {
    cache$k <- 1
    cache$func.name <- deparse(substitute(func))
    func.body <- MonitorCoverageHelper(as.list(body(func))[-1])
    new.func <- function(...){}
    formals(new.func) <- formals(func)
    body(new.func) <- as.call(c(as.name('{'), func.body))
    assign(cache$func.name, vector(length=(cache$k - 1)), envir = cov.cache)
    assign(cache$func.name, new.func, envir = .GlobalEnv)
    assign(cache$func.name, func, envir = func.cache)
    rm('k', envir = cache)
    rm('func.name', envir = cache)
}

#' Replace function body for coverage measurement
#'
#' Decorate function body to be able to measure coverage
#' @param f function object
#' @export
StopMonitoringCoverage <- function(func.name) {
    if (func.name %in% ls(func.cache)) {
        assign(func.name, func.cache$func.name, envir = .GlobalEnv)
        rm(func.name, envir = func.cache)
    } else {
        stop("Function was not monitored for coverage")
    }
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
            if (stmt.list[[i]][[1]] != 'for') {
                decl <- unlist(MonitorCoverageHelper(as.list(stmt.list[[i]])[-1]))
                stmt.list[[i]] <- as.call(c(stmt.list[[i]][[1]], decl))
            } else {
                temp.k <- cache$k
                cache$k <- cache$k + 1
                decl <- unlist(MonitorCoverageHelper(as.list(stmt.list[[i]])[-(1:3)]))
                stmt.list[[i]] <- as.call(c(stmt.list[[i]][[1]], stmt.list[[i]][[2]],  stmt.list[[i]][[3]], decl))
                stmt.list[[i]] <- as.call(c(as.name("{"), 
                                            parse(text=sprintf("cov.cache$%s[%d] <- TRUE", cache$func.name, temp.k)), 
                                            stmt.list[[i]]))
            }
        } else {
            stmt.list[[i]] <- as.call(c(as.name("{"), 
                                parse(text=sprintf("cov.cache$%s[%d] <- TRUE", cache$func.name, cache$k)), 
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
    if (deparse(stmt[[1]]) %in% c('if', 'while', 'switch', '{', 'for', 'repeat'))
        return(TRUE)
    FALSE
}

#' Replace function body for coverage measurement
#'
#' Decorate function body to be able to measure coverage
#' @param f function object
#' @export
ReportCoverageInfo <- function(){
    cov.data <- data.frame(stmt = integer(0), mstmt = integer(0), cov = numeric(0))
    for(func in ls(cov.cache)) {
        cov.data <- do.call(rbind, list(cov.data, data.frame(stmt = length(cov.cache[[func]]), 
                                             mstmt = length(cov.cache[[func]]) - sum(cov.cache[[func]]), 
                                             cov = sum(cov.cache[[func]])/length(cov.cache[[func]]))))    
    }
    row.names(cov.data) <- ls(cov.cache)
    cov.data
}
