#' Replace function body for coverage measurement
#'
#' Decorate function body to be able to measure coverage. Function object is replaced in a proper namespace
#' @param func function object to be monitored
#' @export
MonitorCoverage <- function(func) {
    if (is.character(func)) {
        getAW <- getAnywhere(func)
        if (getAW$where[1] == '.GlobalEnv') {
            if (length(getAW$where) > 1) {
                warning("Found multiple instances. Taking the one from GlobalEnv")
            } 
        } else {
            if (length(getAW$where) > 2) {
                stop("Function found in more than one package. Please supply the exact function")
            }
        }
        func.obj <- getAW$obj[[1]]
        func.name <- func
    } else if (!is.function(func)) {
        stop("Supplied argument is neither a function object or a function name")
    } else {
        func.obj <- func
        func.name <- deparse(substitute(func))
    }
    cache$k <- 1
    cache$func.name <- func.name
    func.body <- as.list(body(func.obj))
    if (func.body[[1]] != '{') {
        func.body <- as.call(c(as.name("{"), body(func.obj)))
    } 
    new.func <- function(...){}
    new.func.body <- MonitorCoverageHelper(as.list(func.body)[-1])
    formals(new.func) <- formals(func.obj)
    body(new.func) <- as.call(c(as.name('{'), new.func.body))
    func.where <- getAnywhere(func.name)$where[1]
    if (grepl('package', func.where)) {
        package.name <- gsub('package:',  "\\1", func.where)
        func.where <- getNamespace(package.name)    
        if (bindingIsLocked(func.name, func.where)) {
            unlockBinding(func.name, func.where)
            assign(cache$func.name, new.func, envir = func.where)
            lockBinding(func.name, func.where)
        } else {
            assign(cache$func.name, new.func, envir = func.where)
        }       
    } else {
        assign(cache$func.name, new.func, envir = .GlobalEnv)
    }
    
    assign(cache$func.name, vector(length=(cache$k - 1)), envir = cov.cache)
    assign(cache$func.name, func.obj, envir = func.cache)
    rm('k', envir = cache)
    rm('func.name', envir = cache)
}

#' Stop monitoring coverage for a function
#'
#' Function stop clears the annotation added to monitor coverage
#' @param func.name
#' @export
StopMonitoringCoverage <- function(func) {
    if (is.character(func)) {
        func.name <- func
    } else if (is.function(func)) {
        func.name <- deparse(substitute(func))
    } else {
        stop("Supplied argument is neither a function object or a function name")
    }
    if (func.name %in% ls(func.cache)) {
        func.where <- getAnywhere(func.name)$where[1]
        if (grepl('package', func.where)) {
            package.name <- gsub('package:',  "\\1", func.where)
            func.where <- getNamespace(package.name)    
            if (bindingIsLocked(func.name, func.where)) {
                unlockBinding(func.name, func.where)
                assign(func.name, func.cache[[func.name]], envir = func.where)
                lockBinding(func.name, func.where)
            } else {
                assign(func.name, func.cache[[func.name]], envir = func.where)
            }       
        } else {
            assign(func.name, func.cache[[func.name]], envir = .GlobalEnv)
        }
        rm(list = c(func.name), envir = func.cache)
    } else {
        stop("Function was not monitored for coverage")
    }
}

#' Add coverage annotation to statement list
#'
#' Recursive decorator of statement list that adds annotations needed to measure coverage
#' @param stmt.list statement list
#' @return annotated statement list
#' @aliases MonitorCoverage
MonitorCoverageHelper <- function(stmt.list) {
    ## TODO rename variables
    for (i in 1:(length(stmt.list))) {
        if (IsControlFlow(stmt.list[[i]])) {
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
            if (is.symbol(stmt.list[[i]]) || stmt.list[[i]][[1]] != 'switch'){
                stmt.list[[i]] <- as.call(c(as.name("{"), 
                                            parse(text=sprintf("cov.cache$%s[%d] <- TRUE", cache$func.name, cache$k)), 
                                            stmt.list[[i]]))
                cache$k <- cache$k + 1
            } else {
                temp.k <- cache$k
                cache$k <- cache$k + 1
                decl <- unlist(MonitorCoverageHelper(as.list(stmt.list[[i]])[-(1:2)]))
                stmt.list[[i]] <- as.call(c(stmt.list[[i]][[1]], stmt.list[[i]][[2]], decl))
                stmt.list[[i]] <- as.call(c(as.name("{"), 
                                            parse(text=sprintf("cov.cache$%s[%d] <- TRUE", cache$func.name, temp.k)), 
                                            stmt.list[[i]]))
            }
        }
    }
    stmt.list
}

#' Is statment is a control flow statement
#'
#' Check if statement is a control flow statement that needs special handling for coverage annotations
#' @param stmt statement to be checked
#' @return \code{TRUE/FALSE}
#' @aliases MonitorCoverageHelper
IsControlFlow <- function(stmt){
    if (length(stmt) <= 1)
        return(FALSE)
    if (deparse(stmt[[1]]) %in% c('if', 'while', '{', 'for', 'repeat'))
        return(TRUE)
    FALSE
}

#' Report Coverage Information
#'
#' Gather coverage information in a data frame and return it. Report contains following information per function - 
#' total number of statements, number of non-executed statement, line coverage percentage.
#' @return data.frame with coverage information
#' @export
ReportCoverageInfo <- function(){
    cov.data <- data.frame(stmt = integer(0), mstmt = integer(0), cov = numeric(0))
    for(func in ls(cov.cache)) {
        cov.data <- do.call(rbind, list(cov.data, data.frame(stmt = length(cov.cache[[func]]), 
                                                             mstmt = length(cov.cache[[func]]) - sum(cov.cache[[func]]), 
                                                             cov = format(100 * sum(cov.cache[[func]])/length(cov.cache[[func]]), digits = rcovOptions('digits')))))
    }
    row.names(cov.data) <- ls(cov.cache)
    cov.data
}
