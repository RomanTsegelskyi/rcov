#' Replace function body for coverage measurement
#'
#' Decorate function body to be able to measure coverage. Function object is replaced in a proper namespace
#' @param func function object to be monitored
#' @param envir environment where to place the function. Primeraly used for debugging purposes
#' @import utils
#' @export
MonitorCoverage <- function(func, envir) {
    ### TODO check if function in not found in any environment
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
    new.func <- MonitorCoverageHelper(func.obj, func.name)
    environment(new.func) <- environment(func.obj)
    if (missing(envir)){
        func.where.package <- getAnywhere(func.name)$where[2]
        if (!is.null(func.where.package) && grepl('namespace', func.where.package)) {
            package.name <- gsub('namespace:',  "\\1", func.where.package)
            func.package.namespace <- getNamespace(package.name)   
            func.package.env <- as.environment(paste("package", package.name, sep=":"))
            reassignInEnv(func.name, new.func, func.package.namespace)
            reassignInEnv(func.name, new.func, func.package.env)
        } else {
            assign(func.name, new.func, envir = .GlobalEnv)
        }
    } else {
        assign(func.name, new.func, envir = envir)
    }
    assign(func.name, vector(length=(cache$k - 1)), envir = cov.cache)
    assign(func.name, func.obj, envir = func.cache)
    rm('k', envir = cache)
    rm('func.name', envir = cache)
    invisible()
}

#' Create new function with coverage annotation
#'
#' Change function body to add statements needed to measure code coverage
#' @param func.obj function to be changed
#' @param func.name function name 
MonitorCoverageHelper <- function(func.obj, func.name) {
    cache$k <- 1
    cache$func.name <- func.name
    func.body <- as.list(body(func.obj))
    if (func.body[[1]] != '{') {
        func.body <- as.call(c(as.name("{"), body(func.obj)))
    } 
    new.func <- function(...){}
    new.func.body <- CoverageAnnotationDecorator(as.list(func.body)[-1])
    formals(new.func) <- formals(func.obj)
    body(new.func) <- as.call(c(as.name('{'), new.func.body))
    new.func
}

#' Stop monitoring coverage for a function
#'
#' Function stop clears the annotation added to monitor coverage
#' @param func function or function name
#' @param envir environment where function with coverage annotation was placed. Primeraly used for debugging purposes
#' @export
StopMonitoringCoverage <- function(func, envir) {
    if (is.character(func)) {
        func.name <- func
    } else if (is.function(func)) {
        func.name <- deparse(substitute(func))
    } else {
        stop("Supplied argument is neither a function object or a function name")
    }
    if (func.name %in% ls(func.cache)) {
        if (missing(envir)) {
            func.where.package <- getAnywhere(func.name)$where[1]
            if (grepl('package', func.where.package)) {
                package.name <- gsub('package:',  "\\1", func.where.package)
                func.package.namespace <- getNamespace(package.name)   
                func.package.env <- as.environment(func.where.package)
                reassignInEnv(func.name, func.cache[[func.name]], func.package.namespace)
                reassignInEnv(func.name, func.cache[[func.name]], func.package.env)
            } else {
                assign(func.name, func.cache[[func.name]], envir = .GlobalEnv)
            }
        } else {
            assign(func.name, func.cache[[func.name]], envir = envir)
        }
        rm(list = c(func.name), envir = func.cache)
        rm(list = c(func.name), envir = cov.cache)
    } else {
        stop("Function was not monitored for coverage")
    }
}

#' Add coverage annotation to statement list
#'
#' Recursive decorator of statement list that adds annotations needed to measure coverage
#' @param stmt.list statement list
#' @return annotated statement list
CoverageAnnotationDecorator <- function(stmt.list) {
    ## TODO rename variables
    for (i in 1:(length(stmt.list))) {
        if (IsControlFlow(stmt.list[[i]])) {
            if (stmt.list[[i]][[1]] != 'for') {
                decl <- unlist(CoverageAnnotationDecorator(as.list(stmt.list[[i]])[-1]))
                stmt.list[[i]] <- as.call(c(stmt.list[[i]][[1]], decl))
            } else {
                temp.k <- cache$k
                cache$k <- cache$k + 1
                decl <- unlist(CoverageAnnotationDecorator(as.list(stmt.list[[i]])[-(1:3)]))
                stmt.list[[i]] <- as.call(c(stmt.list[[i]][[1]], stmt.list[[i]][[2]],  stmt.list[[i]][[3]], decl))
                stmt.list[[i]] <- as.call(c(as.name("{"), 
                                            parse(text=sprintf("rcov:::SetExecuteValue('%s', %d)", cache$func.name, temp.k)), 
                                            stmt.list[[i]]))
            }
        } else {
            if (is.symbol(stmt.list[[i]]) || stmt.list[[i]][[1]] != 'switch'){
                stmt.list[[i]] <- as.call(c(as.name("{"), 
                                            parse(text=sprintf("rcov:::SetExecuteValue('%s', %d)", cache$func.name, cache$k)), 
                                            stmt.list[[i]]))
                cache$k <- cache$k + 1
            } else {
                temp.k <- cache$k
                cache$k <- cache$k + 1
                decl <- unlist(CoverageAnnotationDecorator(as.list(stmt.list[[i]])[-(1:2)]))
                stmt.list[[i]] <- as.call(c(stmt.list[[i]][[1]], stmt.list[[i]][[2]], decl))
                stmt.list[[i]] <- as.call(c(as.name("{"), 
                                            parse(text=sprintf("rcov:::SetExecuteValue('%s', %d)", cache$func.name, temp.k)), 
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

#' Write down that line was executed
#'
#' Record that particual line was executed. Used in statement coverage, needed for namespace replacement
#' @param func.name function name
#' @param line.number line.number
SetExecuteValue <- function(func.name, line.number) {
    cov.cache[[func.name]][line.number] <- TRUE
}
