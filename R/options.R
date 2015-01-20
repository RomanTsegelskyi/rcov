.onLoad <- function(libname, pkgname)
{
    require(utils)
    ## rcov settings
    options('rcov' = list(
        'digits'                   = 4,
        'decimal.mark'             = '.'
    ))
}

.onUnload <- function(libpath) {
    for (func.name in ls(func.cache)) {
        StopMonitoringCoverage(func.name)
    }
}

# temporary solution for testing
.onAttach <- function(libname, pkgname){
    rm(list=ls(.GlobalEnv), envir = .GlobalEnv)
    source("R/testf.R")
}

.onDetach <- function(libpath) {
    for (func.name in ls(func.cache)) {
        StopMonitoringCoverage(func.name)
    }
}

## general (temporary) storage for rcov's stuff
cov.cache <- new.env()
cache <- new.env()
func.cache <- new.env()

#' Querying/setting rcov option
#'
#' To list all \code{rcov} options, just run this function without any parameters provided. To query only one value, pass the first parameter. To set that, use the \code{value} parameter too.
#'
#' The following \code{rcov} options are available:
#'
#' \itemize{
#'      \item \code{digits}: numeric (default: \code{2}) passed to \code{format}
#'      \item \code{decimal.mark}: string (default: \code{.}) passed to \code{format}
#' }
#' @param o option name (string). See below.
#' @param value value to assign (optional)
#' @export
#' @seealso \code{\link{evalsOptions}}
#' @aliases rcov.option
#' @note \code{rcov.option} is deprecated and is to be removed in future releases.
#' @examples \dontrun{
#' rcovOptions()
#' rcovOptions('digits')
#' rcovOptions('digits', 5)
#' }
rcovOptions <- function(o, value) {
    
    res <- getOption('rcov')
    
    ## just querying
    if (missing(value)) {
        
        if (missing(o))
            return(res)
        
        if (o %in% names(res))
            return(res[[o]])
        
        pandoc.header('Possible `rcov` options:', style = 'setext')
        pandoc.list(names(res))
        stop('Wrong option queried.')
        
    } else {
        
        if (!o %in% names(res))
            stop(paste('Invalid option name:', o))
        
        res[[o]] <- value
        options('rcov' = res)
        
    }
    
}

#' @export rcov.option
rcov.option <- function(x, ...) {
    mc <- match.call(rcovOptions)
    mc[[1]] <- quote(rcovOptions)
    eval(mc)
}
