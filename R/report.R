
#' Measure coverage of files using tests
#'
#' Monitor coverage for functions while executing test files
#' @param source.files files with function objects to be monitored for coverage
#' @param execution.files files with tests
#' @param ... for specific tests separeted by commas
#' @param report.by.file if report should be by file or by function
#' @export
ReportCoverageFiles <- function(source.files, execution.files, ..., report.by.file = FALSE) {
    ## TODO add try catch for error
    ## What happens if objects already exist?
    original.objects <- ls(envir = .GlobalEnv)
    tests <- eval(substitute(alist(...)))
    function.to.file <- list()
    added.objects <- vector()
    # source files in the .GlobalEnv and start monitoring coverage
    for (sfile in source.files) {
        source(sfile)
        function.to.file[[sfile]] <- setdiff(ls(envir = .GlobalEnv), c(added.objects, original.objects)) 
        added.objects <- c(added.objects, function.to.file[[sfile]])
    }
#     added.objects <- setdiff(ls(envir = .GlobalEnv), original.objects)
    for (func in added.objects){
        func.obj <- getAnywhere(func)$obj[[1]]
        if (is.function(func.obj))
            MonitorCoverage(func)
    }
    # Execute tests
    if (length(execution.files) < 1 && length(tests) < 1) { stop("No execution files") }
    for (efile in execution.files) {
        finfo <- file.info(efile)
        if (finfo$isdir) {
            for (f in list.files(efile, full.names = T))
                source(f)
        } else if (file.exists(efile)) {
            source(efile)
        } else {
            warning(paste(efile, "does not exist"))
        }
    }
    for (test in tests) {
        eval(test)
    }
    res <- ReportCoverageInfo()
    # Cleaning
    for (func in added.objects){
        func.obj <- getAnywhere(func)$obj[[1]]
        if (is.function(func.obj))
            StopMonitoringCoverage(func)
    }
    rm(list=added.objects, envir = .GlobalEnv)
    if (report.by.file) {
        new.res <- data.frame(stmt = integer(0), mstmt = integer(0), cov = numeric(0))
        for (file in names(function.to.file)) {
            fres <- res[function.to.file[[file]], ]    
            new.res[file, ] <- data.frame(stmt = sum(fres$stmt), 
                                                     mstmt = sum(fres$mstmt),  
                                                     cov = (sum(fres$stmt) - sum(fres$mstmt))/sum(fres$stmt))
        }
        res <- new.res
    }
    res
}


#' Measure coverage of a package
#'
#' Monitor coverage of a package
#' @param package.dir package directory
#' @param ... for specific tests separeted by commas
#' @export
ReportPackageCoverage <- function(package.dir, ...) {
    if (!file.exists(package.dir)){
        return(NULL)
    }
    test.dir <- GetTestDir(package.dir)
    tests <- eval(substitute(alist(...)))
    if (file.exists(test.dir) || length(tests) > 0) {
        ns <- devtools::load_all(package.dir, export_all = FALSE, quiet = TRUE, recompile = TRUE)$env
        package.name <- ns$.packageName
        env <- new.env(parent = ns)
        tests <-
            c(tests,
              if (file.exists(test.dir)) {
                  bquote(try(testthat::source_dir(path = .(test.dir), env = .(env))))
              })
        res <- ReportEnvironmentCoverage(ns, package.name, tests, enclos = .GlobalEnv)
    }
    res
}

#' Measure coverage of an environment
#'
#' Monitor coverage of an environment using tests
#' @param envir environment to monitor coverage in
#' @param ... for specific tests separeted by commas
#' @param enclos environment where tests should be evaluated
#' @export
ReportEnvironmentCoverage <- function(envir, package.name, tests, enclos = parent.frame()) {
#     tests <- eval(substitute(alist(...)))
    objects <- ls(envir)
    sapply(objects, MonitorCoverage, package.name)
    for (test in tests) {
        eval(test, enclos)
    }
    res <- ReportCoverageInfo()
    sapply(objects, StopMonitoringCoverage, package.name)
    res
}

