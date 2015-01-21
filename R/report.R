#' Measure coverage of files using tests
#'
#' Monitor coverage for functions while executing test files
#' @param source.files files with function objects to be monitored for coverage
#' @param execution.files files with tests
#' @export
ReportCoverageFiles <- function(source.files, execution.files) {
    original.objects <- ls(envir = .GlobalEnv)
    for (sfile in source.files) {
        source(sfile)
    }
    added.objects <- setdiff(ls(envir = .GlobalEnv), original.objects)
    for (func in added.objects){
        func.obj <- getAnywhere(func)$obj[[1]]
        if (is.function(func.obj))
            MonitorCoverage(func)
    }
    if (length(execution.files) < 1) { stop("No execution files") }
    for (efile in execution.files) {
        source(efile)
    }
    ReportCoverageInfo()
}