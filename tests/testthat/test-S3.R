context('S3')
require(devtools, quite = T)
devtools::load_all("./PackageS3", export_all = FALSE, quiet = TRUE, recompile = TRUE)

test_that('Custom package S3 replacement works', {
    require(PackageS3, quiet = T)
    expect_equal(length(methods(testS3)), 3)
    MonitorCoverage("testS3.numeric", "PackageS3")
    expect_equal(length(methods(testS3)), 3)
    expect_equal(utils::getAnywhere("testS3.logical")$where, c("registered S3 method for testS3 from namespace PackageS3", "namespace:PackageS3"))
    expect_equal(PackageS3:::testS3.numeric, function (x, ...) 
    {
{
    rcov:::SetExecuteValue("testS3.numeric", 1)
    cat("Numeric version called\n")
}
    })
    MonitorCoverage("testS3.logical")
    MonitorCoverage("testS3.default")
    expect_output(testS3(numeric()), "Numeric version called")
    expect_output(testS3(logical()), "Logical version is called")
    expect_output(testS3(data.frame()), "Default version is called")
    cov.data <- ReportCoverageInfo()
    expect_true(all(cov.data$mstmt == 0))
    expect_true(all(cov.data$cov == 1))
    StopMonitoringCoverage("testS3.logical")
    StopMonitoringCoverage("testS3.numeric")
    StopMonitoringCoverage("testS3.default")
    expect_equal(length(methods(testS3)),3)
    expect_equal(PackageS3:::testS3.numeric, function (x, ...) 
    {
    cat("Numeric version called\n")
    })
    expect_equal(length(ls(rcov:::func.cache)), 0)
    expect_equal(length(ls(rcov:::cov.cache)), 0)
})

test_that('Base package S3 method replacement works correctly', {
    expect_equal(length(methods(mean)), 5)
    old.mean <- mean.default
    MonitorCoverage(mean.Date)
    MonitorCoverage(mean.default)
    expect_equal(utils::getAnywhere("mean.Date")$where, 
                 c("package:base", "registered S3 method for mean from namespace base", "namespace:base"))
    expect_equal(mean.Date, function (x, ...) 
    {
{
    rcov:::SetExecuteValue("mean.Date", 1)
    structure(mean(unclass(x), ...), class = "Date")
}
    })
    expect_equal(mean.default, function (x, trim = 0, na.rm = FALSE, ...) 
    {
        if ({
            rcov:::SetExecuteValue("mean.default", 1)
            !is.numeric(x) && !is.complex(x) && !is.logical(x)
        }) {
{
    rcov:::SetExecuteValue("mean.default", 2)
    warning("argument is not numeric or logical: returning NA")
}
{
    rcov:::SetExecuteValue("mean.default", 3)
    return(NA_real_)
}
        }
if ({
    rcov:::SetExecuteValue("mean.default", 4)
    na.rm
}) {
    rcov:::SetExecuteValue("mean.default", 5)
    x <- x[!is.na(x)]
}
if ({
    rcov:::SetExecuteValue("mean.default", 6)
    !is.numeric(trim) || length(trim) != 1L
}) {
    rcov:::SetExecuteValue("mean.default", 7)
    stop("'trim' must be numeric of length one")
}
{
    rcov:::SetExecuteValue("mean.default", 8)
    n <- length(x)
}
if ({
    rcov:::SetExecuteValue("mean.default", 9)
    trim > 0 && n
}) {
    if ({
        rcov:::SetExecuteValue("mean.default", 10)
        is.complex(x)
    }) {
        rcov:::SetExecuteValue("mean.default", 11)
        stop("trimmed means are not defined for complex data")
    }
    if ({
        rcov:::SetExecuteValue("mean.default", 12)
        anyNA(x)
    }) {
        rcov:::SetExecuteValue("mean.default", 13)
        return(NA_real_)
    }
    if ({
        rcov:::SetExecuteValue("mean.default", 14)
        trim >= 0.5
    }) {
        rcov:::SetExecuteValue("mean.default", 15)
        return(stats::median(x, na.rm = FALSE))
    }
{
    rcov:::SetExecuteValue("mean.default", 16)
    lo <- floor(n * trim) + 1
}
{
    rcov:::SetExecuteValue("mean.default", 17)
    hi <- n + 1 - lo
}
{
    rcov:::SetExecuteValue("mean.default", 18)
    x <- sort.int(x, partial = unique(c(lo, hi)))[lo:hi]
}
}
{
    rcov:::SetExecuteValue("mean.default", 19)
    .Internal(mean(x))
}
    })
    data <- list()
    for (i in 1:10)
        data[[i]] <- rnorm(10)
    expect_equal(lapply(data, mean), lapply(data, old.mean))
    StopMonitoringCoverage("mean.Date")
    StopMonitoringCoverage("mean.default")
    expect_equal(length(methods(mean)),5)
    expect_equal(mean.Date, function (x, ...) 
        structure(mean(unclass(x), ...), class = "Date"))
    expect_equal(length(ls(rcov:::func.cache)), 0)
    expect_equal(length(ls(rcov:::cov.cache)), 0)
})


