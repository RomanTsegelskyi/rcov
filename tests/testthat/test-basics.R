context('basic.coverage')

functions <- list(slow.power = 
slow.power <- function(x, p) {
    r <- 1
    if (p >= 1) {
        for (i in 1:(p - 1))
            r <- r * x
    }
    r
}, slow.power.mot = 
slow.power.mot <- function (x, p) 
{
{
    rcov:::SetExecuteValue("slow.power", 1)
    r <- 1
}
if ({
    rcov:::SetExecuteValue("slow.power", 2)
    p >= 1
}) {
{
    rcov:::SetExecuteValue("slow.power", 3)
    for (i in 1:(p - 1)) {
        rcov:::SetExecuteValue("slow.power", 4)
        r <- r * x
    }
}
}
{
    rcov:::SetExecuteValue("slow.power", 5)
    r
}
})

test_that('Monitor coverage works correctly', {
    expect_that(MonitorCoverageHelper(slow.power, 'slow.power'), is_a('function'))
    expect_null(cov.cache$slow.power)
    expect_equal(MonitorCoverageHelper(functions$slow.power, 'slow.power'), functions$slow.power.mot)
    slow.power <- functions$slow.power
    MonitorCoverage(slow.power, environment())
    expect_equal(slow.power, slow.power.mot)
    expect_equal(func.cache$slow.power, functions$slow.power)
    expect_true(!is.null(cov.cache$slow.power))
    expect_equal(length(cov.cache$slow.power), 5)
    StopMonitoringCoverage(slow.power, environment())
    expect_null(cov.cache$slow.power)
    expect_null(func.cache$slow.power)
    rm(slow.power)
})

