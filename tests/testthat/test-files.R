context('File Coverage')

test_that('File coverage works correctly (simple)', {
   before <- ls()
   cov <- ReportCoverageFiles("./FileCoverage/files/functions1.R", 
                              "./FileCoverage/tests//tests1.R")
   expect_equal(c(before, "before", "cov"), ls())
   expect_equal(row.names(cov), c("function1"))
   expect_equal(as.numeric(cov$cov[1]), 1)
})

test_that('File coverage works correctly (multiple tests and functions)', {
    before <- ls()
    cov <- ReportCoverageFiles("./FileCoverage/files/functions2.R", 
                               c("./FileCoverage/tests//tests2.R", 
                                 "./FileCoverage/tests//tests3.R"))
    expect_equal(c(before, "before", "cov"), ls())
    expect_equal(row.names(cov), c("function1"))
    expect_equal(cov$mstmt[1], 2)
    cov <- ReportCoverageFiles("./FileCoverage/files/functions2.R", 
                               c("./FileCoverage/tests//tests2.R", 
                                 "./FileCoverage/tests//tests3.R", 
                                 "./FileCoverage/tests//tests4.R"))
    expect_equal(c(before, "before", "cov"), ls())
    expect_equal(row.names(cov), c("function1"))
    expect_equal(cov$mstmt[1], 0)
    cov <- ReportCoverageFiles("./FileCoverage/files/functions2.R", 
                               c("./FileCoverage/tests2/"))
    expect_equal(c(before, "before", "cov"), ls())
    expect_equal(row.names(cov), c("function1"))
    expect_equal(cov$mstmt[1], 0)
    cov <- ReportCoverageFiles("./FileCoverage/files/functions2.R", 
                               c("./FileCoverage/tests//tests2.R", 
                                 "./FileCoverage/tests//tests3.R"), report.by.file = T)
    expect_equal(c(before, "before", "cov"), ls())
    expect_true(grepl("functions2.R", row.names(cov)[1]))
    expect_equal(cov$mstmt[1], 2)
})


