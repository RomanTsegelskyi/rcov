[![Build Status](https://travis-ci.org/RomanTsegelskyi/rcov.svg?branch=master)](https://travis-ci.org/RomanTsegelskyi/rcov)
# RCov - Coverage of R code

The main aim of the *rcov* [R](http://r-project.org) package is to provide a tool for measuring different types of code coverage for R code. 

# Installation

Development of package just started recently and is far away from CRAN release yet.

It can be installed easily with the nifty function of the `devtools` package from [CRAN](http://cran.r-project.org/web/packages/devtools/index.html):

```r
library(devtools)
install_github('rcov', 'RomanTsegelskyi')
```

Or download the [sources](https://github.com/RomanTsegelskyi/rcov/archive/master.zip) and build manually. If you're running R on Windows, you need to install [Rtools](http://cran.stat.ucla.edu/bin/windows/Rtools/).

# Basic Usage

Function is the smallest building block for code coverage measure. To start monitoring function for coverage information, it needs to be decorated first by `MonitorCoverage`. To see the coverage information call `ReportCoverageInfo`. 

```r
f <- function(x) {
    if (x %% 2 == 0){
        x <- x + 20
    } else {
        x <- 10
    }
    x
}
MonitorCoverage(f)
f(1) 
ReportCoverageInfo()
f(2)
ReportCoverageInfo()
```

# More reporting options

`ReportCoveragePercentage` reports mean of coverage percentage accross all function that coverage is being monitored for.

`ReportMissingStatements` returns a list of statements that were not executed for function whose coverage is being monitored.

```r
MonitorCoverage(agrep)
MonitorCoverage(abbreviate)
examples(agrep)
# get mean of coverage percentage
ReportCoveragePercentage()
# list statements that were not covered
ReportMissingStatements()
```
# Alternative R Code Coverage Projects
There are some other projects for R code coverage that I am aware of:

* https://github.com/MangoTheCat/testCoverage
* http://r2d2.quartzbio.com/posts/r-coverage-docker.html
* https://github.com/jimhester/covr
