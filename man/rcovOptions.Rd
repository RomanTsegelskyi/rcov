% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/options.R
\name{rcovOptions}
\alias{rcov.option}
\alias{rcovOptions}
\title{Querying/setting rcov option}
\usage{
rcovOptions(o, value)
}
\arguments{
\item{o}{option name (string). See below.}

\item{value}{value to assign (optional)}
}
\description{
To list all \code{rcov} options, just run this function without any parameters provided. To query only one value, pass the first parameter. To set that, use the \code{value} parameter too.
}
\details{
The following \code{rcov} options are available:

\itemize{
     \item \code{digits}: numeric (default: \code{2}) passed to \code{format}
     \item \code{decimal.mark}: string (default: \code{.}) passed to \code{format}
}
}
\note{
\code{rcov.option} is deprecated and is to be removed in future releases.
}
\examples{
\dontrun{
rcovOptions()
rcovOptions('digits')
rcovOptions('digits', 5)
}
}

