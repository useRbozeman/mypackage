\name{MethodsOfDataAnalysis-package}
\alias{MethodsOfDataAnalysis-package}
\alias{MethodsOfDataAnalysis}
\docType{package}
\title{
A collection of random things
}
\description{
Functions I've written over the years that I find usefull.
}
\details{
\tabular{ll}{
Package: \tab MethodsOfDataAnalysis\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2017-08-27\cr
License: \tab What license is it under?\cr
}

}
\author{
Jordan Schupbach

Maintainer: Jordan Schupbach <jordans1882@gmail.com>
}
\references{
reference section
}

\keyword{ package }
\seealso{
 Optional links to other man pages, e.g. 
}
\examples{
A <- matrix(rnorm(10000), 100, 100)
B <- matrix(rnorm(10000), 100, 100)
system.time(AB <- mat_mult(A,B))
}
