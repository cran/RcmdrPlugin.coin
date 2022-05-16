\name{RcmdrcoinPlugin-package}
\alias{RcmdrcoinPlugin-package}
\alias{RcmdrcoinPlugin}
\docType{package}
\title{
Install the coin Rcmdr Plug-In
}
\description{
Rcmdr GUI extension \dQuote{plug-in} based on the coin package for an easier access to the coin package functions.
}
\details{
\tabular{ll}{
Package: \tab RcmdrcoinPlugin\cr
Type: \tab Package\cr
Version: \tab 1.0-23\cr
Date: \tab 2022-05-15\cr
License: \tab GPL (>= 2)\cr
}
}

\author{
Daniel-Corneliu Leucuta <danny.ldc@gmail.com>

Maintainer: Daniel-Corneliu Leucuta <danny.ldc@gmail.com>
}

\keyword{ package }
\seealso{
\code{\link[Rcmdr]{Rcmdr}}.
}

\examples{ 
data(alzheimer, package="coin")
lbl_test(xtabs(~gender+disease, data=alzheimer))
}
