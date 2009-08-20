\name{registerDoSNOW}
\alias{registerDoSNOW}
\title{registerDoSNOW}
\description{
The \code{registerDoSNOW} function is used to register the SNOW parallel backend
with the foreach package.
}
\usage{
registerDoSNOW(cl)
}
\arguments{
  \item{cl}{The cluster object to use for parallel execution.}
}
\seealso{
  \code{\link[snow]{makeCluster}}
}

\keyword{utilities}
