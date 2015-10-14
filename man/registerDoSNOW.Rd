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
% donttest added; may fail due to closed ports. Tested via doRUnit.R
\examples{\donttest{
cl <- makeCluster(2, type="SOCK")
registerDoSNOW(cl)
m <- matrix(rnorm(9), 3, 3)
foreach(i=1:nrow(m), .combine=rbind) \%dopar\%  (m[i,] / mean(m[i,]))
stopCluster(cl)
}}
\keyword{utilities}
