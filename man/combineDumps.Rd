\name{combineDumps}
\alias{combineDumps}

\title{
Combine MCMC samples from files
}
\description{
Takes a list of files which contain output from \code{\link{dumpJAGS}}, loads them into R and combines into a single \code{\link{mcmc.list}} object.
}
\usage{
combineDumps(fileList)
}
\arguments{
  \item{fileList}{
a list of file names as returned by \code{dumpJAGS}.
}
}

\value{
An object of class \code{mcmc.list}.
}
\author{
Mike Meredith.
}

