\name{combineSaves}
\alias{combineSaves}

\title{
Combine MCMC samples from files
}
\description{
Takes a list of files which contain output from \code{\link{saveJAGS}}, loads them into R and combines into a single \code{\link{mcmc.list}} object.
}
\usage{
combineSaves(fileList)
}
\arguments{
  \item{fileList}{
a list of file names as returned by \code{saveJAGS}.
}
}

\value{
An object of class \code{mcmc.list}.
}
\author{
Mike Meredith.
}

