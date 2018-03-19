\name{combineSaves}
\alias{combineSaves}

\title{
Combine MCMC samples from files
}
\description{
Takes a list of files which contain output from \code{\link{saveJAGS}}, loads them into R and combines into a single \code{\link{mcmc.list}} object.
}
\usage{
combineSaves(fileList, params=NULL, thin=1)
}
\arguments{
  \item{fileList}{
a list of file names as returned by \code{saveJAGS}.
}
  \item{params}{
a character vector giving the names of variables to be retained; if NULL, all will be retained.
}
  \item{thin}{
the amount of additional thinning to be applied to the chains when read in.
}
}

\value{
An object of class \code{mcmc.list}.
}
\author{
Mike Meredith.
}

