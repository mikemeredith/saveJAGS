\name{combineSaves}
\alias{mcmcOutput.saveJAGSfileList}
\alias{as.mcmc.list.saveJAGSfileList}
\alias{combineSaves}

\title{
Combine MCMC samples from files
}
\description{
Takes a list of files which contain output from \code{\link{saveJAGS}}, loads them into R and combines into a single \code{\link{mcmcOutput}} or \code{\link{mcmc.list}} object.

\code{combineSaves} is now superseded by \code{as.mcmc.list} and will be removed in a future version.
}
\usage{
\method{mcmcOutput}{saveJAGSfileList}(object, header,
    params=NULL, thin=1, burnFiles=0, cores=1, ...)
\method{as.mcmc.list}{saveJAGSfileList}(x,
    params=NULL, thin=1, burnFiles=0, cores=1, ...)

combineSaves(fileList, params=NULL, thin=1, burnFiles=0, cores=1)
}
\arguments{
  \item{object, x, fileList}{
a list of file names as returned by \code{saveJAGS} or \code{recoverSaves}.
}
  \item{header}{
text to use as the header by \code{print} and \code{summary} methods.
}
  \item{params}{
a character vector giving the names of parameters to be retained; if NULL, all will be retained.
}
  \item{thin}{
the amount of additional thinning to be applied to the chains when read in.
}
  \item{burnFiles}{
the number of initial files to discard as burn-in.
}
  \item{cores}{
the maximum number of cores (threads) to use; the number used is the minimum of (a) \code{cores}, (b) the number of chains to process, (c) the number of cores available on the machine. Parallel processing uses more memory and may fail if files are very large.
}
  \item{\dots}{
other arguments to pass to the method.
}
}

\value{
An object of class \code{mcmcOutput} or \code{mcmc.list}.
}
\author{
Mike Meredith.
}

