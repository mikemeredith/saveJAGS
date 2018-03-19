\name{saveJAGS-package}
\alias{saveJAGS-package}
\docType{package}
\title{
A wrapper for \code{rjags} that regularly saves the output to file.
}
\description{
Yet another wrapper for \code{rjags}, to compete with \code{jagsUI}, \code{R2jags}, \code{runjags} and the rest. The \code{saveJAGS} function runs JAGS to extract blocks of samples and saves each block to file. These files can later be loaded into R and a \code{mcmc.list} object constructed. For analyses requiring a long run-time, the output is safe even if the run is interrupted.
}

\details{

The core function, \code{saveJAGS}, works in the same way as other wrappers, accepting a list of data, initial values, a list of parameters to monitor, and the name of a text file containing the model. The user specifies the number of iterations per block and the number of blocks to extract for each chain. If run in parallel, the workers write separate files. The function returns a list of file names.

The file names are then passed to the function \code{combineSaves}, which produces an object of class \code{\link{mcmc.list}}. Utility functions are available to convert this into a list in the same format as the \code{sims.list} component in the output from other wrappers.
}

\author{
Mike Meredith

Maintainer: Mike Meredith <mmeredith@wcs.org>
}

\keyword{ package }
