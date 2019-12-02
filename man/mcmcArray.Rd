\name{mcmcArray-class}
\alias{mcmcArray-class}
\alias{mcmcArray}
\alias{mcmcArray.default}
\alias{mcmcArray.mcmc.list}
\alias{mcmcArray.jagsUI}

\title{
Conversion to class mcmcArray
}
\description{
Convert output containing MCMC chains to the class \code{mcmcArray}. The function is generic, with methods for a range of input objects.

This class is still experimental and specs may be changed in future versions.
}
\usage{
mcmcArray(object, ...)

\method{mcmcArray}{default}(object, ...)

\method{mcmcArray}{mcmc.list}(object, ...)

\method{mcmcArray}{jagsUI}(object, ...)

}

\arguments{
  \item{object}{
an object containing the MCMC chains; see Details.
}
  \item{\dots}{
named parameters to be passed to other methods (currently not used).
}
}

\details{
\code{mcmcArray} objects store the output from MCMC estimation runs in a compact and easily accessible format. Several customised extraction methods are available:

  \tabular{ll}{
  \code{$} \tab  : extracts arrays for individual parameters in the same way as a \code{sims.list}. \cr
  \code{[} with 1 index \tab  : returns the selected columns(s). \cr
  \code{[} with 2 indices \tab  : returns the selected row(s) and columns(s). \cr
  \code{[} with 3 indices \tab  : behaves as an iterations x chains x parameters array.}
}

\value{
An object of class \code{mcmcArray}. This is a matrix with a column for the MCMC chain for each parameter. The first 3 attributes in the list below must be present, the rest are optional but may be used by \code{print} or \code{summary} methods:

  \tabular{ll}{
  \code{nChains} \tab  the number of chains. \cr
  \code{simsList} \tab  a list specifying which columns correspond to each parameter. \cr
  \code{summary} \tab  a data frame with summary statistics for all parameters. \cr
  \code{header} \tab text to be displayed as the first line when the object is printed. \cr
  \code{call} \tab the original function call. \cr
  \code{n.eff} \tab  a vector of effective sample sizes for the parameters. \cr
  \code{MCerror} \tab a vector with Monte Carlo errors for the parameters. \cr
  \code{Rhat} \tab a vector with potential scale reduction factors for the parameters. \cr
  \code{timetaken} \tab an object of class \code{difftime} with the time taken for the MCMC run. }
}

\author{
Mike Meredith.
}

\keyword{methods}

