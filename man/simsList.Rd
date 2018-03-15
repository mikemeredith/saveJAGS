\name{simsList}
\alias{simsList}

\title{
Convert MCMC samples to a list of simulations
}
\description{
Takes an object of class \code{mcmc.list} and converts it to a list with one component for each parameter present. Parameters which are not scalar are stored as arrays of the appropriate dimension. This is the same format as the \code{sims.list} component in the output of other JAGS wrappers.
}
\usage{
simsList(mc)
}
\arguments{
  \item{mc}{
an object of class \code{mcmc.list}.
}
}

\value{
A list with one component for each parameter monitored.
}
\author{
Mike Meredith.
}

