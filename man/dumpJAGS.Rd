\name{dumpJAGS}
\alias{dumpJAGS}

\title{
Write MCMC samples to file
}
\description{
Takes a list of data, initial values, a list of parameters to monitor, and the name of a text file containing the model, calls JAGS via \code{rjags}, and writes to output to a series of files.
}
\usage{
dumpJAGS(data, inits, params, modelFile,
        chains=3, sample2dump=1000, nDumps=3, burnin=1000, thin=1, fileStub="dump",
        modules = c("glm"), parallel = NULL, seed=NULL)
}
\arguments{
  \item{data}{
a named list or environment containing the data.
}
  \item{inits}{
optional specification of initial values in the form of a list or a function.
}
  \item{params}{
a character vector giving the names of variables to be monitored.
}
  \item{modelFile}{
the name of the file containing a description of the model.
}
  \item{chains}{
the number of chains to run (maximum 26).
}
  \item{sample2dump}{
the number of MCMC samples to include in each file after thinning.
}
  \item{nDumps}{
the number of files to write per chain; the total iterations saved = sample2dump * nDumps * chains.
}
  \item{burnin}{
the number of iterations to discard at the beginning of each chain; unlike other wrappers, adaptation continues until the end of the burn-in sequence.
}
  \item{thin}{
the thinning interval.
}
  \item{fileStub}{
a character value to use as the first part of the file name; it can include the path; to this is added a letter to identify the chain, a sequence number and date-time the block was started.
}
  \item{modules}{
the modules to load before calling JAGS.
}
  \item{parallel}{
if TRUE, the chains will be run in parallel. If NULL, parallel processing will be used if the number of cores available exceeds the number of chains.
}
  \item{seed}{
the seed to pass to JAGS for the random number generation.
}
}

\value{
A list of file names with one component per chain.
}
\author{
Mike Meredith. Based code used to run JAGS in the \code{wiqid} package.
}

