\name{saveJAGS}
\alias{saveJAGS}

\title{
Write MCMC samples to file
}
\description{
Takes a list of data, initial values, a list of parameters to monitor, and the name of a text file containing the model, calls JAGS via \code{rjags}, and writes the output to a series of files.

Note: Currently, this only works with parallel processing with one chain per core.
}
\usage{
saveJAGS(data, inits, params, modelFile,
        chains=3, sample2save=1000, nSaves=3, burnin=1000, thin=1, fileStub,
        modules = "glm")
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
the number of chains to run; must not exceed the number of cores/threads available on the machine (maximum 26).
}
  \item{sample2save}{
the number of MCMC samples to include in each file AFTER thinning.
}
  \item{nSaves}{
the number of files to write per chain; the total iterations saved = sample2save * thin * nSaves * chains.
}
  \item{burnin}{
the number of iterations to discard at the beginning of each chain; unlike other wrappers, adaptation continues until the end of the burn-in sequence.
}
  \item{thin}{
the thinning interval.
}
  \item{fileStub}{
a character value to use as the first part of the file name; it can include the path provided the target directory exists; to this is added a letter to identify the chain, a sequence number and date-time the block was started.
}
  \item{modules}{
a character vector with names of the modules to load before calling JAGS.
}
}
\details{
After running \code{saveJAGS}, you can extract the saved values to create an \code{mcmc.list} object with \code{combineSaves}; see the examples. \code{recoverSaves} can be used to recreate the file list if \code{saveJAGS} terminates before completion.

Note that pressing 'Esc' will interrupt the master process in the R Console, but \emph{will not stop the workers}, which will continue until the task is finished, even if you close R. To stop the workers, right-click on the task in Task Manager and select 'End task' (Windows) or highlight in Activity Monitor and press the 'Force Quit' button (Mac).

The files written are \code{*.RData} files, which are compressed binary files. Each file contains four objects:

  \tabular{ll}{
  \code{out} \tab an object of class \code{mcmc.list} with 1 MCMC chain.\cr
  \code{jm} \tab the JAGS model; use \code{print(jm)} to see the model code, \code{jm$data()} to access the data. \cr
  \code{JAGSsettings} \tab a list with details of the modules loaded and the status of samplers. \cr
  \code{adaptIsAdequate} \tab logical, TRUE if adaptation was adequate. }
}
\value{
An object of class 'saveJAGSfileList', a list of file names with one component per chain. There is a summary method for the class.
}
\author{
Mike Meredith. Based on code used to run JAGS in the \code{wiqid} package.
}
\examples{
# An example with salamander occupancy: the number of occasions (out of 5)
#  that salamanders were detected at each of 39 sites
sal <- rep(0:4, c(21,12,1,4,1))

# JAGS code for psi(.) p(.) occupancy model
modelText <- "
model {
  for(i in 1:nSites) {
    z[i] ~ dbern(psi)
    y[i] ~ dbin(p * z[i], n)
  }
  psi ~ dbeta(1, 1)
  p ~ dbeta(1, 1)
} "
writeLines(modelText, con = "JAGSmodel.txt")

JAGSdata <- list(y = sal, n = 5, nSites = length(sal))
inits <- function(chain) list(z = rep(1, 39))
wanted <- c("p", "psi", "z")

# Create a folder for the output:
dir.create("mysaves")

res1 <- saveJAGS(JAGSdata, inits, wanted, "JAGSmodel.txt",
        chains=2, sample2save=1000, nSaves=4, burnin=1000, thin=1,
        fileStub="mysaves/testing")
str(res1)
summary(res1)

# If the saveJAGS run was terminated before completion,
#   we can recover the file list with recoverSaves:
res2 <- recoverSaves("mysaves/testing")
all.equal(res1, res2)

# Load the results into R as an mcmc.list object
mcmc <- combineSaves(res1)
str(mcmc)
#... or subset by parameters (omit z), or thin:
mcmc2 <- combineSaves(res1, params=c("p", "psi"), thin=4)
str(mcmc2)

# Clean up
unlink("JAGSmodel.txt")
unlink("mySaves", recursive=TRUE)
}

