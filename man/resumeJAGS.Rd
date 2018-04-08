\name{resumeJAGS}
\alias{resumeJAGS}

\title{
Resume a saveJAGS run
}
\description{
The files written by \code{saveJAGS} and \code{resumeJAGS} contain information on the model and the JAGS settings which allow the MCMC to be restarted at almost the point where it was interrupted or terminated. \code{resumeJAGS} extracts this information from the last files for each chain in the previous run and writes further files with MCMC output to disk.
}
\usage{
resumeJAGS(fileStub, nSaves=3)
}
\arguments{
  \item{fileStub}{
a character value giving the path to the folder containing the files from the previous run and the first part of the file name.
}
  \item{nSaves}{
the number of additional files to write per chain; the total iterations saved = sample2save * thin * nSaves * chains.
}
}
\details{
When \code{saveJAGS} is interrupted, some files may be damaged or missing from the set. It is important that chains have matching sets of valid files from the previous run before resuming. \code{resumeJAGS} begins by calling \code{\link{recoverSaves}}, which runs a series of checks and indicates issues with the existing files which the user needs to address before continuing.
}
\value{
An object of class 'saveJAGSfileList', a list of file names with one component per chain. There is a summary method for the class.
}
\author{
Mike Meredith.
}
\examples{
# We continue the example from saveJAGS:
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
wanted <- c("p", "psi")

# Create a folder for the output:
dir.create("mysaves")

res1 <- saveJAGS(JAGSdata, inits, wanted, "JAGSmodel.txt",
        chains=2, sample2save=1000, nSaves=4, burnin=1000, thin=1,
        fileStub="mysaves/testing")
str(res1)

# Now we resume the run, writing a further 3 files for each chain:
res2 <- resumeJAGS(fileStub="mysaves/testing", nSaves=3)
str(res2)

# Load the results into R as an mcmc.list object
mcmc <- combineSaves(res2)
str(mcmc)

# Clean up
unlink("mySaves", recursive=TRUE)
}

