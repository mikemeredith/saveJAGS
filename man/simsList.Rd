\name{simsList}
\alias{simsList}

\title{
Convert MCMC samples to a list of simulations (deprecated)
}
\description{
Takes an object of class \code{mcmc.list} and converts it to a list with one component for each parameter present. Parameters which are not scalar are stored as arrays of the appropriate dimension. Ragged arrays in the JAGS output result in all-NA columns in the output - see Examples.

We now recommend the use of \code{\link{mcmcOutput}} objects instead of \code{simsList}. This function will be removed in future versions.
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
A list with one component for each parameter monitored. This is the same format as the \code{sims.list} component in the output of other JAGS wrappers.
}
\author{
Mike Meredith.
}
\examples{
# Salamander occupancy again:
sal <- rep(0:4, c(21,12,1,4,1))

# JAGS code for psi(.) p(.) occupancy model but with additional parameters:
#   p appears to be a 2x2 matrix but p[1,1] is undefined;
#   this will be a ragged array in the output.
modelText <- "
model {
  for(i in 1:nSites) {
    z[i] ~ dbern(psi)
    y[i] ~ dbin(p[1,2] * z[i], n)
  }
  psi ~ dbeta(1, 1)
  p[1,2] ~ dbeta(1, 1)
  p[2,1] ~ dbeta(1, 1)
  p[2,2] <- 3
} "
writeLines(modelText, con = "JAGSmodel.txt")

JAGSdata <- list(y = sal, n = 5, nSites = length(sal))
inits <- function(chain) list(z = rep(1, 39))
wanted <- c("p", "psi")

res1 <- saveJAGS(JAGSdata, inits, wanted, "JAGSmodel.txt",
        chains=2, sample2save=1000, nSaves=4, burnin=1000, thin=1,
        fileStub="testing")
str(res1)
summary(res1)
  # p has 3 elements

# Load the results into R as an mcmc.list object
mcmc <- combineSaves(res1)
str(mcmc)
#... and convert to a sims.list:
sl <- simsList(mcmc)
str(sl)
  # sl$p is an 8000 x 2 x 2 array, with sl$p[, 1, 1] filled with NAs
apply(sl$p, 2:3, mean)
}

