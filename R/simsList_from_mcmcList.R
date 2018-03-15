
# Takes an object of class 'mcmc.list' and produces a "sims list"

simsList <- function(mc) {

  # Get parameter names
  params <- colnames(mc[[1]])
  # Extract base names of parameters
  base <- sapply(strsplit(params, "\\["), "[", 1)
  parNames <- unique(base)

  # Convert mcmc.list to matrix
  mcMat <- do.call(rbind,mc)
  n <- nrow(mcMat)  # number of MCMC samples

  # Build simsList
  simsList <- vector('list', length(parNames))
  names(simsList) <- parNames
  for(i in seq_along(parNames)) {
    simsList[[i]] <- mcMat[, base == parNames[i]]
    if(!is.null(dim(simsList[[i]]))) {  # not scalar, need to convert to array
      foo <- colnames(simsList[[i]])
      f1 <- strsplit(foo[length(foo)],'\\[')[[1]][2]
      f2 <- strsplit(f1,'\\]')[[1]]
      d <- as.numeric(strsplit(f2, ",")[[1]]) # dimensions of the parameter
      dim(simsList[[i]]) <- c(n, d)
    }
  }
  return(simsList)
}



