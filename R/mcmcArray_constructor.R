
# Constructor function for mcmcArray objects

mcmcArray <- function(object, ...) UseMethod("mcmcArray")

mcmcArray.default <- function(object, ...) {
    stop(paste("No applicable method for class", class(object)))
}
# ...................................................................

mcmcArray.mcmc.list <- function(object, ...) {
  mcMat <- as.matrix(object)
  attr(mcMat, "nChains") <- length(object)
  attr(mcMat, "simsList") <- simsListAttr(mcMat)
  # Build summary
  summary <- cbind(
    mean = colMeans(mcMat),
    sd = apply(mcMat, 2, sd),
    median = apply(mcMat, 2, median),
    t(apply(mcMat, 2, HDInterval::hdi)) )
  attr(mcMat, "summary") <- summary

  class(mcMat) <- "mcmcArray"
  return(mcMat)
}
# .......................................................................

mcmcArray.jagsUI <- function(object, ...) {
  mcMat <- mcmcArray(object$samples)
  attr(mcMat, "nChains") <- length(object$samples)
  attr(mcMat, "simsList") <- simsListAttr(mcMat)
  # add attributes for summary
  attr(mcMat, "summary") <- object$summary
  return(mcMat)
}
# .......................................................................

# Generate the simsList attribute
simsListAttr <- function(mcMat, ...) {
  # Get parameter names
  params <- colnames(mcMat)
  # Extract base names of parameters
  base <- sapply(strsplit(params, "\\["), "[", 1)
  parNames <- unique(base)

  # Build the list
  simsList <- vector('list', length(parNames))
  names(simsList) <- parNames
  for(i in seq_along(parNames)) {
    simsList[[i]] <- which(base == parNames[i])
    if(length(simsList[[i]]) > 1)   # not scalar, need to convert to array
      simsList[[i]] <- params2raggedArray(simsList[[i]], params)
  }
  return(simsList)
}
# .......................................................................

# Get an array with indices into a larger matrix
params2raggedArray <- function(index, names) {
  nms <- names[index]
  # Extract the indices from the column names
  t1 <- sapply(strsplit(nms, "\\["), "[", 2)
  indices.c <- unlist(strsplit(t1, "\\]")) # indices as character string

  # Get the max indices = size of output array
  t4 <- simplify2array(strsplit(indices.c, ","))
  if(is.null(dim(t4))) {
    maxind <- max(as.integer(t4))
  } else {
    maxind <- apply(t4, 1, function(x) max(as.integer(x)))
  }

  # Create output array and plug in values
  output <- array(NA, dim=maxind)
  for(i in seq_along(nms)) {
    command <- paste0("output[", indices.c[i], "] <- index[i]")
    eval(parse(text=command))
  }

  # convert 1d array to vector
  if(length(dim(output)) == 1)
    output <- c(output)
  return(output)
}
# ...................................................................

