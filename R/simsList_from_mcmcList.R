
# Takes an object of class 'mcmc.list' and produces a "sims list"

simsList <- function(mc) {

  # Convert mcmc.list to matrix
  mcMat <- do.call(rbind,mc)
  # Get parameter names
  params <- colnames(mcMat)
  # Extract base names of parameters
  base <- sapply(strsplit(params, "\\["), "[", 1)
  parNames <- unique(base)

  # Build simsList
  simsList <- vector('list', length(parNames))
  names(simsList) <- parNames
  for(i in seq_along(parNames)) {
    simsList[[i]] <- mcMat[, base == parNames[i]]
    if(!is.null(dim(simsList[[i]])))   # not scalar, need to convert to array
      simsList[[i]] <- matrix2raggedArray(simsList[[i]])
  }
  return(simsList)
}
# ...............................................................


# Function to convert a mcmc.list-type matrix to an array, which may be ragged.
matrix2raggedArray <- function(mat) {
  stopifnot(length(dim(mat)) == 2)
  niter <- nrow(mat)
  nms <- colnames(mat)
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
  output <- array(NA, dim=c(niter, maxind))
  for(i in seq_along(nms)) {
    command <- paste0("output[1:", niter, ",",indices.c[i], "] <- mat[, i]")
    eval(parse(text=command))
  }
  stopifnot(length(mat) == sum(!is.na(output)))
  stopifnot(mean(mat) == mean(output, na.rm=TRUE))
  return(output)
}




