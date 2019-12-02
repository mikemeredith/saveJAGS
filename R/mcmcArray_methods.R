
# S3 methods for mcmcArray objects

`$.mcmcArray` <- function(x, name) {
  sl <- attr(x, "simsList")
  this <- getElement(sl, name)
  if(is.null(this)) {
    warning("Cannot find parameter '", name, "' in '", deparse(substitute(x)), "'.", call.=FALSE)
    return(NULL)
  }
  out <- x[, this]
  dims <- dim(this)
  if(is.null(dims) && length(this) > 1) # if the parameter is not scalar
    dims <- length(this)
  if(!is.null(dims))
    dim(out) <- c(dim(out)[1], dims)
  # else: parameter is scalar, just return the vector
  return(out)
}
# ........................................................................

`[.mcmcArray` <- function(x, i, j, ..., drop=TRUE) {
  Nindices <- nargs() - 1 # ignore 'drop'
  if(Nindices > 3) {
    warning("Maximum number of indices is 3.", call.=FALSE)
    return(NULL)
  }
  out <- switch(Nindices,
    unclass(x)[, i, drop=drop],
    unclass(x)[i, j, drop=drop],
    {nChains <- attr(x, "nChains")
      parnames <- dimnames(x)[[2]]
      dim(x) <- c(dim(x)[1]/nChains, nChains, dim(x)[2])
      dimnames(x) <- list(NULL, 1:nChains, parnames)
      unclass(x)[i, j, ..., drop=drop]
    })
  return(out)
}
# ----------------------------------------------------------------


summary.mcmcArray <- function(object, ...)  {
  summary <- attr(object, "summary")
  if(is.null(summary)) {
    summary <- cbind(
      mean = colMeans(object),
      sd = apply(object, 2, sd),
      median = apply(object, 2, median),
      t(apply(object, 2, HDInterval::hdi)))
    colnames(summary)[4:5] <- c("HDIlo", "HDIup")
  }
  return(summary)
}
# .........................................................

print.mcmcArray <- function(x, digits=3, ...)  {
  toPrint <- attr(x, "summary")
  if(is.null(toPrint)) {
    toPrint <- cbind(
      mean = colMeans(x),
      sd = apply(x, 2, sd),
      median = apply(x, 2, median),
      t(apply(x, 2, HDInterval::hdi)))
    colnames(toPrint)[4:5] <- c("HDIlo", "HDIup")
  }
  print(toPrint, digits = digits)
}
# .........................................................
