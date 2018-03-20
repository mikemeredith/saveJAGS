
# Provides a summary of fileList object


summary.saveJAGSfileList <- function(object, ...) {
  # TODO check for consistency of files
  
  cat("File list with ", length(object), "chains, each with ",
      length(object[[1]]), "files.\n")
  # Open first file and check stuff
  out <- NULL
  load(object[[1]][1])
  if(is.null("out") || class(out) != "mcmc.list")
    stop("object is not a valid saveJAGS file list")
  stopifnot(length(out) == 1)
  niter <- nrow(out[[1]])  
  nthin <- thin(out)
  parAll <- colnames(out[[1]])
  base <- sapply(strsplit(parAll, "\\["), "[", 1)
  parNames <- unique(base)
  cat("Parameters included:\n")
  print(parNames, quote=FALSE)

}

