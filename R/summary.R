
# Provides a summary of fileList object


summary.saveJAGSfileList <- function(object, ...) {
  # Check for consistency of files
  stopifnot(is.list(object))
  fileCount <- sapply(object, length)
  if(any(diff(fileCount) != 0))
    stop("Chains do not have equal numbers of files.")
  fileSize <- sapply(object, file.size)
  if(any(diff(round(fileSize/100)) != 0))
    warning("Files differ in size.")
  
  # Open first file and check stuff
  out <- NULL
  load(object[[1]][1])
  if(is.null("out") || class(out) != "mcmc.list")
    stop("object is not a valid saveJAGS file list")
  stopifnot(length(out) == 1)
  
  nchains <- length(object)
  filesPerChain <- length(object[[1]])
  cat("File list with", nchains, "chains, each with",
      filesPerChain, "files.\n")
  nthin <- thin(out)
  cat("Chains already thinned by:", nthin, "\n")
  niter <- nrow(out[[1]])
  nRows <- niter*filesPerChain*nchains # rows in final matrix/mcmc.list/sims.list
  cat("Iterations saved:", niter, "per file,", niter*filesPerChain, "per chain,",
    nRows, "total.\n")
  parAll <- colnames(out[[1]])
  base <- sapply(strsplit(parAll, "\\["), "[", 1)
  nPars <- length(base)
  tb <- table(base)
  cat("Parameters included (with number of elements):\n")
  cat("    ", paste0(names(tb), " (", tb, ")", collapse=", "), "\n")
  cat("Total elements monitored:", nPars, "\n")
  cat("Total number of values saved:", nPars*nRows, "\n")
  cat("Expected object size:", round(nPars*nRows/131072, 2), "Mb\n")
}

