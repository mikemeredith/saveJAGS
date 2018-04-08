
# Provides a summary of fileList object


summary.saveJAGSfileList <- function(object, ...) {
  # Check for consistency of files
  stopifnot(is.list(object))
  fileCount <- sapply(object, length)
  if(any(diff(fileCount) != 0))
    stop("Chains do not have equal numbers of files.")
  fileSize <- sapply(object, file.size)
  diverg <- max(1 - min(fileSize)/median(fileSize),
                  max(fileSize)/median(fileSize)-1) *100
  if(diverg > 1)
    cat("File sizes diverge from the median by", round(diverg, 1), "%\n")

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
  if(niter < 1e4) {
    cat(sprintf("Iterations saved: %i per file, %i per chain, %i total.\n",
      niter, niter*filesPerChain, nRows))
  } else {
    cat(sprintf("Iterations saved: %1.1e per file, %1.1e per chain, %1.1e total.\n",
      niter, niter*filesPerChain, nRows))
  }
  parAll <- colnames(out[[1]])
  base <- sapply(strsplit(parAll, "\\["), "[", 1)
  nPars <- length(base)
  tb <- table(base)
  cat("Parameters included (with number of elements):\n")
  cat("    ", paste0(names(tb), " (", tb, ")", collapse=", "), "\n")
  cat("Total elements monitored:", nPars, "\n")
  if(niter < 1e4) {
    cat(sprintf("Total values saved: %i\n", nPars*nRows))
  } else {
    cat(sprintf("Total values saved: %1.1e\n", nPars*nRows))
  }
  cat("Expected object size:", round(nPars*nRows/16384, 2), "Mb\n")
}

