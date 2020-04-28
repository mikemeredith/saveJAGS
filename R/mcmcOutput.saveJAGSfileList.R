
# Reads files and combines the dumped output into an 'mcmc.list' object

mcmcOutput.saveJAGSfileList <- function(object, header, params=NULL, thin=1,
    burnFiles=0, cores=1, ...) {
  name <- deparse(substitute(object))
  if(missing(header))
    header <- paste("MCMC values from saveJAGS object", sQuote(name))

  fl <- verifyFileList(object)

  wanted <- NULL
  if(!is.null(params)) {
    # Find which columns of the mcmc object to extract
    bad <- is.na(match(params, fl$parNames))
    if(sum(bad) == 1)
      stop("The parameter ", params[bad], " is not in the output.",
        call. = FALSE)
    if(sum(bad) > 1)
      stop("The parameters ", paste(params[bad], collapse=", "),
          " are not in the output", call. = FALSE)
    wanted <- fl$base %in% params
  }
  if(cores < 2) {
    MC <- lapply(object, mcmcOutput1chain, wanted=wanted, thin=thin,
      burnFiles = burnFiles, fl=fl)
  } else {
    coresToUse <- min(cores, length(object), detectCores())
    cl <- makeCluster(coresToUse) ; on.exit(stopCluster(cl))
    MC <- parLapply(cl, object, mcmcOutput1chain, wanted=wanted, thin=thin,
      burnFiles = burnFiles, fl=fl)
  }
  # Check for errors ???

  mcMat <- do.call(rbind, MC)
  out <- mcmcOutput(mcMat, header=header, nChains=length(object))

  return(out)
}
# ........................................................................

# Function to combine the files for a single chain
mcmcOutput1chain <- function(fileVector, wanted, thin, burnFiles, fl) {
  loadEnv <- new.env(FALSE)  # Need to ring-fence the stuff loaded
  nFiles <- length(fileVector) - burnFiles
  outsList <- vector('list', nFiles)
  for(j in 1:nFiles) {
    chk <- load(fileVector[[j+burnFiles]], envir=loadEnv)
    if( !("out" %in% chk))
      return(paste0("File '", fileVector[[j+burnFiles]], "' does not have a valid MCMC chain."))
    if(!all(colnames(loadEnv$out[[1]]) == fl$parAll))
      return(paste0("File '", fileVector[[j+burnFiles]], "' does not have the correct parameters."))
    if(nrow(loadEnv$out[[1]]) != fl$niter)
      return(paste0("File '", fileVector[[j+burnFiles]], "' does not have the correct length."))
    if(coda::thin(loadEnv$out[[1]]) != fl$nthin)
      return(paste0("File '", fileVector[[j+burnFiles]], "' does not have the correct thinning."))

    if(thin > 1)
      loadEnv$out <- window(loadEnv$out, thin=thin * coda::thin(loadEnv$out))
    if(is.null(wanted)) {
      outsList[j] <- loadEnv$out
    } else {
      outsList[j] <- loadEnv$out[, wanted, drop=FALSE]
    }
  }
  return(do.call(rbind, outsList))
}

