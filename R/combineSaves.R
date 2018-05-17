
# Reads files and combines the dumped output into an 'mcmc.list' object

combineSaves <- function(fileList, params=NULL, thin=1, cores=1) {

  fl <- verifyFileList(fileList)

  wanted <- NULL
  if(!is.null(params)) {
    # Find which columns of the mcmc object to extract
    bad <- is.na(match(params, fl$parNames))
    if(sum(bad) == 1)
      stop("The parameter ", params[bad], " is not in the output.")
    if(sum(bad) > 1)
      stop("The parameters ", paste(params[bad], collapse=", "),
          " are not in the output")
    wanted <- fl$base %in% params
  }
  if(cores < 2) {
    MC <- lapply(fileList, combineSaves1chain, wanted=wanted, thin=thin, fl=fl)
  } else {
    coresToUse <- min(cores, length(fileList), detectCores())
    cl <- makeCluster(coresToUse) ; on.exit(stopCluster(cl))
    MC <- parLapply(cl, fileList, combineSaves1chain, wanted=wanted, thin=thin, fl=fl)
  }
  # Check for errors
  classError <- sapply(MC, class) != "mcmc"
  if(any(classError)) {
    frstErr <- MC[[which(classError)[1]]]
    stop("Errors in ", sum(classError), " chain(s); first error:\n   ", frstErr,
      call.=FALSE)
  }

  return(coda::as.mcmc.list(MC))
}
# ........................................................................

# Function to combine the files for a single chain
combineSaves1chain <- function(fileVector, wanted, thin, fl) {
  loadEnv <- new.env(FALSE)  # Need to ring-fence the stuff loaded
  outsList <- vector('list', length(fileVector))
  for(j in 1:length(fileVector)) {
    chk <- load(fileVector[[j]], envir=loadEnv)
    if( !("out" %in% chk))
      return(paste0("File '", fileVector[[j]], "' does not have a valid MCMC chain."))
    if(!all(colnames(loadEnv$out[[1]]) == fl$parAll))
      return(paste0("File '", fileVector[[j]], "' does not have the correct parameters."))
    if(nrow(loadEnv$out[[1]]) != fl$niter)
      return(paste0("File '", fileVector[[j]], "' does not have the correct length."))
    if(coda::thin(loadEnv$out[[1]]) != fl$nthin)
      return(paste0("File '", fileVector[[j]], "' does not have the correct thinning."))

    if(thin > 1)
      loadEnv$out <- window(loadEnv$out, thin=thin * coda::thin(loadEnv$out))
    if(is.null(wanted)) {
      outsList[j] <- loadEnv$out
    } else {
      outsList[j] <- loadEnv$out[, wanted, drop=FALSE]
    }
  }
  MC <- coda::mcmc(combo <- do.call(rbind, outsList),
    start = attr(outsList[[1]], "mcpar")[1],
    thin = attr(outsList[[1]], "mcpar")[3])
  return(MC)
}

