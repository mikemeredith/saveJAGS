
# Reads files and combines the dumped output into an 'mcmc.list' object

combineSaves <- function(fileList, params=NULL, thin=1) {

  if(!dir.exists(dirname(fileList[[1]][1])))
    stop("Can't find the folder: ", dirname(fileList[[1]][1]))
  if(!file.exists(fileList[[1]][1]))
    stop("Can't find the first file: ", fileList[[1]][1])
    
  # Open first file and check stuff
  loadEnv <- new.env(FALSE)  # Need to ring-fence the stuff loaded
  chk <- load(fileList[[1]][1], envir=loadEnv)
  if(!("out" %in% chk) || is.null(loadEnv$out) ||
          class(loadEnv$out) != "mcmc.list")
    stop("fileList is not a valid saveJAGS file list")
  if(length(loadEnv$out) != 1)
    stop("Files should contain a single chain.")
  niter <- nrow(loadEnv$out[[1]])
  nthin <- coda::thin(loadEnv$out)
  parAll <- colnames(loadEnv$out[[1]])

  if(!is.null(params)) {
    # Find which columns of the mcmc object to extract
    base <- sapply(strsplit(parAll, "\\["), "[", 1)
    parNames <- unique(base)
    bad <- is.na(match(params, parNames))
    if(sum(bad) == 1)
      stop("The parameter ", params[bad], " is not in the output.")
    if(sum(bad) > 1)
      stop("The parameters ", paste(params[bad], collapse=", "),
          " are not in the output")
    wanted <- base %in% params
  }

  MC <- vector('list', length(fileList))
  for(i in 1:length(fileList)) {
    this <- fileList[[i]]
    outsList <- vector('list', length(this))
    for(j in 1:length(this)) {
      chk <- load(this[[j]], envir=loadEnv)
      if( !("out" %in% chk))
        stop("File ", this[[j]], " does not have a valid MCMC chain.")
      if(!all(colnames(loadEnv$out[[1]]) == parAll))
        stop("File ", this[[j]], " does not have the correct parameters.")
      if(nrow(loadEnv$out[[1]]) != niter)
        stop("File ", this[[j]], " does not have the correct length.")
      if(coda::thin(loadEnv$out[[1]]) != nthin)
        stop("File ", this[[j]], " does not have the correct thinning.")

      if(thin > 1)
        loadEnv$out <- window(loadEnv$out, thin=thin * thin(loadEnv$out))
      if(!is.null(params)) {
        outsList[j] <- loadEnv$out[, wanted, drop=FALSE]
      } else {
        outsList[j] <- loadEnv$out
      }
    }
    MC[[i]] <- coda::mcmc(combo <- do.call(rbind, outsList),
      start = attr(outsList[[1]], "mcpar")[1],
      end = attr(outsList[[length(this)]], "mcpar")[2],
      thin = attr(outsList[[1]], "mcpar")[3])
  }
  return(coda::as.mcmc.list(MC))
}

