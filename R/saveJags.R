
# Functions to run JAGS via 'rjags' in parallel,
#  saving output to disk in chunks.

# Purpose is to keep RAM requirements down and to keep output in case of power outages.

# Improving handling of JAGS errors:
#  1. Catch and exit gracefully.
#  2. Pass error info to master
#  3. Write error info to a file in case of crash.
# Failure during model-building and adaptation, and during the main run.
# Need to modify recoverSaves and combineSaves to deal.

# This will ONLY run in parallel and will ONLY do one chain per core.

# This helper function is called (with chains=1) to run JAGS in each worker.
# Note that initList MUST be the first argument to work with parLapply.
saveJagsSerial <- function(initList, data, params, modelFile,
    chains=1, sample2save, nSaves, burnin=1000, thin=1, fileStub) {

  chainID <- initList$chainID
  initList$chainID <- NULL # is necessary
  
  JAGSerrorMessage <- NULL
  jm <- try(rjags::jags.model(modelFile, data, initList, n.chains=chains, n.adapt=0))
  if(inherits(jm, "try-error"))
    JAGSerrorMessage <- jm
  if(is.null(JAGSerrorMessage) && burnin > 0) {
    trial <- try(update(jm, burnin))
    if(inherits(trial, "try-error"))
      JAGSerrorMessage <- trial
  }
  if(is.null(JAGSerrorMessage)) {
    adaptIsAdequate <- try(rjags::adapt(jm, n.iter=0, end.adaptation=TRUE))
    if(inherits(adaptIsAdequate, "try-error"))
      JAGSerrorMessage <- adaptIsAdequate
  }
  
  if(!is.null(JAGSerrorMessage)) {
    # Save the error message to a file
    errorfile <- paste(fileStub, chainID, "startupError.RData", sep="_")
    save(JAGSerrorMessage, file=errorfile)
    return(JAGSerrorMessage)
  }
  
  # Create JAGSsettings object
  JAGSsettings <- list(modules=list.modules(), samplers=list.factories("sampler"))
  fileNames <- character(nSaves)
  for(i in 1:nSaves) {
    TS <- format(Sys.time(), "%y%m%d_%H%M.RData")
    fileNames[i] <- paste(fileStub, chainID, sprintf("%03i",i), TS, sep="_")
    out <- try(rjags::coda.samples(jm, params, n.iter=sample2save * thin, thin=thin))
    if(inherits(out, "try-error")) {
      JAGSerrorMessage <- out
      save(JAGSerrorMessage, jm, JAGSsettings, adaptIsAdequate, file=fileNames[i])
      break
    }
    save(out, jm, JAGSsettings, adaptIsAdequate, file=fileNames[i])
    rm(out)
  }
  return(fileNames)
}
# ---------------------------------------------------------------

# The main function to run JAGS

saveJAGS <- function(data, inits, params, modelFile,
        chains=3, sample2save=1000, nSaves=3, burnin=1000, thin=1, fileStub="save",
        modules = "glm")  {

  starttime  <- Sys.time()

  # Round up fractions:
  chains <- ceiling(chains)
  sample2save <- ceiling(sample2save)
  nSaves <- ceiling(nSaves)
  burnin <- ceiling(burnin)
  thin <- ceiling(thin)
  # Deal with parallelism:
  nCores <- detectCores()
  if(nCores < 2)
      stop("Multiple cores not available.", call.=FALSE)
  if(chains > 26)
    stop("Currently limited to 26 chains.", call.=FALSE)
  if(chains > nCores)
    stop("Cannot run", chains, "chains on", nCores, "cores/threads.", call.=FALSE)

  # Check that path exists and files do not exist
  if(!dir.exists(dirname(fileStub)))
    stop("Can't find the folder: ", dirname(fileStub), call.=FALSE)
  firstFile <- paste0(basename(fileStub), "_A_001_")
  raw <- list.files(dirname(fileStub), pattern=".RData$")
  if(any(grepl(firstFile, raw)))
    stop("Files with names '", fileStub, "' already exist.\n\tUse a different fileStub.", call.=FALSE)

  # Deal with seeds and RNGs -- use 'lecuyer'
  load.module("lecuyer", quiet=TRUE)
  seeds <- parallel.seeds("lecuyer::RngStream", chains)

  # Fix inits
  if(is.function(inits))  {
    initList <- lapply(1:chains, function(x) inits())
  } else if (is.list(inits) && length(inits) == chains) {
    initList <- inits
  } else stop("inits must be a function or a list of length = chains")
  for(i in 1:chains) {
    initList[[i]] <- c(initList[[i]], seeds[[i]])
    initList[[i]]$chainID <- LETTERS[i]
  }
  names(initList) <- LETTERS[1:chains]
  
  message("Parallel processing now running; output will be written to files.") ; flush.console()
  cl <- makeCluster(chains) ; on.exit(stopCluster(cl))
  clusterEvalQ(cl, library(rjags))
  clusterEvalQ(cl, load.module("lecuyer"))
  if(!is.null(modules)) {
    clusterExport(cl, c("modules", "loadJagsModules"), envir=environment())
    clusterEvalQ(cl, loadJagsModules(modules))
  }
  fileList <- parLapply(cl, initList, saveJagsSerial, data=data, params=params,
    modelFile=modelFile, chains=1, sample2save=sample2save, nSaves=nSaves,
    burnin=burnin, thin=thin, fileStub=fileStub)
  message("Processing done.")

  print(Sys.time() - starttime)
  class(fileList) <- c("saveJAGSfileList", class(fileList))
  invisible(fileList)
}

