
# Functions to run JAGS via 'rjags' in parallel,
#  saving output to disk in chunks.

# Purpose is to keep RAM requirements down and to keep output in case of power outages.

# Improving handling of JAGS errors:
#  1. Catch and exit gracefully.
#  2. Pass error info to master
#  3. Write error info to a file in case of crash.
# Failure during model-building and adaptation, and during the main run.
# Need to modify recoverSaves and combineSaves to deal.

# Change in chain IDs to double letters 2019-08-20

# This will ONLY run in parallel and will ONLY do one chain per core.

saveJAGS <- function(data, inits, params, modelFile, fileStub,
        chains=3, sample2save=1000, nSaves=3, burnin=1000, thin=1,
        modules = "glm", firstChainID="AA")  {

  starttime  <- Sys.time()

  # Round up fractions:
  chains <- ceiling(chains)
  sample2save <- ceiling(sample2save)
  nSaves <- ceiling(nSaves)
  burnin <- ceiling(burnin)
  thin <- ceiling(thin)

  # Deal with chain IDs
  fstChain <- which(DLETTERS == firstChainID)
  lstChain <- fstChain + chains - 1
  if(lstChain > 26^2)
    stop("Chain IDs go beyond 'ZZ'.", call.=FALSE)
  chainIDs <- DLETTERS[fstChain:lstChain]

  # Check that path exists and files do not exist
  # check fileStub for final "/"
  if(grepl("/$", fileStub))
    stop("File stub must include a file name, ie, must not end with '/'.", call.=FALSE)
  files <- getSaves(fileStub)
  if(!is.null(files)) {
    clash <- chainIDs %in% names(files)
    if(any(clash))
      stop("Files for some chains already exist for '", fileStub, "'.\n\tUse a different fileStub.", call.=FALSE)
  }

  # Deal with parallelism:
  nCores <- detectCores()
  if(nCores < 2)
      stop("Multiple cores not available.", call.=FALSE)
  if(chains > nCores)
    stop("Cannot run", chains, "chains on", nCores, "cores/threads.", call.=FALSE)


  # Deal with seeds and RNGs -- use 'lecuyer'
  load.module("lecuyer", quiet=TRUE)
  seeds <- parallel.seeds("lecuyer::RngStream", chains)

  # Fix inits
  if(missing(inits) || is.null(inits))
    inits <- vector('list', chains)
  if(is.function(inits))  {
    if(is.null(formals(inits)$chain)) {  # does inits have a 'chain' argument?
      initList <- lapply(1:chains, function(x) inits())
    } else {
      initList <- lapply(1:chains, function(x) inits(chain=x))
    }
  } else if (is.list(inits) && length(inits) == chains) {
    initList <- inits
  } else stop("inits must be EITHER a function OR a list of length = chains")
  for(i in 1:chains) {
    initList[[i]] <- c(initList[[i]], seeds[[i]])
    initList[[i]]$chainID <- chainIDs[i]
  }
  names(initList) <- chainIDs

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

