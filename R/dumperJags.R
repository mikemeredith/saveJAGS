
# Functions to run JAGS via 'rjags' in parallel,
#  saving output to disk in chunks.

# Purpose is to keep RAM requirements down and to keep output in case of power outages

# loads/unloads more than one JAGS module

# rjags::load.module and unload.module will only load/unload one module!
loadJagsModules <- function(modules)  {
  for(i in seq_along(modules))
    rjags::load.module(modules[i])
}

unloadJagsModules <- function(modules)  {
  for(i in seq_along(modules))
    rjags::unload.module(modules[i])
}
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


# Run JAGS in serial mode.

# This function is also called (with chains=1) to run JAGS in each worker.
# Note that initList MUST be the first argument to work with parLapply.
dumperJagsSerial <- function(initList, data, params, modelFile,
    chains=1, sample2dump, nDumps, burnin=1000, thin=1, fileStub="dump") {
    
  chainID <- initList$chainID
  initList$chainID <- NULL # is necessary
  jm <- rjags::jags.model(modelFile, data, initList, n.chains=chains, n.adapt=0)
  if(burnin > 0)
    update(jm, burnin)
  rjags::adapt(jm, n.iter=0, end.adaptation=TRUE)
  TS <- format(Sys.time(), "%y%m%d_%H%M.RData")
  fileNames <- character(nDumps)
  for(i in 1:nDumps) {
    fileNames[i] <- paste(fileStub, chainID, i, TS, sep="_")
    out <- rjags::coda.samples(jm, params, n.iter=sample2dump * thin, thin=thin)
    save(out, file=fileNames[i])
  }
  return(fileNames)
}
# ---------------------------------------------------------------

# The main function to run JAGS

dumperJags <- function(data, inits, params, modelFile,
        chains=3, sample2dump=1000, nDumps=3, burnin=1000, thin=1,
        modules = c("glm"), parallel = NULL, seed=NULL)  {

  # Deal with parallelism:
  if(chains == 1)
    parallel <- FALSE
  if(is.null(parallel))
    parallel <- chains < detectCores()
  if(parallel) {
    coresToUse <- min(chains, detectCores() - 1)
    if(coresToUse < 2) {
      warning("Multiple cores not available; running chains sequentially.")
      parallel <- FALSE
    }
  }
  if(parallel) {
    if(chains > coresToUse)
      warning(paste("Running", chains, "chains on", coresToUse, "cores."))
  }

  # Deal with seeds and RNGs
  set.seed(seed, kind='default')
  chainSeeds <- sample.int(1e6, chains)
  rng0 <- paste("base", c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
    "Mersenne-Twister"), sep="::")
  rng <- rep(rng0, length=chains)

  # Fix inits
  if(is.function(inits))  {
    initList <- lapply(1:chains, function(x) inits())
  } else if (is.list(inits) && length(inits) == chains) {
    initList <- inits
  } else stop("inits must be a function or a list of length = chains")
  for(i in 1:chains) {
    initList[[i]]$.RNG.name <- rng[i]
    initList[[i]]$.RNG.seed <- chainSeeds[i]
    initList[[i]]$chainID <- LETTERS[i]
  }

  if(parallel) {   ##### Do the parallel stuff #####
    message("Waiting for parallel processing to complete...", appendLF=FALSE) ; flush.console()
    cl <- makeCluster(coresToUse) ; on.exit(stopCluster(cl))
    clusterEvalQ(cl, library(rjags))
    if(!is.null(modules)) {
      clusterExport(cl, c("modules", "loadJagsModules"), envir=environment())
      clusterEvalQ(cl, loadJagsModules(modules)) # No need to unload as we stopCluster
    }
    fileList <- parLapply(cl, initList, dumperJagsSerial, data=data, params=params,
      modelFile=modelFile, chains=1, sample2dump=sample2dump, nDumps=nDumps,
      burnin=burnin, thin=thin)
    message("done.")
  } else {     ##### Do the serial stuff #####
    if(!is.null(modules))
      loadJagsModules(modules)
    fileList <- dumperJagsSerial(initList, data=data, params=params,
      modelFile=modelFile, chains=chains, sample2dump=sample2dump, nDumps=nDumps,
      burnin=burnin, thin=thin)
    if(!is.null(modules))
      unloadJagsModules(modules)
  }

  invisible(fileList)
}

combineDumps <- function(fileList) {
  MC <- vector('list', length(fileList))
  for(i in 1:length(fileList)) {
    this <- fileList[[i]]
    outsList <- vector('list', length(this))
    for(j in 1:length(this)) {
      load(this[[j]])
      outsList[j] <- out
    }
    MC[[i]] <- coda::mcmc(combo <- do.call(rbind, outsList),
      start = attr(outsList[[1]], "mcpar")[1],
      end = attr(outsList[[length(this)]], "mcpar")[2],
      thin = attr(outsList[[1]], "mcpar")[3])
  }
  return(as.mcmc.list(MC))
}

