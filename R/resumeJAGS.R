
# Resuming after a saveJAGS run has been interrupted

resumeJAGS <- function(fileStub, nSaves=3) {
  # fileStub
  # nSaves is the number of additional files needed
  nSaves <- ceiling(nSaves)

  message("Calling recoverSaves to check target folder.")
  fileList <- recoverSaves(fileStub, force=FALSE)

  fl <- verifyFileList(fileList)

  # Load last file in each chain and extract model objects
  loadEnv <- new.env(FALSE)  # Need to ring-fence the stuff loaded
  last <- length(fileList[[1]])
  resList <- vector('list', length(fileList))
  for(i in seq_along(fileList)) {
    this <- fileList[[i]][last]
    chk <- load(this, envir=loadEnv)
    if( !("jm" %in% chk))
      stop("File ", this, " does not have a valid JAGS model.", call.=FALSE)
    resList[[i]] <- list(jm=loadEnv$jm, chainID=names(fileList)[i])
  }
  names(resList) <- names(fileList)

  # Get JAGSsettings from the last file loaded (they should all be identical)
  if(!"JAGSsettings" %in% chk) {
    modules <- "glm"  # LEGACY - later replace with stop
  } else {
    tmp <- loadEnv$JAGSsettings$modules
    modules <- tmp[tmp != "basemod" &
                   tmp != "bugs" &
                   tmp != "lecuyer"]
  }

  # Deal with parallelism:
  chains <- length(resList)
  nCores <- detectCores()
  if(chains > nCores)
    stop("Cannot run", chains, "chains on", nCores, "cores/threads.", call.=FALSE)

  message("Parallel processing now running; output will be written to files.") ; flush.console()
  starttime  <- Sys.time()
  cl <- makeCluster(chains) ; on.exit(stopCluster(cl))
  clusterEvalQ(cl, library(rjags))
  clusterEvalQ(cl, load.module("lecuyer"))
  if(length(modules) > 0) {
    clusterExport(cl, c("modules", "loadJagsModules"), envir=environment())
    clusterEvalQ(cl, loadJagsModules(modules))
  }

  newFileList <- parLapply(cl, resList, resumeJAGS1, params=fl$parNames,
    sample2save=fl$niter, nSaves=nSaves, startAt=last+1,
    thin=fl$nthin, fileStub=fileStub)
  message("Parallel processing done.")

  out <- fileList
  for(i in seq_along(fileList))
    out[[i]] <- c(fileList[[i]], newFileList[[i]])

  print(Sys.time() - starttime)
  class(out) <- c("saveJAGSfileList", class(fileList))
  invisible(out)
}

# ...............................................................


resumeJAGS1 <- function(resList, params, sample2save, nSaves, startAt=1,
  thin=1, fileStub="save") {

  jm <- resList$jm
  jm$recompile()
  # Adaptation phase
  for(i in 1:100) {
    done <- rjags::adapt(jm, 100)
    if(done)
      break
  }
  adaptIsAdequate <- rjags::adapt(jm, n.iter=0, end.adaptation=TRUE)

  # Create JAGSsettings object
  JAGSsettings <- list(modules=list.modules(), samplers=list.factories("sampler"))
  fileNames <- character(nSaves)
  for(i in 1:nSaves) {
    TS <- format(Sys.time(), "%y%m%d_%H%M.RData")
    fileNames[i] <- paste(fileStub, resList$chainID, sprintf("%03i",i+startAt-1),
        TS, sep="_")
    out <- rjags::coda.samples(jm, params, n.iter=sample2save * thin, thin=thin)
    save(out, jm, JAGSsettings, adaptIsAdequate, file=fileNames[i])
  }
  return(fileNames)
}



