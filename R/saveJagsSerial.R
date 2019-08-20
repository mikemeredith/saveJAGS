
# This helper function is called (with chains=1) to run JAGS in each worker.
# Note that initList MUST be the first argument to work with parLapply.

# Not exported.

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

