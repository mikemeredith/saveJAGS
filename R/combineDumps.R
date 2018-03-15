
# Reads files and combines the dumped output into an 'mcmc.list' object

combineDumps <- function(fileList) {
  MC <- vector('list', length(fileList))
  for(i in 1:length(fileList)) {
    this <- fileList[[i]]
    outsList <- vector('list', length(this))
    for(j in 1:length(this)) {
      out <- NULL  # in case the loaded file does not contain 'out'
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

