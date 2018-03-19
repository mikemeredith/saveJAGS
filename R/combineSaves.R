
# Reads files and combines the dumped output into an 'mcmc.list' object

# TODO check that all vectors in fileList are same length.
# TODO check that all files exist and are same size.
# TODO check that thinning, chain length and parameter names match across files.
# TODO implement thinning and parameter selection when files are read in.

combineSaves <- function(fileList, params=NULL, thin=1) {

  # Open first file and check stuff
  load(fileList[[1]][1])
  stopifnot(exists("out"))
  stopifnot(class(out) == "mcmc.list")
  stopifnot(length(out) == 1)
  niter <- nrow(out[[1]])  
  nthin <- thin(out)
  parAll <- colnames(out[[1]])
  
  if(!is.null(params)) {
    base <- sapply(strsplit(parAll, "\\["), "[", 1)
    parNames <- unique(base)
    stopifnot(!any(is.na(match(params, parNames))))
    wanted <- base %in% params
  }
  
  MC <- vector('list', length(fileList))
  for(i in 1:length(fileList)) {
    this <- fileList[[i]]
    outsList <- vector('list', length(this))
    for(j in 1:length(this)) {
      out <- NULL  # in case the loaded file does not contain 'out'
      load(this[[j]])
      if(thin > 1)
        out <- window(out, thin=thin)
      if(!is.null(params)) {
        outsList[j] <- out[, wanted, drop=FALSE]
      } else {
        outsList[j] <- out
      }
    }
    MC[[i]] <- coda::mcmc(combo <- do.call(rbind, outsList),
      start = attr(outsList[[1]], "mcpar")[1],
      end = attr(outsList[[length(this)]], "mcpar")[2],
      thin = attr(outsList[[1]], "mcpar")[3])
  }
  return(coda::as.mcmc.list(MC))
}

