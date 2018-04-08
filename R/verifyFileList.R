
# Function to check a fileList object and extract key values

verifyFileList <- function(fileList) {
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
  
  # Get items to return
  parAll <- colnames(loadEnv$out[[1]])
  base <- sapply(strsplit(parAll, "\\["), "[", 1)
  out <- list(
    niter = nrow(loadEnv$out[[1]]),
    nthin = coda::thin(loadEnv$out),
    parAll = parAll,
    base = base,
    parNames = unique(base) )
  return(out)
}