
# Several small functions to get information from the saved files

getRunTime <- function(fileList) {
  # Get information about the files in fileList and calculate the run time for each
  fns <- unlist(fileList)
  t1 <- strsplit(basename(fns), "[._]")
  datecr <- sapply(t1, function(x) x[length(x)-2])
  timecr <- sapply(t1, function(x) x[length(x)-1])
  t2 <- paste(datecr, timecr, sep="_")
  startTime <- as.POSIXct(t2, format="%y%m%d_%H%M")
  endTime <- file.mtime(fns)
  return(endTime-startTime)
}
# ............................................................

getJAGSmodel <- function(fileList, raw=FALSE, show=TRUE) {
  if(!inherits(fileList, "saveJAGSfileList"))
    stop("This is not a valid saveJAGS file list.")
  # Load the first file in the list, grab the jm object and get the model code
  loadEnv <- new.env(FALSE)  # Need to ring-fence the stuff loaded
  chk <- load(fileList[[1]][1], envir=loadEnv)
  rw <- loadEnv$jm$model()
  cd <- paste0(rw, "\n", collapse="")
  if(raw) {
    out <- rw
  } else {
    out <- cd
  }
  if(show) {
    cat(cd)
    return(invisible(out))
  }
  return(out)
}
# .........................................................

getJAGSdata <- function(fileList) {
  if(!inherits(fileList, "saveJAGSfileList"))
    stop("This is not a valid saveJAGS file list.")
  # Load the first file in the list, grab the jm object and get the model code
  loadEnv <- new.env(FALSE)  # Need to ring-fence the stuff loaded
  chk <- load(fileList[[1]][1], envir=loadEnv)
  return(loadEnv$jm$data())
}





