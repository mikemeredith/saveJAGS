
# Recover saved files created by 'saveJAGS'

recoverSaves <- function(fileStub, force=FALSE) {
  if(!dir.exists(dirname(fileStub)))
    stop("Can't find the folder: ", dirname(fileStub))
  raw <- sort(list.files(dirname(fileStub), pattern=".RData$"))
  # Check filename stubs
  stubs <- sapply(strsplit(raw, "_"), function(x) x[1])
  files <- raw[stubs == basename(fileStub)]
  if(length(files) == 0)
    stop("No files found that match the stub: ", fileStub)

  # Get chain IDs
  fnames <- basename(unlist(files))
  t2 <- strsplit(fnames, "_")
  n <- table(sapply(t2, function(x) x[2]))
  chainIDs <- names(n)

  # Create the file list
  chainNames <- paste0("_", chainIDs, "_")
  fileList <- list()
  for(i in seq_along(chainNames)) {
    this <- grepl(chainNames[i], files)
    fileList[[i]] <- file.path(dirname(fileStub), files[this])
  }
  names(fileList) <- chainIDs

  # Check all chains have same number of files
  if(any(n != min(n))) {
    cat("Chains have differing numbers of files:\n")
    print(n)
    if(!force) {
      stop("Please remove extra files before proceeding.")
    } else {
      fileList <- lapply(fileList, `length<-`, min(n))
      fnames <- basename(unlist(fileList))
      t2 <- strsplit(fnames, "_")
      n <- table(sapply(t2, function(x) x[2]))
      cat("Extra files will not be included in the list.")
    }
  }
  
  # Check for duplicate chain/save combinations
  IDno <- sapply(t2, function(x) paste(x[2], x[3], sep='_'))
  dups <- duplicated(IDno)
  if(any(dups)) {
    cat("The following duplicate IDs were found:\n")
    print(IDno[dups])
    if(!force)
      stop("Please remove duplicate files before proceeding.")
  }
  
  # Check for correct letter/number sequences
  ok <- sort(outer(chainIDs, sprintf("%03i",1:min(n)), paste, sep="_"))
  bad <- IDno != ok
  if(any(bad)) {
    cat("File numbers are not in sequence:\n")
    try(print(data.frame(correct=ok, actual=IDno, check=ifelse(bad, "<--", ""))))
    if(!force)
      stop("Please check files before proceeding.")
  }

  
  # Check file sizes
  fileSize <- unlist(sapply(fileList, file.size))
  if(any(fileSize == 0)) {
    if(!force) {
      stop("At least one file has size 0.")
    } else {
      cat("At least one file has size 0.\n")
    }
  }
  diverg <- max(1 - min(fileSize)/median(fileSize),
                  max(fileSize)/median(fileSize)-1) *100
  if(diverg > 1)
    cat("File sizes diverge from the median by", round(diverg, 1), "%\n")


  class(fileList) <- c("saveJAGSfileList", class(fileList))
  return(fileList)
}
