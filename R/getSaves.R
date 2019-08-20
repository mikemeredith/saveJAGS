
# Get the list of saved files created by 'saveJAGS' with chain IDs
# Don't check

getSaves <- function(fileStub) {
  if(!dir.exists(dirname(fileStub)))
    stop("Can't find the folder: ", dirname(fileStub))
  raw <- sort(list.files(dirname(fileStub), pattern=".RData$"))
  # Check filename stubs
  stubs <- sapply(strsplit(raw, "_"), function(x) x[1])
  files <- raw[stubs == basename(fileStub)]
  if(length(files) == 0)
    return(NULL)

  # Get chain IDs ###################################
  fnames <- basename(unlist(files))
  t2 <- strsplit(fnames, "_")
  # Get chain IDs
  n <- table(sapply(t2, function(x) x[2]))
  chainIDs <- names(n)

  # Create the file list
  chainNames <- paste0("_", chainIDs, "_")   #####################
  fileList <- list()
  for(i in seq_along(chainNames)) {
    this <- grepl(chainNames[i], files)
    fileList[[i]] <- file.path(dirname(fileStub), files[this])
  }
  names(fileList) <- chainIDs  #############################
  return(fileList)
}
