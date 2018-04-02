
# Recover saved files created by 'saveJAGS'

recoverSaves <- function(fileStub) {
  raw <- sort(list.files(dirname(fileStub), pattern=".RData$", recursive = TRUE))
  # Check filename stubs
  stubs <- sapply(strsplit(raw, "_"), function(x) x[1])
  files <- raw[stubs == basename(fileStub)]
  if(length(files) == 0)
    stop("No files found that match the stub: ", fileStub)

  chainNames <- paste0("_", LETTERS, "_")

  fileList <- list()
  for(i in seq_along(chainNames)) {
    this <- grepl(chainNames[i], files)
    if(sum(this) == 0)
      break
    fileList[[i]] <- file.path(dirname(fileStub), files[this])
  }
  # Check for duplicate file IDs, eg >1 file with "_A_1_"
  dups <- sum(grepl("_A_001_", fileList[[1]])) +
            sum(grepl("_A_1_", fileList[[1]])) # LEGACY - remove later
  if(dups > 1)
    stop("There are ", dups, " files with ID '_A_001_' or '_A_1_'.")
  # Check all chains have same number of files; trim off excess
  n <- sapply(fileList, length)
  if(any(n != min(n))) {
    fileList <- lapply(fileList, `length<-`, min(n))
    warning("Chains had differing numbers of files; extra files were ignored.")
  }
  class(fileList) <- c("saveJAGSfileList", class(fileList))
  return(fileList)
}
