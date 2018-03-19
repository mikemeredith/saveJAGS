
# Recover saved files created by 'saveJAGS'

recoverSaves <- function(fileStub) {
  raw <- sort(list.files(pattern=".RData$", recursive = TRUE))
  # Check filename stubs
  stubs <- sapply(strsplit(raw, "_"), function(x) x[1])
  files <- raw[stubs == fileStub]
  if(length(files) == 0)
    stop("No files found that match the stub: ", fileStub)

  chainNames <- paste0("_", LETTERS, "_")

  fileList <- list()
  for(i in seq_along(chainNames)) {
    this <- grepl(chainNames[i], files)
    if(sum(this) == 0)
      break
    fileList[[i]] <- files[this]
  }
  # Check all chains have same number of files
  n <- sapply(fileList, length)
  if(any(diff(n) != 0))
    warning("Chains have differing numbers of files.")
  return(fileList)
}
