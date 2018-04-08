\name{recoverSaves}
\alias{recoverSaves}

\title{
Recover files written by saveJAGS
}
\description{
Searches for files matching a file stub and the appropriate format, performs a series of checks on the folder contents, and returns a list of file names. 
}
\usage{
recoverSaves(fileStub, force=FALSE)
}
\arguments{
  \item{fileStub}{
a character value to use as the first part of the file name; it can include the path, but files will be searched for only in the current working directory and its subdirectories.
}
  \item{force}{
if TRUE, errors will not be thrown and the resulting file list may not be valid.
}
}

\details{
The function first checks that the target folder exists and that files matching the \code{fileStub} are present. It then checks that (1) all chains have the same number of files (if \code{force=TRUE}, extra files are not included in the list returned), (2) file IDs are not duplicated, (3) file IDs are in the right sequence, (4) no files have size 0.
}
\value{
An object of class 'saveJAGSfileList', a list of file names with one component per chain. There is a summary method for the class.
}
\author{
Mike Meredith.
}
\examples{
# See the examples for saveJAGS function
}

