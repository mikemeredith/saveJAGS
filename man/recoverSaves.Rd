\name{recoverSaves}
\alias{recoverSaves}

\title{
Recover files written by saveJAGS
}
\description{
Searches for files matching a file stub and the appropriate format and returns a list of file names.
}
\usage{
recoverSaves(fileStub)
}
\arguments{
  \item{fileStub}{
a character value to use as the first part of the file name; it can include the path, but files will be searched for only in the current working directory and its subdirectories.
}
}

\value{
An object of class 'saveJAGSfileList', a list of file names with one component per chain. There is a summary method for the class.
}
\author{
Mike Meredith.
}

