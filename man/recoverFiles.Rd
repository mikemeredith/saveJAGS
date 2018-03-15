\name{recoverDumps}
\alias{recoverDumps}

\title{
Recover files written by dumpJAGS
}
\description{
Searches for files matching a file stub and the appropriate format and returns a list of file names.
}
\usage{
recoverDumps(fileStub)
}
\arguments{
  \item{fileStub}{
a character value to use as the first part of the file name; it can include the path, but files will be searched for only in the current working directory and its subdirectories.
}
}

\value{
A list of file names with one component per chain.
}
\author{
Mike Meredith.
}

