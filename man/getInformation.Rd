\name{getInformation}
\alias{summary.saveJAGSfileList}
\alias{getJAGSmodel}
\alias{getJAGSdata}
\alias{getRunTime}

\title{
Get information about files written by saveJAGS
}
\description{
\code{summary} gives a summary of the informations available; \code{getRunTime} gives the run time of each file; \code{getJAGSmodel} displays the JAGS model code; \code{getJAGSdata} returns the data list used for fitting the model.
}
\usage{
\method{summary}{saveJAGSfileList}(object, ...)

getRunTime(fileList)
getJAGSmodel(fileList, raw=FALSE, show=TRUE)
getJAGSdata(fileList)
}
\arguments{
  \item{fileList, object}{
a list of file names as returned by \code{saveJAGS}, \code{recoverSaves} or \code{resumeJAGS}.
}
  \item{raw}{
the model is stored as a character vector with one string per line of code; if \code{raw = TRUE}, this is returned, otherwise the text is converted to a single string with new lines marked with \code{\\n}.
}
  \item{show}{
if TRUE, the model is displayed in the Console; the character vector is then returned invisibly.
}
  \item{...}{
not used.
}
}

\value{
\code{summary} returns the base names of the parameters.

\code{getRunTime} returns a vector of \code{difftime} objects with the run times; start times are recorded to 1 min precision only which limits the precision of the result.

\code{getJAGSmodel} returns a character object with the model code, either as a character vector with one string per line of code, or as a single string with new lines marked with \code{\\n}.

\code{getJAGSdata} returns a list with the data list used for fitting the model.
}
\author{
Mike Meredith.
}
\examples{
# See the examples for saveJAGS function
}

