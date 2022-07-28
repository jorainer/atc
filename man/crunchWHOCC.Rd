\name{crunchWHOCC}
\alias{crunchWHOCC}
\title{
  Extract ATC data from the WHOCC web page
}
\description{
  The \code{crunchWHOCC} function recursively queries the WHOCC web page
  to extract the ATC data.
}
\usage{
crunchWHOCC(codes, baseurl = "https://www.whocc.no/atc_ddd_index/?code=",
            encoding = "utf-8")
}
\arguments{
  \item{codes}{
    Optional character vector of ATC codes to query. If not specified,
    the full ATC database will be queried.
  }

  \item{baseurl}{
    Character string specifying the URL to be used for the query.
  }

  \item{encoding}{
    The expected encoding of the returned HTML pages. Although specified
    to be iso-8859-1 in the HTML header, the data seems to be encoded,
    and provided, in utf-8.
  }
}
\value{
  The function returns a \code{list} with 3 \code{data.frames}:

  \code{atc}: the \code{data.frame} with the ATC data (columns
  \emph{key}, \emph{name} and \emph{level}).

  \code{ddd}: the \code{data.frame} with the defined daily dose data for
  level 5 ATC codes (columns \emph{key}, \emph{ddd}, \emph{unit},
  \emph{administration_route} and \emph{note}).

  \code{metadata}: the \code{data.frame} with some general information
  (columns \emph{name} and \emph{value}).
}
\author{
  Johannes Rainer
}
\seealso{
  \code{\link{atc}},
  \code{\link{keys}},
  \code{\link{atcData}}
}
\examples{

## See the vignette for an example how to use this function.

}
\keyword{data}
