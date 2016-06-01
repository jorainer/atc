\name{atc}
\alias{atc}
\alias{atc_wido}
\title{
  ATC data resource
}
\description{
  Database connection objects providing access to the ATC database(s) of
  the package.
}
\usage{
  atc

}
\details{
  \describe{
    \item{atc}{
      The \code{\linkS4class{AtcDb}} object representing the
      connection to the (internal) ATC database.
    }
  }
}
\author{
Johannes Rainer
}
\seealso{
  \code{\link{atcs}}, \code{\link{as.data.frame}},
  \code{\link{keys}}
}
\examples{

## Print some summary information for the database:
atc

## Get all ATC codes of level 1
## atcData(atc, level=1)
atcs(atc, filter=LevelFilter(1))

}
\keyword{ data }

