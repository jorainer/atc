\name{NameFilter-class}
\Rdversion{1.1}
\docType{class}
\alias{NameFilter-class}
\alias{NameFilter}

%% LevelFilter
\alias{LevelFilter-class}
\alias{LevelFilter}

%% KeyFilter
\alias{KeyFilter-class}
\alias{KeyFilter}

%% AdminrouteFilter
\alias{AdminrouteFilter-class}
\alias{AdminrouteFilter}

\title{Filter framework for the ATC database}

\description{
  The filter objects (extending the \code{BasicFilter} from the
  \code{ensembldb} package) listed on this page allow to customize the
  queries to the ATC database and retrieve specific entries. Filters can
  be passed with the \code{filter} argument to the \code{\link{atcs}}
  method. Multiple filters can be combined (and passed as a \code{list}
  to the beforementioned method). The supported filter objects are (in
  alphabetical order):

  \describe{

    \item{AdminrouteFilter}{
      Filter results based on the administration route of (level 5)
      entries (such as \code{"O"} for \emph{oral}).
    }

    \item{KeyFilter}{
      Filter results based on the ATC key.
    }

    \item{LevelFilter}{
      Filter results based on the ATC level.
    }

    \item{NameFilter}{
      Filter results based on the name of the entry.
    }

    \code{KeyFilter} and \code{NameFilter} both allow to perform also
    pattern matching queries by setting \code{condition="like"} and
    using a percentage sign as wild card (e.g. instead of *).

  }
}

\section{Objects of the class}{
  Filter objects should be created using the constructor functions
  \code{KeyFilter}, \code{LevelFilter} and \code{NameFilter}.
}
\usage{

AdminrouteFilter(value, condition="==")
KeyFilter(value, condition="==")
LevelFilter(value, condition="==")
NameFilter(value, condition="==")

}
\arguments{

  (in alphabetic order)

  \item{condition}{
    The condition for the filter. For character values: \code{"="},
    \code{"!="}, \code{"in"}, \code{"not in"}, \code{"like"}, for
    numeric values: \code{"="}, \code{"<="}, \code{"<"}, \code{">"},
    \code{">="}, \code{"in"}, \code{"not in"}.
  }

  \item{db}{
    The \code{\linkS4class{AtcDb}} object from which the data should be
    retrieved.
  }

  \item{object}{
    A filter object (either \code{AdminrouteFilter}, \code{KeyFilter},
    \code{LevelFilter} or \code{NameFilter}).
  }

  \item{with.tables}{
    Optional argument; for internal use.
  }

  \item{value}{
    The value for the filter (against which database entries are
    compared against, based on the filter's \code{condition} argument).
  }

  \item{...}{
    Presently not used.
  }

}

\section{Methods}{

  All classes inherit all methods from the
  \code{\link[AnnotationFilter]{AnnotationFilter}} class.

  (in alphabetic order)

  \describe{

    \item{AdminrouteFilter}{
      Creates and returns a new \code{AdminrouteFilter} instance.
    }

    \item{KeyFilter}{
      Creates and returns a new \code{KeyFilter} instance.
    }

    \item{LevelFilter}{
      Creates and returns a new \code{LevelFilter} instance.
    }

    \item{NameFilter}{
      Creates and returns a new \code{NameFilter} instance.
    }

  }
}

\value{
  Refer to the method and function description above for detailed
  information on the returned result object.
}
\author{
  Johannes Rainer.
}
\examples{

## Create a simple KeyFilter
kf <- KeyFilter("R07")
kf

## Define a filter for all keys starting with R07
kf <- KeyFilter("R07", condition = "startsWith")
kf

}
\keyword{classes}

