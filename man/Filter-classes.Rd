\name{NameFilter-class}
\Rdversion{1.1}
\docType{class}
\alias{NameFilter-class}
\alias{NameFilter}
\alias{column,NameFilter,missing,missing-method}
\alias{column,NameFilter,AtcDb,missing-method}
\alias{column,NameFilter,AtcDb,character-method}
\alias{where,NameFilter,missing,missing-method}
\alias{where,NameFilter,AtcDb,missing-method}
\alias{where,NameFilter,AtcDb,character-method}

%% LevelFilter
\alias{LevelFilter-class}
\alias{LevelFilter}
\alias{column,LevelFilter,missing,missing-method}
\alias{column,LevelFilter,AtcDb,missing-method}
\alias{column,LevelFilter,AtcDb,character-method}
\alias{where,LevelFilter,missing,missing-method}
\alias{where,LevelFilter,AtcDb,missing-method}
\alias{where,LevelFilter,AtcDb,character-method}

%% KeyFilter
\alias{KeyFilter-class}
\alias{KeyFilter}
\alias{column,KeyFilter,missing,missing-method}
\alias{column,KeyFilter,AtcDb,missing-method}
\alias{column,KeyFilter,AtcDb,character-method}
\alias{where,KeyFilter,missing,missing-method}
\alias{where,KeyFilter,AtcDb,missing-method}
\alias{where,KeyFilter,AtcDb,character-method}

%% AdminrouteFilter
\alias{AdminrouteFilter-class}
\alias{AdminrouteFilter}
\alias{column,AdminrouteFilter,missing,missing-method}
\alias{column,AdminrouteFilter,AtcDb,missing-method}
\alias{column,AdminrouteFilter,AtcDb,character-method}
\alias{where,AdminrouteFilter,missing,missing-method}
\alias{where,AdminrouteFilter,AtcDb,missing-method}
\alias{where,AdminrouteFilter,AtcDb,character-method}


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

AdminrouteFilter(value, condition="=")
KeyFilter(value, condition="=")
LevelFilter(value, condition="=")
NameFilter(value, condition="=")

\S4method{column}{KeyFilter,AtcDb,character}(object, db, with.tables, ...)

\S4method{where}{KeyFilter,AtcDb,character}(object, db, with.tables, ...)

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
  \code{\link[ensembldb]{GeneidFilter}} class. Also, the methods listed
  below will in most cases not be called directly by the user.

  (in alphabetic order)

  \describe{
    \item{column}{
      Get the name of the database column that will be used by the
      filter.
      Returns a character of length 1.
    }

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

    \item{where}{
      Get the \emph{where} condition of the filter for the SQL query.
      Returns a character of length 1.
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

## Get the database column that will be used by the filter in the ATC DB.
column(kf, atc)

## Get the corresponding where-condition
where(kf, atc)

}
\keyword{classes}

