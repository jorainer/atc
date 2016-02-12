\name{AtcDb-class}
\Rdversion{1.1}
\docType{class}
\alias{AtcDb-class}
\alias{AtcDb}
\alias{show,AtcDb-method}
\alias{dbconn,AtcDb-method}
\alias{keys,AtcDb-method}
\alias{as.data.frame,AtcDb-method}
\alias{atcData}
\alias{atcData,AtcDb-method}
\alias{columns}

\title{Anatomical Therapeutic Chemical Data}

\description{
  The \code{AtcDb} provides the data from the Anatomical Therapeutic
  Chemical (ATC) Classification System as defined by the WHOCC (WHO
  Collaborating Centre for Drug Statistics Methodology).

  In this system, drugs are classified into groups at 5 different
  levels:

  \describe{

    \item{First level}{
      Indicates the anatomical main group and consists of one letter
      (e.g. \emph{C} for \emph{cardiovascular system}).
    }

    \item{Second level}{
      Indicates the therapeutic main group and consists of two digits
      (e.g. \emph{C03} for \emph{diuretic drugs}. Note that the digits
      \emph{03} have another meaning in another level, i.e. \emph{D03}
      stands for \emph{Treatment of wounds and ulcers}).
    }

    \item{Third level}{
      Indicates the therapeutic/pharmacological subgroup and consists of
      one letter (e.g. \emph{C03C} for \emph{High-ceiling diuretics}).
    }

    \item{Fourth level}{
      indicates the chemical/therapeutic/pharmacological subgroup and
      consists of one letter (e.g. \emph{C03CA} for
      \emph{Sulfonamides}).
    }

    \item{Fifth level}{
      The fifth level indicates the chemical substance and consists of
      two digits (e.g. \emph{C03CA01} for \emph{Furosemide}).
    }
  }
}

\section{Objects of the class}{
  \code{AtcDb} objects are created by the constructor function
  \code{AtcDb}. The database is automatically loaded on package startup
  and the \code{AtcDb} object providing access to the data is available
  through the \code{atc} variable. Thus, there should be no need to load
  or instatiate such an object manually.
}
\usage{

(in alphabetic order)

\S4method{as.data.frame}{AtcDb}(x, ...)

\S4method{atcData}{AtcDb}(x, columns, key, level, pattern)

\S4method{columns}{AtcDb}(x)

\S4method{dbconn}{AtcDb}(x)

\S4method{keys}{Props}(x, level, ...)

}
\arguments{

  (in alphabetic order)

  \item{columns}{
    A character vector specifying the columns that should be
    returned. Use the \code{columns} method to list all allowed column
    names.
  }

  \item{key}{
    A character vector of ATC code(s). Use method \code{keys} to
    retrieve all ATC codes stored in the database.
  }

  \item{level}{
    A numeric vector specifying the ATC code level(s) (from 1 to 5) that
    should be retrieved from the database.
  }

  \item{pattern}{
    A character vector of possible key words that are searched in the
    ATC names attribute. Note that a case insensitive pattern matching
    is performed on this keyword (or keywords).
  }

  \item{x}{
    The \code{AtcDb} object.
  }

  \item{...}{
    Presently not used.
  }

}
\section{Slots}{
  \describe{
    \item{atcdb}{
      The connection to the internal SQLite database.
    }

    \item{.properties}{
      Internal list to be used to set/specify properties. Not used at
      present.
    }

    \item{tables}{
      \code{list} providing the available column names in each table
      (names being the table names, elements the column/attribute
      names).
    }

  }
}

\details{
  The columns of the database are:
  \describe{

    \item{key}{
      The ATC code of the entry.
    }

    \item{name_de}{
      The german name of the entry.
    }

    \item{name_en}{
      The english name of the entry.
    }

    \item{level}{
      The ATC level of the entry (1 to 5).
    }

    \item{ddd}{
      (Only for level 5 entries): the defined daily doses. This may not
      be defined for all drugs.
    }
  }
}

\section{Basic usage}{

  (in alphabetic order)

  \describe{
    \item{as.data.frame}{
      Extract the full ATC data from the database and return it as
      \code{data.frame}.
    }

    \item{atcData}{
      Retrieve data from the database. This method allows to query
      either the full data from the database (if called without
      additional arguments) or allows to search and retrieve specific
      information, e.g. ATC data from specified ATC levels or ATC data
      where the name matches the supplied pattern.
      The method returns a \code{data.frame} with the columns specified
      by the argument \code{columns} or (by default) all columns.
    }

    \item{columns}{
      Retruns a character vector with the column names of the database
      tables.
    }

    \item{dbconn}{
      Returns the connection to the internal (RSQLite) database.
    }

    \item{keys}{
      Returns a character vector of all ATC codes in the database, or
      ATC codes of the level(s) that was/were specified with argument
      \code{level}.
    }

  }
}

\value{
  Refer to the method and function description above for detailed
  information on the returned result object.
}
\references{
  The WHO Collaborating Centre for Drug Statistics Methodology, Oslo,
  Norway (\url{http://www.whocc.no}).
}
\author{
  Johannes Rainer.
}
\examples{

## Display some information on the database.
atc

## List all columns in the database.
columns(atc)

## Extract the full data as data.frame.
tmp <- as.data.frame(atc)
head(tmp)

## Get all the ATC codes of level 1.
keys(atc, level=1)

## Get all data for ATC codes of level 1
atcData(atc, level=1)

## Get all data for ATC codes of level 2:4
tmp <- atcData(atc, level=2:4)

## Get all data containing "cardio" or "kidney" in the name
atcData(atc, pattern=c("cardio", "renal"), level=1:3)


}
\keyword{classes}

