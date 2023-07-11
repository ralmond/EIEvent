\name{ContextSet-class}
\Rdversion{1.1}
\docType{class}
\alias{ContextSet-class}
\alias{newContextSet}

\title{Class \code{"ContextSet"}}
\description{

  This is a reference class which is a wrapper for a handle to a
  database collection of \code{\linkS4class{Context}} objects associated
  with a particular application.  Note that this is a reference class
  and many of the methods permanently modify the database.

}
\section{Extends}{

All reference classes extend and inherit methods from
\code{"\linkS4class{envRefClass}"}.

}
\section{Methods}{
  \describe{
    \item{clearContexts}{\code{signature(set = "ContextSet")}: Removes
      all of the contexts associated with this applicaiton from the
      database. }
    \item{matchContext}{\code{signature(id = "character", set =
        "ContextSet")}: searches for a \code{\linkS4class{Context}} with
      the given context id (\code{\link{cid}}).}
    \item{matchContext}{\code{signature(id = "integer", set =
        "ContextSet")}: searches for a \code{\linkS4class{Context}} with
      a given numeric id (\code{\link{number}}).}
    \item{updateContext}{\code{signature(con = "Context", set =
        "ContextSet")}: Adds or replaces a \code{\linkS4class{Context}}
      in the database.}
    \item{ContextSet}{\code{signature(app = "character", dbname =
        "character", dburi = "character", ...)}:
      Constructor, creates a new context set for the given application,
      connecting to the database referenced by \code{dbname} and
      \code{dburi}.}
  }
}
\references{
  The document \dQuote{Rules Of Evidence} gives extensive documentation
  for the context system:
  \url{https://pluto.coe.fsu.edu/Proc4/RulesOfEvidence.pdf}.

}
\author{Russell Almond}
\note{

  In general, several context sets can share the same database
  collection.  These are distinguished by the application ID
  (\code{app}).  The effect primary key for the collection is
  \code{(app,cid)} with a secondary key as \code{(app,number)}.  The
  collection maintains an index on both of those field pairs.

}
\seealso{

  \code{\linkS4class{Context}} describes the context object.

  The functions \code{\link{matchContext}}, \code{\link{updateContext}}
  \code{\link{clearContexts}}, and \code{\link{loadContexts}} are used
  to manipulate context sets.

}
\examples{
showClass("ContextSet")
newContextSet(app="default",colname="test",
dbname="test", dburi=character())
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{app}:}{Object of class \code{character} giving the
      identifier for the application.  This is part of the database key
      for the collection. }
    \item{\code{dbname}:}{Object of class \code{character} giving the
      name of the database (e.g., \dQuote{EIRecords}).}
    \item{\code{dburi}:}{Object of class \code{character} giving the uri
      for the database, (e.g., \dQuote{mongodb://localhost}).}
    \item{\code{db}:}{Object of class \code{MongoDB} which is the handle
      to the database.  As this field is initialized when first
      requested, it should not be accessed directly, but instead through
      the \code{contextdb()} method.}
  }
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{update(con)}:}{ This inserts of replaces a
      \code{\linkS4class{Context}} object in the database. }
    \item{\code{initialize(app, dbname, dburi, db, ...)}:}{ This sets up
      the object.  Note that the db field is not initialized until
      \code{contextdb()} is first called.}
    \item{\code{contextdb()}:}{ This returns the handle to the
      \code{\link[mongolite]{mongo}} collection object.  If the
      connection has not yet been initialized. }
    \item{\code{named(id)}:}{ This searches for the context by context
      id (\code{\link{cid}}), not by name. }
    \item{\code{numbered(num)}:}{ This searchers for the context by number. }
    \item{\code{clearAll()}:}{ This removes all contexts associated with
      this application from the database.}
  }
}
