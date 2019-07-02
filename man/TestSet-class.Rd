\name{TestSet-class}
\Rdversion{1.1}
\docType{class}
\alias{TestSet-class}

\title{Class \code{"TestSet"}}
\description{

  This is a database stored collection of \code{\linkS4class{RuleTest}}
  objects.  It can be used to store test suites with a rule set.  This
  is a stub implementation to be completed later. 

}
\section{Extends}{

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}
\references{

  The document \dQuote{Rules Of Evidence} gives extensive documentation
  for the rule system.
  \url{https://pluto.coe.fsu.edu/Proc4/RulesOfEvidence.pdf}. 

}
\author{Russell Almond}
\note{

  Not yet fully implemented, this documentation is subject to change in
  the next version.

}
\seealso{
  \code{\link{runTest}}, \code{\linkS4class{EIEngine}},
  \code{\linkS4class{Rule}}, \code{\link{testRule}}

}
\examples{
showClass("TestSet")
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
    \item{\code{contexts}:}{Object of class \code{ContextSet} which
      gives the context information. }
    \item{\code{rules}:}{Object of class \code{RuleTable} which gives
      the rules against which this test set should be run. }
    \item{\code{db}:}{Object of class \code{MongoDB} which is the handle
      to the database collection.  As this field is initialized when first
      requested, it should not be accessed directly, but instead through
      the \code{recorddb()} method. }
  }
}
\section{Methods}{
  \describe{
    \item{\code{testdb()}:}{ This returns the handle to the
      \code{\link[mongolite]{mongo}} collection object.  If the
      connection has not yet been initialized. }
    \item{\code{initialize(app, dbname, dburi, contexts, rules, db, ...)}:}{ This sets up
      the object.  Note that the db field is not initialized until
      \code{testdb()} is first called.}
    \item{\code{clearAll()}:}{ Clears all records from the test collection. }
  }
}
