\name{UserRecordSet-class}
\Rdversion{1.1}
\docType{class}
\alias{UserRecordSet-class}
\alias{UserRecordSet}

\title{Class \code{"UserRecordSet"}}
\description{

  A collection of user records associated with a given application;
  actually, a handle for the database collection holding the user
  records.  User records are object of class
  \code{\linkS4class{Status}}.   

}
\section{Extends}{

All reference classes extend and inherit methods from
\code{"\linkS4class{envRefClass}"}. 

}
\references{

  The document \dQuote{Rules Of Evidence} gives extensive documentation
  for the context system:
  \url{https://pluto.coe.fsu.edu/Proc4/RulesOfEvidence.pdf}. 

}
\author{Russell G. Almond}
\note{

  This is actually a wrapper for a database collection.  The collection
  can contain records from many applications.  The primary key for a
  record is \code{(app,uid,timestamp)}.  For a given uid and
  application, the current record is the one with the most recent
  timestamp.  

}
\seealso{

  \code{\linkS4class{Status}}, \code{\linkS4class{EIEngine}}

}
\examples{
showClass("UserRecordSet")
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
      to the database collection.  As this field is initialized when first
      requested, it should not be accessed directly, but instead through
      the \code{recorddb()} method. }
  }
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{newStudent(uid)}:}{ This generates a new blank status
      for a given student id. If there is an existing status for the
      student, that is returned.  If not, if there is a status with uid
      \dQuote{*DEFAULT*}, that is returned.  If neither of those exists,
      a new blank status is created with context id \dQuote{*INITIAL*}.}
    \item{\code{getStatus(uid)}:}{ Finds the current status for a given
      \code{uid}. }
    \item{\code{initialize(app, dbname, dburi, db, ...)}:}{ This sets up
      the object.  Note that the db field is not initialized until
      \code{recorddb()} is first called.}
    \item{\code{recorddb()}:}{ This returns the handle to the
      \code{\link[mongolite]{mongo}} collection object.  If the
      connection has not yet been initialized. }
    \item{\code{saveStatus(state)}:}{ This saves the status in the database. }
  }
}
