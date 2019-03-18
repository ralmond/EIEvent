\name{EIEngine}
\alias{EIEngine}
\title{Creator for the EIEngine class.}
\description{

  The \code{\linkS4class{EIEngine}} is the prime mover for the EIEvent
  class.  This command constructs the engine.  The engine
  stores most of its information in a database, so the constructor
  mainly consists of the database location and credentials.  
}
\usage{
EIEngine(app = "default", listeners = list(), username = "", password = "", host = "localhost", port = "", dbname = "EIRecords", ...)
}
\arguments{
  \item{app}{The application ID for the engine.  See Details.}
  \item{listeners}{A list of objects of type
  \code{\linkS4class{Listener}} which will receive messages from the
  engine.}
  \item{username}{A username for the database. If left blank the
  database will be accessed with an anonymous user.}
  \item{password}{A password for the database.  If left blank, the
  database will be accessed without a password.}
  \item{host}{The hostname for the database.  Defaults to \dQuote{localhost}.}
  \item{port}{This is the port used for the database connection.  If
  left blank, the default port 27017 will be used.}
  \item{dbname}{This the name of the database.  The default name is
  \dQuote{EIRecords}.} 
  \item{\dots}{For future expansions, subclasses.}
}
\details{

  The \code{\linkS4class{EIEngine}} is an interpreter for the rules,
  which form a code base for a specific engine.  The \code{EIEngine}
  class provides connection to six collections in a database which
  provide most of the action of the EIEvent system.

  \describe{
    \item{Events}{Incomming events (\code{\linkS4class{Event}} objects).
      Accessed through \code{$eventdb()} method.} 
    \item{Messages}{Outgoing messages (\code{\linkS4class{P4Message}}
      objects).  Accessed through 
      \code{\linkS4class{ListenerSet}} (\code{$listenerSet}) object.}
    \item{Rules}{Rule collection (\code{\linkS4class{Rule}} objects).
      Accessed through 
      \code{\linkS4class{RuleTable}} (\code{$rules}) object.}
    \item{States}{Collection of saved statuses
      (\code{\linkS4class{Status}} objects).  Accessed through
      \code{\linkS4class{UserRecordSet}} (\code{$userRecords}) object.}
    \item{Contexts}{Context collection (\code{\linkS4class{Context}}
      objects).  Accessed through 
      \code{\linkS4class{ContextSet}} (\code{$contexts}) object.}
    \item{Tests}{Self-test collection (\code{\linkS4class{EITest}}
      objects).  Accessed through 
      \code{\linkS4class{TestSet}} (\code{$tests}) object.}
  }

  Note that the database connections are made in a lazy fashion,
  connecting to the database when first accessed rather than when the
  object is created.  So the proper accessor methods (e.g.,
  \code{$eventdb()} whould be used instead of the raw field names.

  All of the collections have the \code{app} field as part of their key,
  so that several applications can share a database.  A different
  EIEngine is needed for each application.

}
\value{

  A reference object of class \code{\linkS4class{EIEngine}}.  See the
  class page for information about the methods supported.

}
\references{
  The document \dQuote{Rules Of Evidence} gives extensive documentation
  for the rule system.
  \url{https://pluto.coe.fsu.edu/Proc4/RulesOfEvidence.pdf}. 

  Almond, R. G., Steinberg, L. S., and Mislevy, R.J. (2002).  Enhancing
  the design and delivery of Assessment Systems:  A Four-Process
  Architecture.  \emph{Journal of Technology, Learning, and Assessment},
  \bold{1},
  \url{http://ejournals.bc.edu/ojs/index.php/jtla/article/view/1671}. 

  Almond, R. G., Shute, V. J., Tingir, S. and Rahimi,
  S. (2018). Identifying Observable Outcomes in Game-Based Assessments.
  Talk given at the \emph{2018 Maryland Assessment Research
  Conference}.  Slides:
  \url{https://education.umd.edu/file/11333/download?token=kmOIVIwi},
  Video: \url{https://pluto.coe.fsu.edu/Proc4/Almond-Marc18.mp4}.

  MongoDB, Inc. (2018).  \emph{The MongoDB 4.0 Manual}.
  \url{https://docs.mongodb.com/manual/}.
}
\author{Russell Almond}
\note{

  This class is not currently threadsafe.  In particular, if the
  \code{$handleEvent()} method is called simultaneously on two different
  events with the same \code{uid}, the result will not be pretty.
  However, separate applications can be run in parallel (in different
  processes or threads).

}

\seealso{

  The primary classes in the EIEvent system are:
  \code{\linkS4class{EIEngine}}, \code{\linkS4class{Context}},
  \code{\linkS4class{Status}}, \code{\linkS4class{Event}},
  \code{\linkS4class{Rule}}.

  The EIEngine class is a container for the following classes:
  \code{\linkS4class{UserRecordSet}}, \code{\linkS4class{RuleTable}},
  \code{\linkS4class{ContextSet}},  \code{\linkS4class{TestSet}} and
  \code{\link[Proc4]{ListenerSet}}, 

  \code{\link[futile.logger]{flog.logger}}

}
\examples{
cl <- new("CaptureListener")
eng <- EIEngine(app="ecd://epls.coe.fsu.edu/EItest",listeners=list(capture=cl))

}
\keyword{ interface }
\keyword{ class }
