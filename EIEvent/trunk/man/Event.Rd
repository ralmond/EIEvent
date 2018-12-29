\name{Event}
\alias{Event}
\alias{parseEvent}
\alias{as.jlist,Event,list-method}
\title{Event object constructor}
\description{

  The \code{Event} funciton is the constructor for the
  \code{\linkS4class{Event}} object.  As Event objects are usually read
  from a database or other input stream, the \code{parseEvent} function
  is recreates an event from a JSON list.

}
\usage{
Event(uid, verb, object = "", timestamp = Sys.time(), details = list(), app = "default", context = "")
parseEvent(rec)
\S4method{as.jlist}{Event,list}(obj, ml)
}
\arguments{
  \item{uid}{A character scalar identifying the examinee or player.}
  \item{verb}{A character scalar identifying the action which triggered
    the event.}
  \item{object}{A character scalar identifying the direct object of the
    verb.}
  \item{timestamp}{An object of class \code{\link[base]{POSIXt}} which
    provides the time at which the event occurred.}
  \item{details}{A named list of detailed data about the the event.  The
    available fields will depend on the \code{app}, \code{verb} and
    \code{object}. }
  \item{app}{A character scalar providing a unique identifier for the
    application (game or assessment).  This defines the available
    vocabulary for \code{verb} and \code{object}, as well as the set of
    applicable \code{\linkS4class{Rule}} objects.}
  \item{context}{A character string describing the task, item or game
    level during which the event occurred.  It could be blank if the
    context needs to be figured out from surrounding events.}
  \item{rec}{A named list containing JSON data.}
  \item{obj}{An object of class \code{\linkS4class{Event}} to be
    encoded.}
  \item{ml}{A list of fields of \code{obj}.  Usually, this is created by
    using \code{\link[base]{attributes}(obj)}.}
}
\details{

  Most of the details about the \code{\linkS4class{Event}} object, and
  how it works is documented under \link{Event-class}.

  The function \code{as.jlist} converts the \code{obj} into a named
  list. It is usually called from the function
  \code{\link[Proc4]{as.json}}.  

  The \code{parseEvent} function is the inverse of \code{as.jlist}
  applied to an event object.  It is designed to be given as an argument
  to \code{\link[Proc4]{getOneRec}} and 
  \code{\link[Proc4]{getManyRecs}}.   

}
\value{

  The functions \code{Event} and \code{parseEvent} return objects of
  class event.  The function \code{as.jlist} produces a named list
  suitable for passing to \code{\link[jsonlite]{toJSON}}.
}
\references{

  The document \dQuote{Rules Of Evidence} gives extensive documentation
  for the JSON layout of the Event objects.
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

  Betts, B, and Smith, R. (2018). The Leraning Technology Manager's Guid
  to {xAPI}, Second Edition.  HT2Labs Research Report:
  \url{https://www.ht2labs.com/resources/the-learning-technology-managers-guide-to-the-xapi/#gf_26}.

  HT2Labs (2018). Learning Locker Documentation.
  \url{https://docs.learninglocker.net/welcome/}.
}

\author{Russell Almond}
\seealso{

  \code{\linkS4class{Event}} describes the event object.

  \code{\link[Proc4]{parseMessage}} and \code{\link[Proc4]{as.json}}
  describe the JSON conversion system.  In particular, as
  \code{\linkS4class{Event}} extends
  \code{\link[Proc4]{P4Message}}, the Event method for
  \code{as.jlist} calls the P4Message method.

  The functions \code{\link[Proc4]{getOneRec}} and 
  \code{\link[Proc4]{getManyRecs}} use \code{parseEvent} to extract
  events from a database.

}
\examples{

ev1 <- Event("Phred","test","message",
      timestamp=as.POSIXct("2018-12-21 00:01:01"),
      details=list("list"=list("one"=1,"two"=1:2),"vector"=(1:3)))
ev2 <- Event("Phred","wash","window",
      timestamp=as.POSIXct("2018-12-21 00:02:01"),
      details=list(condition="streaky"))
ev3 <- Event("Fred","open","can",
      timestamp=as.POSIXct("2018-12-21 00:03:01"),
      details=list(lidOn=FALSE))


ev1a <- parseEvent(ununboxer(as.jlist(ev1,attributes(ev1))))
ev2a <- parseEvent(ununboxer(as.jlist(ev2,attributes(ev2))))
ev3a <- parseEvent(ununboxer(as.jlist(ev3,attributes(ev3))))

stopifnot(all.equal(ev1,ev1a), all.equal(ev2,ev2a), all.equal(ev3,ev3a))

\dontrun{ #Requires test DB setup.
testcol <- mongo("Messages",
                 url="mongodb://test:secret@127.0.0.1:27017/test")
## Mongodb is the protocol
## user=test, password =secret
## Host = 127.0.0.1 -- localhost
## Port = 27017 -- Mongo default
## db = test
## collection = Messages
testcol$remove('{}')  ## Clear everything for test.                  

ev1 <- saveRec(ev1,testcol)
ev2 <- saveRec(ev2,testcol)
ev3 <- saveRec(ev3,testcol)

ev1b <- getOneRec(buildJQuery("_id"=ev1@"_id"),testcol,parseEvent)
ev23 <- getManyRecs(buildJQuery("uid"="Phred"),testcol,parseEvent)
stopifnot(all.equal(ev1,eb1b), length(ev23)==2L)

}

}
\keyword{ interface }
\keyword{ database }

