\name{Status}
\alias{Status}
\alias{parseStatus}
\alias{as.jlist,Status,list-method}
\title{Status (state) object constructor}
\description{

  The \code{Status} function is the constructor for the
  \code{\linkS4class{Status}} (or state) object.  As Status objects are
  usually read from a database or other input stream, the
  \code{parseStatus} function is recreates an event from a JSON list.

}
\usage{
Status(uid, context, timerNames = character(), flags = list(), observables = list(), timestamp = Sys.time(), app = "default")
parseStatus(rec)
\S4method{as.jlist}{Status,list}(obj, ml)
}
\arguments{
  \item{uid}{A character scalar identifying the examinee or player.}
  \item{context}{A character string describing the task, item or game
    which the player is currently in.}
  \item{timerNames}{A list of names for timer objects.  These are
    created calling \code{\link{Timer}} with each name as an argument.}
  \item{flags}{A named list containing internal details about the
    event.  The necessary fields will depend on the \code{app} and the
    \code{context}.} 
  \item{observables}{A named list containing external details about the
    event.  The necessary fields will depend on the \code{app} and the
    \code{context}.}
  \item{timestamp}{The timestamp of the most recent
    \code{\linkS4class{Event}} processed.}
  \item{app}{A character scalar providing a unique identifier for the
    application (game or assessment).  This defines the available
    vocabulary for \code{flags} \code{timers} and \code{observables}, as
    well as the set of  applicable \code{\linkS4class{Rule}} objects.}
  \item{rec}{A named list containing JSON data.}
  \item{obj}{An object of class \code{\linkS4class{Status}} to be
    encoded.}
  \item{ml}{A list of fields of \code{obj}.  Usually, this is created by
    using \code{\link[base]{attributes}(obj)}.}

}
\details{

  Most of the details about the \code{\linkS4class{Status}} object, and
  how it works is documented under \link{Status-class}.  Note that the
  distinction between \code{flags} and \code{observables} is mostly one
  of intended usage:  \code{observables} are reproted to other
  processes, and \code{flags} are not.  Also, \code{timers} are created
  by their name and then need to be specifically set.

  The function \code{as.jlist} converts the \code{obj} into a named
  list. It is usually called from the function
  \code{\link[Proc4]{as.json}}.  

  The \code{parseStatus} function is the inverse of \code{as.jlist}
  applied to an event object.  It is designed to be given as an argument
  to \code{\link[Proc4]{getOneRec}} and 
  \code{\link[Proc4]{getManyRecs}}.   

}
\value{

  The functions \code{Status} and \code{parseStatus} return objects of
  class event.  The function \code{as.jlist} produces a named list
  suitable for passing to \code{\link[jsonlite]{toJSON}}.

}
\references{

  The document \dQuote{Rules Of Evidence} gives extensive documentation
  for the JSON layout of the Status/State objects.
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

  \code{\linkS4class{Status}} describes the state object.

  \code{\link[Proc4]{parseMessage}} and \code{\link[Proc4]{as.json}}
  describe the JSON conversion system.  

  The functions \code{\link[Proc4]{getOneRec}} and 
  \code{\link[Proc4]{getManyRecs}} use \code{parseStatus} to extract
  events from a database.

}
\examples{
st <- Status("Phred","Level 0",timerNames=c("watch","stopwatch"),
   flags=list("list"=list("one"=1,"two"="too"),"vector"=(1:3)*10),
   observables=list("numeric"=12.5,char="foo",
                 "list"=list("one"="a","two"=2),"vector"=(1:3)*100),
   timestamp=as.POSIXct("2018-12-21 00:01"))

st <- setTimer(st,"state.timers.stopwatch",as.difftime(15,units="secs"),TRUE,
               as.POSIXct("2018-12-21 00:01"))

sta <- parseStatus(as.jlist(st,attributes(st)))
stopifnot(all.equal(st,sta))


}
\keyword{ interface }
\keyword{ database }

