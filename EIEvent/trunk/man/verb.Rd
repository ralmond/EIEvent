\name{verb}
\alias{verb}
\alias{verb,Event-method}
\alias{verb,Rule-method}
\alias{object}
\alias{object,Event-method}
\alias{object,Rule-method}
\alias{context}
\alias{context,Event-method}
\alias{context,Rule-method}
\alias{context,Status-method}
\alias{context<-}
\alias{context<-,Status-method}
\alias{oldContext}
\alias{oldContext,Status-method}
\title{Accessor for verb and object field of events and rules.}
\description{

  Both \code{\linkS4class{Event}} and \code{\linkS4class{Rule}} objects
  have \code{verb}, \code{object} and \code{context} fields which
  describe the event that occurred or to which the rule applies.  The
  vocabulary is given by the \code{\link[Proc4]{app}} field.

}
\usage{
verb(x)
\S4method{verb}{Event}(x)
\S4method{verb}{Rule}(x)
object(x)
\S4method{object}{Event}(x)
\S4method{object}{Rule}(x)
context(x)
\S4method{context}{Event}(x)
\S4method{context}{Rule}(x)
\S4method{context}{Status}(x)
context(x) <- value
\S4method{context}{Status}(x) <-value
oldContext(x)
\S4method{oldContext}{Status}(x)
}
\arguments{
  \item{x}{An object of class \code{\linkS4class{Event}} or
    \code{\linkS4class{Rule}} (or in the case of \code{context}, a
    \code{\linkS4class{Status}} object).}
  \item{value}{A character scalar giving a new value for the context.}
}
\details{

  The \code{verb} and \code{object} fields are fairly self-explanatory.
  They are simply strings which give the verb and direct object of the
  activity in the presentation process which triggered the event.  In
  the case of an \code{\linkS4class{Event}}, these are created by the
  presentation process.  For \code{\linkS4class{Rule}} objects, the
  fields indicate the type of event for which the rule should be
  applicable.  In particular, a rule is applicable to a certain event
  when the \code{verb} and \code{object} fields match.  Note that if the
  the \code{verb} or \code{objec}field of thas the special keywork 
  special keyword \code{"ALL"} that indicates that the rule is
  applicable for all verbs or objects respectively.  

  The \code{context} field is a little more complex. First, the value of
  the \code{context} field could correspond to a
  \code{\linkS4class{Context}} object or, in the case of a rule, be the
  keyword \code{"ALL"}.  For the \code{\linkS4class{Status}} (or
  \code{\linkS4class{Event}}) class this should be a primative context.
  In the case of a \code{\linkS4class{Rule}} class, it could also be a
  context group, which covers several other contexts through the
  \code{\link{belongTo}} field.  The rule context must match the context
  field in the \code{\linkS4class{Status}} (not the
  \code{\linkS4class{Event}}.  The rule is considered applicable if (a) the
  contexts match exactly, (b) the rule context is a context set and the
  status context belongs to it, or (c) the rule context is the keyword
  \code{"ALL"}.

  In some cases, the presentation process will recognize and record the
  context. In this case the \code{context} field of the
  \code{\linkS4class{Event}} object will be of length 1.  Otherwise, it
  will have the value \code{character(0)}.  In many cases, a change of
  context is recognized by a \dQuote{Context} rule.  After the context
  changes, the \code{oldContext} field retains the value of the previous
  context (useful for \dQuote{Trigger} rules which usually fire after a
  change in context).  The \code{oldContext} field gets reset when the
  \code{\linkS4class{EIEngine}} starts processing a new event.

}
\value{

  A character scalar giving the verb, object, or context.
  
}
\references{

  The document \dQuote{Rules Of Evidence} gives extensive documentation
  for the JSON layout of the Event objects.
  \url{https://pluto.coe.fsu.edu/Proc4/RulesOfEvidence.pdf}. 

  Almond, R. G., Shute, V. J., Tingir, S. and Rahimi,
  S. (2018). Identifying Observable Outcomes in Game-Based Assessments.
  Talk given at the \emph{2018 Maryland Assessment Research
  Conference}.  Slides:
  \url{https://education.umd.edu/file/11333/download?token=kmOIVIwi},
  Video: \url{https://pluto.coe.fsu.edu/Proc4/Almond-Marc18.mp4}.

  Betts, B, and Smith, R. (2018). The Leraning Technology Manager's Guid
  to {xAPI}, Second Edition.  HT2Labs Research Report:
  \url{https://www.ht2labs.com/resources/the-learning-technology-managers-guide-to-the-xapi/#gf_26}.

}
\author{Russell Almond}
\note{

  The xAPI format (Betts and Smith, 2018) uses \dQuote{verb}s and
  \dQuote{object}s, but they are 
  much more complex objects.  In EIEvent, the verb and object
  vocabularies are driven by the \code{\link[Proc4]{app}} field of the
  \code{\linkS4class{Event}}, and all supporting details are put in the
  \code{\link[Proc4]{details}} field.


}
\seealso{

  Objects with verb and object fields:
  \code{\linkS4class{Event}}, \code{\linkS4class{Rule}}

  Related fields of the event object.
  \code{\link[Proc4]{app}} \code{\link[Proc4]{details}}

  The functions \code{\link{setJS}}, \code{\link{getJS}} and
  \code{\link{removeJS}} provide mechanisms for accessing the fields of
  an event object from \code{\linkS4class{Rule}} \link{Conditions} and
  \code{Predicates}. 
  
}
\examples{

ev2 <- Event("Phred","wash","window",
      timestamp=as.POSIXct("2018-12-21 00:02:01"),
      details=list(condition="streaky"))

stopifnot(verb(ev2)=="wash", object(ev2)=="window", context(ev2)==character(0))

r1 <- Rule(name="Coin Rule",
           doc="Set the value of the badge to the coin the player earned.",
           app="ecd://coe.fsu.edu/PPtest",
           verb="satisfied", object="game level",
           context="ALL",
           ruleType="Observable", priority=5,
           condition=list("event.data.badge"=c("silver","gold")),
           predicate=list("!set"=c("state.observables.badge"=
                                   "event.data.badge")))
stopifnot(verb(r1)=="satisfied", object(r1)=="game level",
          context(r1)=="ALL")

st <- Status("Phred","Level 0",timerNames=c("watch","stopwatch"),
   flags=list("list"=list("one"=1,"two"="too"),"vector"=(1:3)*10),
   observables=list("numeric"=12.5,char="foo",
                 "list"=list("one"="a","two"=2),"vector"=(1:3)*100),
   timestamp=as.POSIXct("2018-12-21 00:01"))

stopifnot(context(st)=="Level 0",oldContext(st)=="Level 0")
context(st) <- "Level 1"
stopifnot(context(st)=="Level 1",oldContext(st)=="Level 0")



}
\keyword{ manip }
\keyword{ interface }

