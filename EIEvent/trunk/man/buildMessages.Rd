\name{buildMessages}
\alias{buildMessages}
\alias{!send}
\alias{!send1}
\alias{!send2}
\title{These functions build messages for Trigger Rules.}
\description{

  A trigger rule has a special predicate function \code{!send} which
  build a \code{\linkS4class{P4Message}} object to send to listeners of
  the \code{\linkS4class{EIEngine}} (through the
  \code{\link[Proc4]{notifyListeners}} method.

}
\usage{
buildMessages(predicate, state, event)
"!send"(predicate, state, event)
"!send1"(predicate, state, event)
"!send2"(predicate, state, event)
}
\arguments{
  \item{predicate}{One element of the predicate, or in the case of
    \code{buildMessages} the complete predicate. }
  \item{state}{An object of class \code{\linkS4class{Status}} giving the
    current state of the system.  Note that often \code{\link{context}}
    and \code{oldContext} will be different.}
  \item{event}{An object of class \code{\linkS4class{Event}} that gives
    the event that triggered the message.}
}
\details{

  The function \code{!send} builds a \code{\linkS4class{P4Message} which
  is then sent to the registered listeners of the
  \code{\linkS4class{EIEngine}}.  The default message is
  \dQuote{"Observables Available"}, which is what the Evidence
  Accumulation process is listening for.  The default content is all of
  the observables.

  The rule can override certian defaults of the message by setting
  appropriate fields of the object that is the value of the \code{!send}
  element in the predicate.
  \describe{
    \item{context}{The context for the message.  Default is
      \code{oldContext(event)}.  Value can be a direct value of a field
      name (starting with \dQuote{event.data.} or \dQuote{state.}.}
    \item{mess}{The text (subject) of the message.  Default is
      \dQuote{Observables Available}.  Value can be a direct value of a field
      reference (starting with \dQuote{event.data.} or \dQuote{state.}).} 
    \item{data}{The fields to be added in the details section of the
      message.  If omitted, all of the observables are sent (with their
      default names).  If supplied this should either be an named
      character vector or named list.  The names are used as the names
      of the details portion of the message, and the values are field
      references (starting with \dQuote{event.data.} or \dQuote{state.})
      for where to find the values.
  }

  The function \code{buildMessages} runs through multiple elements of
  the predicate and builds multiple messages.  The additional predicates
  \code{!send1} and {!send2} are there to allow for additional
  messages.  This will also work with the name of an arbitrary R
  function which takes \code{(predicate, state, event)} as an argument
  list and returns a message.

}
\value{

  The function \code{!send} returns an object of class
  \code{\linkS4class{P4Message}} with the following fields:

  \item{app}{The application, value is \code{app(event)}.}
  \item{uid}{The user ID, value is \code{uid(event)}.}
  \item{context}{The context (task) for this message, default is the
    value of \code{oldContext(state)}.}
  \item{mess}{The message header.  The default value is
    \dQuote{Observables Available.}}
  \item{sender}{The value is \dQuote{EIEvent}.}
  \item{timestamp}{The timestamp for the message.  The value is
    \code{timestamp(event)}.}
  \item{details}{The data that should be sent with the message.  The
    default is all of the \code{\link{observables}} of the
    \code{\linkS4class{Status}} argument.}

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

}
\author{Russell Almond}
\note{

  JSON parsers are not happy with multiple fields with the same name, so
  the \code{!send1} and \code{!send2} operators are provided to get
  around this problem.
}
\seealso{


  \code{\linkS4class{Rule}} describes the rule object and
  \link{Conditions} describes the Conditions, and \link{Predicates} the
  predicates in general.

  The functions \code{\link{testRule}} and \code{\link{testRuleScript}}
  can be used to test that rule conditions and predicates function
  properly together.
  
  Other classes in the EIEvent system:
  \code{\linkS4class{EIEngine}}, \code{\linkS4class{Context}},
  \code{\linkS4class{Status}}, \code{\linkS4class{Event}},
  \code{\linkS4class{RuleTable}}.

  The \code{\linkS4class{P4Message}} class is from the
  \link{Proc4-package}.   The \code{\link[Proc4]{notifyListeners}}
  method describes the Proc4 message passing system.

}
\examples{

st9 <- Status(uid="Test0",context="SpiderWeb",
             timestamp=as.POSIXct("2018-09-25 12:13:30 EDT"),
             observables=list(agentsUsed=list("Pendulum"),
                              lastAgent="Pendulum",
                              badge="gold"))


evnt10 <- Event(uid="Test0",
               verb="satisfied",object="game level",
               context="SpiderWeb",
               timestamp=as.POSIXct("2018-09-25 12:13:30 EDT"),
               details= list("badge"="gold"))

mess0 <- buildMessages(list("!send"=list()),st9,evnt10)[[1]]
stopifnot(mess(mess0)=="Observeables Available",
          length(details(mess0)) == 3L)
mess1 <- buildMessages(list("!send1"=list()),st9,evnt10)[[1]]
messb <- buildMessages(list("!send"=list(mess="Badge",
                 data=c("badge"="state.observables.badge"))),
         st9,evnt10)[[1]]
stopifnot(mess(messb)=="Badge",length(details(messb))==1L)


}
\keyword{ interface }

