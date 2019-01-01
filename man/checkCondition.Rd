\name{checkCondition}
\alias{checkCondition}
\title{Checks to see if a condition in a EIEvent Rule is true.}
\description{

  An \code{\linkS4class{Rule}} object contains a list of
  \link{Conditions}.  The name of each condition is the name of a field
  of the \code{\linkS4class{Event}} or \code{\linkS4class{Status}}
  object.  For example,
  \code{"event.data.trophy"=list("?in"=c("gold","silver"))} would test
  if the trophy field was set to \dQuote{gold} or \dQuote{silver}.  The
  function \code{checkCondition} returns true if all of the conditions
  are satisfied and false if any one of them is not satisfied.

}
\usage{
checkCondition(conditions, state, event)
}
\arguments{
  \item{conditions}{A named list of conditions:  see details.}
  \item{state}{An object of class \code{\linkS4class{Status}} to be
    checked. }
  \item{event}{An object of class \code{\linkS4class{Event}} to be
    checked. }
}
\details{
  The condition of a \code{\linkS4class{Rule}} is a list of queries.
  Each query has the following form:

  \emph{field}\code{=list(}\emph{?op}\code{=}\emph{arg}\code{,...)}
    
  Here, \emph{field} is an identifier of a field in the
  \code{\linkS4class{Status}}  or \code{\linkS4class{Event}} object
  being tested.  This is in the dot notation (see
  \code{\link{getJS}}).  The query operator, \emph{?op} is one of
  the tests described in \link{Conditions}.  The
  \emph{arg} is a value or field reference that the field will be tested
  against. In other words, the query is effectively of the form
  \emph{field ?op arg}.  The \code{...} represents additional
  \code{?op}--\code{arg} pairs to be tested.
  The \emph{arg} can be a literal value (either scalar or vector) or a
  reference to another field in the \code{\linkS4class{Status}} or
  \code{\linkS4class{Event}} object using the dot notation.  

  In general, a rule contains a list of queries.  A rule is satisfied
  only if all of its queries are satisfied:  the function
  \code{checkCondition} checks if the rule is satisfied.  

  Finally, one special query syntax allows for expansion.  If the
  \emph{field} is replaced with \code{"?where"}, that is the query has
  the syntax \code{"?where"=}\emph{funname}, then the named R is
  called. This should be a function of two arguments, the status and the
  event, which returns a logical value.  The condition is satisfied if
  the funciton returns true.

}
\value{

  Returns a logical value:  \code{TRUE} if all conditions are satisfied,
  false otherwise.
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
\seealso{

  \code{\linkS4class{Rule}} describes the rule object and
  \link{Conditions} describes the conditions.

  The functions \code{\link{queryTest}} and \code{\link{queryTestScript}}
  can be used to test that rule conditions function properly.
  
  Other classes in the EIEvent system:
  \code{\linkS4class{EIEngine}}, \code{\linkS4class{Context}},
  \code{\linkS4class{Status}}, \code{\linkS4class{Event}},
  \code{\linkS4class{RuleTable}}.


}
\examples{

st <- Status("Phred","Level 1",timerNames=character(),
   flags=list(lastagent="lever",noobj=7,noagents=0),
   observables=list(),
   timestamp=as.POSIXct("2018-12-21 00:01"))


ev <- Event("Phred","test","message",
      timestamp=as.POSIXct("2018-12-21 00:01:01"),
      details=list(agent="lever"))


stopifnot(
checkCondition(list(event.data.agent=list("?eq"="lever")),   
               st,ev)==TRUE,             #Agent was lever.
checkCondition(list(event.data.agent="ramp"),               
               st,ev)==FALSE,            #Agent abbreviated form.
checkCondition(list(event.data.agent="state.flags.lastagent"), 
               st,ev)==TRUE,             #Same agent used.
checkCondition(list(state.flags.noobj=list("?lt"=10,"?gte"=5)), 
               st,ev)==TRUE,             #Between 5 and 10 objects.
checkCondition(list(event.data.agent=list("?in"=c("pendulum","springboard"))),
               st,ev)==FALSE,             #Between 5 and 10 objects.
#Abbreviated form (note lack of names)
checkCondition(list(event.data.agent=c("lever","springboard")),
               st,ev)==TRUE,
checkCondition(list(state.flags.lastagent=list("?isna"=TRUE)),
               st,ev)==FALSE,
checkCondition(list(state.flags.noagents=
                    list("?and"=list("?isna"=FALSE,"?gt"=0))),
               st,ev)==FALSE
) 

}
\keyword{ interface }
\keyword{ logic }


