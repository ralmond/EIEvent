\name{Conditions}
\alias{?eq}
\alias{?ne}
\alias{?gt}
\alias{?gte}
\alias{?lt}
\alias{?lte}
\alias{?in}
\alias{?nin}
\alias{?exisits}
\alias{?isnull}
\alias{?isna}
\alias{?regexp}
\alias{?any}
\alias{?all}
\alias{?not}
\alias{?and}
\alias{?or}
\alias{?where}
\title{Conditional query operators for Rules.}
\description{

  These are the conditional operators for a \code{\linkS4class{Rule}}.
  The check that the \code{target} value meets the specified condition
  (\code{cond}).  These are called by the function
  \code{\link{checkCondition}} which checks that the condition is correct.

}
\usage{
"?eq"(cond, target, state, event)
"?ne"(cond, target, state, event)
"?gt"(cond, target, state, event)
"?gte"(cond, target, state, event)
"?lt"(cond, target, state, event)
"?lte"(cond, target, state, event)
"?in"(cond, target, state, event)
"?nin"(cond, target, state, event)
"?exists"(cond, target, state, event)
"?isnull"(cond, target, state, event)
"?isna"(cond, target, state, event)
"?regexp"(cond, target, state, event)
"?any"(cond, target, state, event)
"?all"(cond, target, state, event)
"?not"(cond, target, state, event)
"?and"(cond, target, state, event)
"?or"(cond, target, state, event)
}
\arguments{
  \item{cond}{This is the value of the condition clause (the argument)
    of the query. }
  \item{target}{This is the value of the current state of the referenced
    field in the query.}
  \item{state}{This is a \code{\linkS4class{Status}} object used to
    resovle dot notation references.}
  \item{event}{This is a \code{\linkS4class{Event}} object used to
    resovle dot notation references.}
}
\details{

  The condition of a \code{\linkS4class{Rule}} is a list of queries.
  Each query has the following form:

  \emph{field}\code{=list(}\emph{?op}\code{=}\emph{arg}\code{,...)}
    
  Here, \emph{field} is an identifier of a field in the
  \code{\linkS4class{Status}}  or \code{\linkS4class{Event}} object
  being tested.  This is in the dot notation (see
  \code{\link{getJS}}).  The query operator, \emph{?op} is one of
  the tests described in the section \sQuote{Condition Operators}.  The
  \emph{arg} is a value or field reference that the field will be tested
  against. In other words, the query is effectively of the form
  \emph{field ?op arg}.  The \code{...} represents additional
  \code{?op}--\code{arg} pairs to be tested.

  The \emph{arg} can be a literal value (either scalar or vector) or a
  reference to another field in the \code{\linkS4class{Status}} or
  \code{\linkS4class{Event}} object using the dot notation.  

  In general, a rule contains a list of queries.  A rule is satisfied
  only if all of its queries are satisfied (essentially joining the
  queries with a logical-and).  At the present time, the only way to get
  a logical-or is to use multiple rules.

  Finally, one special query syntax allows for expansion.  If the
  \emph{field} is replaced with \code{"?where"}, that is the query has
  the syntax \code{"?where"=}\emph{funname}, then the named R is
  called. This should be a function of two arguments, the status and the
  event, which returns a logical value.  The condition is satisfied if
  the funciton returns true.

}
\section{Condition Operators}{
  The syntax for the condition part of the rule resembles the query
  lanugage used in the Mongo database (MongoDB, 2018).  There are two
  minor differences:  first the syntax uses R lists and vectors rather
  than JSON objects and second the \sQuote{$} operators are replaced
  with \sQuote{?} operators.

  In general, each element of the list should have the form
  \emph{field}\code{=c(}\emph{?op}\code{=}\emph{arg}\code{)}.  In this
  expression, \emph{field} references a field of either the
  \code{\linkS4class{Status}} or \code{\linkS4class{EIEngine}} (see
  sQuote{Dot Notation} section above), \emph{?op} is one of the test
  operators below, and the argument \emph{arg} is a litteral value
  (which could be a list) or a character string in dot notation
  referencing a field of either the \code{Status} or \code{Event}.  If
  \emph{?op} is omitted, it is taken as equals if \emph{arg} is a scalar
  and \emph{?in} if value is a vector.  For more complex queries where
  \code{arg} is a more complex expresison, the \code{c()} function is
  replaced with \code{list()}.

  The following operators (inspired from the operators used in the Mongo
  database, Mongo DB, 2018, only with \sQuote{?} instead of \sQuote{$})
  are currently supported: 
  \describe{
    \item{\code{?eq}, \code{?ne}}{ These are the basic operators, which
      test if the field is (not) equal to the argument.}
    \item{\code{?gt}, \code{?gte}, \code{?lt}, \code{?lte}}{These test
      if the field is greater than (or equal to) or less than (or equal
      to) the argument. Note that \code{c("?lt"=low,"?gt"=high)} can be
      used to test if the value of the field is between the arguments
      \emph{low} and \emph{high}.}
    \item{\code{?in}, \code{?nin}}{These assume that the argument is a
      vector and are satisfied if the value of the field is (not) in the
      vector.}
    \item{\code{?exists}, \code{?isnull}, \code{?isna}}{These test if
      the field exists, contains a \code{NULL} (often true if the field
      does not exist) or contains an \code{NA}.  The \emph{arg} should
      be \code{TRUE} or \code{FALSE} (where false inverts the test.)}
    \item{\code{?any}, \code{?all}}{These operators assume that the
      field contains a vector and check if any (or all) of the elements
      satisfy the condition given in the argument.  Thus, the argument is
      another expression of the form
      \code{=c(}\emph{?op}\code{=}\emph{value}\code{)}.  For example
      \emph{field}\code{=list("?any"=c("?gt"=3))} will be satisfied if
      any element of \emph{field} is greater than 3.}
    \item{\code{?not}}{The argument of this query should be another query
      involving the target field.  The not query is satisfied if the
      inner query is not satistified.}
    \item{\code{?or}, \code{?and}}{In both of these cases, the
      \emph{arg} should be a list of queries for the applicable
      field.  The \code{?or} query is satisfied if any of the inner
      queries is satisfied, and the \code{?and} query is satistified if
      all of the inner queries are satisfied.  Like the R \code{||} (
      \code{&&}) operator, the \code{?or} (\code{?and}) query runs the
      subqueries in order and stops at the first true (false) subquery.}
    \item{\code{?regex}}{This query uses regular expression matching.
    The argument should be a regular expression (see \link[base]{regex} for
    a description of R regular expressions).  The query is satisfied if
    the value of the field matches the regular expression.}
    \item{\code{?where}}{This query is a trapdoor that allows arbitrary
      R code to be run to run as the condition.  This has a special
      syntax: \code{"?where"=}\emph{funname}, where the \code{?where}
      operator takes the place of the field and \emph{funname} give the
      name of a function.  This should be a function of two arguments,
      the status and the event, which returns a logical value.  The
      condition is satisfied if the funciton returns true.}
  }

  Although the \code{?or} operator allows for logical-or expressions for
  a single field, it does not extend to multiple fields.  However, this
  can be accomplished with separate rules.

}
\section{Expansion Mechanisms}{

  The special \dQuote{?where} form obviously allows for expansion.  It
  is particularly designed for queries which involve multiple fields in
  a complex fashion.

  It is also possible to expand the set of \emph{?op} functions.  The
  method used for dispatch is to call
  \code{do.call(op,list(cond, target, state, event))} where \emph{cond}
  is everything after \emph{?op}\code{=}\emph{cond} in the query
  expression.  (This is the same syntax as the supplied operators).  

}
\section{Condition Testing}{

  These functions are comming soon.
  
}
\value{

  The condition operators always return a logical value, \code{TRUE} if
  the query is satisfied, and \code{FALSE} if not.
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

  \code{\linkS4class{Rules}} describes the rule object and
  \link{Predicates} describes the predicates.
  
  Other classes in the EIEvent system:
  \code{\linkS4class{EIEngine}}, \code{\linkS4class{Context}},
  \code{\linkS4class{Status}}, \code{\linkS4class{Event}},
  \code{\linkS4class{RuleTable}}.


}
\examples{

list(
event.data.agent=list("?eq"="lever"),   #Agent was lever.
event.data.agent="lever",               #Agent abbreviated form.
event.data.agent=list("?ne"="ramp"),   #Agent was not a ramp.
event.data.agent="state.flag.lastagent", #Same agent used.
state.flags.noobj=list("?lt"=10),       #Fewer than 10 objects used.
state.flags.noobj=list("?lt"=10,"?gte"=5), #Between 5 and 10 objects.

#Agent is a lever or a springboard.
event.data.agent=list("?in"=c("lever","springboard")),
#Abbreviated form (note lack of names)
event.data.agent=c("lever","springboard"),
#Agent was not a ramp or a pendulum.
event.data.agent=list("?nin"=c("ramp","pendulum")),   

## Checking for existence of fields and NA values.
state.timers.learningsupport=list("?exists"=TRUE),
event.data.newvalue=("?isnull"=FALSE),
state.flags.lastagent=list("?isna"=TRUE),

## Was the slider a blower (name starts with blower).
event.data.slider=list("?regexp"="^[Bb]lower.*"),

## These assume field is a vector.
state.flags.agentsused=list("?any"=list("?eq"="pendulum")),
state.flags.agentsused=list("?any"="pendulum"), #Abbreviated form.
state.flags.agentsused=list("?all"=list("?eq"="ramp")),
state.flags.agentsused=list("?any"="ramp"), #Abbreviated form.

## ?not
state.flags.agentsused=list("?not"=list("?any"="ramp")), 

## ?and, ?or -- note these stop as soon as falsehood (truth) is proved.
state.flags.noagents=list("?and"=list("?is.na"=FALSE,"?gt"=0)),
state.flags.noagents=list("?or"=list("?is.na"=TRUE,"?eq"=0)),

)


}
\keyword{ interface }
\keyword{ logic }
