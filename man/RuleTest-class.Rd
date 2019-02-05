\name{RuleTest-class}
\Rdversion{1.1}
\docType{class}
\alias{RuleTest-class}
\alias{doc,RuleTest-method}
\alias{event,RuleTest-method}
\alias{final,RuleTest-method}
\alias{initial,RuleTest-method}
\alias{name,RuleTest-method}
\alias{queryResult,RuleTest-method}
\alias{rule,RuleTest-method}
\alias{show,RuleTest-method}
\alias{toString,RuleTest-method}

\title{Class \code{"RuleTest"}}
\description{A rule along with a test case for verifying the rule.}
\section{Objects from the Class}{

A rule test object consists of a \code{\linkS4class{Rule}} object, plus
a test case for the rule.  The test case has a \emph{initial}
\code{\linkS4class{Status}}, a triggering \code{\linkS4class{Event}},
and an expected final \code{\link{Status}}.  It also has a logical
\code{queryResult} field which describes whether or not the rule'
\code{\link{Conditions}} should be met in the test case.

Objects can be created by calls of the form \code{\link{RuleTest}(...)},
or from JSON through \code{\link{parseRuleTest}}.

}
\section{Slots}{
  \describe{
    \item{\code{_id}:}{Object of class \code{"character"}, the internal
      mongo identifier. }
    \item{\code{app}:}{Object of class \code{"character"}, the unique
      identifier for the application to which this belongs. }
    \item{\code{name}:}{Object of class \code{"character"}, a human
      readable name for the test, used in reporting. }
    \item{\code{doc}:}{Object of class \code{"character"}, a
      documentation string describing the test. }
    \item{\code{initial}:}{Object of class \code{"Status"} which
      describes the initial state of the system before the test. }
    \item{\code{event}:}{Object of class \code{"Event"} which describes
      the incoming event to which the rule is reacting. }
    \item{\code{rule}:}{Object of class \code{"Rule"}, the rule being tested. }
    \item{\code{queryResult}:}{Object of class \code{"logical"} which
      specifies whether or not the rule \code{\link{Conditions}} are met. }
    \item{\code{final}:}{Object of class \code{"Status"} which describes
      the expected final state of the system after applying the rule.}
  }
}
\section{Methods}{
  \describe{
    \item{as.jlist}{\code{signature(obj = "RuleTest", ml = "list")}:
      helper function for converting the test into a JSON object, see
      \code{\link[Proc4]{as.json}}. }
    \item{doc}{\code{signature(x = "RuleTest")}: Returns the
      documenation string. }
    \item{event}{\code{signature(x = "RuleTest")}: Returns the
      triggering event for the test. }
    \item{final}{\code{signature(x = "RuleTest")}: Returns the expected
      final state of the system. }
    \item{initial}{\code{signature(x = "RuleTest")}: Returns the initial
      state of the test.}
    \item{name}{\code{signature(x = "RuleTest")}: Returns the name of
      the test. }
    \item{queryResult}{\code{signature(x = "RuleTest")}: Returns the
      expected value of the \code{\link{Conditions}} part of the rule.}
    \item{rule}{\code{signature(x = "RuleTest")}: Returns the rule being
    tested.}
    \item{show}{\code{signature(object = "RuleTest")}: Displays the rule
      object.}
    \item{toString}{\code{signature(x = "RuleTest")}: Returns a string
      describing the rule.}
  }
}
\references{

  The document \dQuote{Rules Of Evidence} gives extensive documentation
  for the rule system.
  \url{https://pluto.coe.fsu.edu/Proc4/RulesOfEvidence.pdf}. 

}
\author{Russell G. Almond}
\note{


}
\seealso{

  The functions \code{\link{testQuery}}, \code{\link{testPredicate}},
  and \code{\link{testRule}} are used to actually execute the tests.

  The functions \code{\link{RuleTest}} and \code{\link{parseRule}} test
  are used to construct the rule.

  The functions \code{\link{name}}, \code{\link{doc}},
  \code{\link{initial}}, \code{\link{event}}, \code{\link{rule}},
  \code{\link{queryResult}}, and \code{\link{final}} access the
  components of the test.

  The \code{\linkS4class{TestSet}} maintains a collection of tests for a
  particular application.
  
}
\examples{
showClass("RuleTest")
}
\keyword{classes}
