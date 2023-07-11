\name{EITest-class}
\Rdversion{1.1}
\docType{class}
\alias{EITest-class}
\alias{doc,EITest-method}
\alias{event,EITest-method}
\alias{final,EITest-method}
\alias{initial,EITest-method}
\alias{name,EITest-method}
\alias{show,EITest-method}
\alias{toString,EITest-method}

\title{Class \code{"EITest"}}
\description{

  An object describing a test case for a certain evidence identification
  system.  It describes the input state, the event and the target output
  state so that the interaction of the rules can be checked.

}
\section{Objects from the Class}{

An EITest object consists of a test.  The test case has an \emph{initial}
\code{\linkS4class{Status}}, a triggering \code{\linkS4class{Event}},
and an expected \emph{result} \code{Status}.  If the goal is
intead to check output messages, the result can be a
\code{\linkS4class{P4Message}} or a list of such messages.

Objects can be created by calls of the form \code{\link{EITest}(...)},
or from JSON through \code{\link{parseEITest}}.

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
    \item{\code{initial}:}{Object of class \code{\linkS4class{Status}} which
      describes the initial state of the system before the test. }
    \item{\code{event}:}{Object of class \code{\linkS4class{Event}}
      which describes the incoming event to which the system is reacting. }
    \item{\code{final}:}{Object of class \code{\linkS4class{Status}}
      which describes the expected final state of the system after
      applying the rules, or an object of class
      \code{\linkS4class{P4Message}} describing the generated message.}
  }
}
\section{Methods}{
  \describe{
    \item{as.jlist}{\code{signature(obj = "EITest", ml = "list")}:
      helper function for converting the test into a JSON object, see
      \code{\link[mongo]{as.json}}. }
    \item{doc}{\code{signature(x = "EITest")}: Returns the
      documenation string. }
    \item{event}{\code{signature(x = "EITest")}: Returns the
      triggering event for the test. }
    \item{final}{\code{signature(x = "EITest")}: Returns the expected
      final state of the system. }
    \item{initial}{\code{signature(x = "EITest")}: Returns the initial
      state of the test.}
    \item{name}{\code{signature(x = "EITest")}: Returns the name of
      the test. }
    \item{show}{\code{signature(object = "EITest")}: Displays the rule
      object.}
    \item{toString}{\code{signature(x = "EITest")}: Returns a string
      describing the rule.}
  }
}
\references{

  The document \dQuote{Rules Of Evidence} gives extensive documentation
  for the rule system.
  \url{https://pluto.coe.fsu.edu/Proc4/RulesOfEvidence.pdf}.

}
\author{Russell G. Almond}
\seealso{

  The functions \code{\link{EITest}} and \code{\link{parseEITest}}
  are used to construct the rule.

  The functions \code{\link{name}}, \code{\link{doc}},
  \code{\link{initial}}, \code{\link{event}},
  and \code{\link{final}} access the
  components of the test.

  The \code{\linkS4class{TestSet}} maintains a collection of tests for a
  particular application.

}
\examples{
showClass("EITest")
}
\keyword{classes}
