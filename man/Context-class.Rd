\name{Context-class}
\Rdversion{1.1}
\docType{class}
\alias{Context-class}
\alias{toString,Context-method}
\title{Class \code{"Context"}}
\description{

  This is a descriptor for a measurement context (e.g., item or game
  level) in an assessment system.  A context plays the role of a
  \emph{task} in the four process architecture (Almond, Steinberg, and
  Mislevy, 2002), but allows for the measurement context to be
  determined dynamically from an extended task.  (Almond, Shute, Tingir,
  and Rahimi, 2018).

  The primary of use of the \code{Context} object is determining for
  which events a rule is applicable.  The \code{belongsTo} allows
  designers of an evidence rule system to define context groups for
  rules which are applicable in multiple contexts.

}
\section{Objects from the Class}{

Context object can be created by calls to the \code{\link{Context}())}
function.  Most of the fields in the context are documentation; however,
two, the \code{cid} and \code{belongsTo} fields, play a special role in
the rule dispatch logic.  The \code{cid} field is the identifier of the
context as used in the \code{\linkS4class{Rule}} and
\code{\linkS4class{Status}} classes.

The \code{belongsTo} attribute sets up a hierarchy of classes.  In
particular, each value of the of this field (if set) should be the
\code{cid} of a context group to which this context belongs.  Context
groups are also \code{Context} objects and can in turn belong to larger
groups.  Currently, inheritance is not supported, so that the all
parents (direct and indirect) need to be listed).

}
\section{Context Resolution}{

  When the \code{\linkS4class{EIEngine}} processes an
  \code{\linkS4class{Event}}, it checks the context of the current
  \code{\linkS4class{Status}} object.  It then
  searches the rule table for all rules which match on the \code{verb}
  and \code{object} fields of the event and the \code{context} field of
  the status.  A rule is considered applicable if one of the following
  conditions is met.
  \enumerate{
    \item{The \code{context} fields of the \code{\linkS4class{Status}}
      and \code{\linkS4class{Rule}} class match exactly.}
    \item{The \code{context} fields of the \code{\linkS4class{Rule}}
      class matches the \code{cid} of one of the entries in the
      \code{belongsTo} field of the \code{\linkS4class{Status}}.}
    \item{The \code{context} fields of the \code{\linkS4class{Rule}} is
      the keyword \dQuote{ANY}, \emph{i.e.}, the rule is applicable to
      all classes.}
  }

  This is actually accomplished by using the function
  \code{applicableContexts} which creates a list of the current context
  and all of the context groups that it matches.  The
  \code{\linkS4class{EIEngine}} then grabs from the rule table all rules
  which match one of the applicable contexts (including \dQuote{ANY}).

  This should seem similar to the way that method dispatch works for S4
  classes (see \link[methods]{Introduction} to the methods package).
  The \code{Context} \code{belongsTo} hiearchy is similar to the
  inheritence hierarchy of a typical class system.  There are two key
  differences.  First, inheritense is not currently supported with
  contexts; all context groups must be explicitly listed in the
  \code{belongsTo} field.  Second, while the S4 method dispatch
  mechanism searches for all methods which are applicable to the current
  objects, it only executes the most specific method.  The table
  dispatch mechanism executes \emph{all} the applicable rules and makes
  no attempt to sort them.

}
\section{Slots}{
  \describe{
    \item{\code{_id}:}{Object of class \code{"character"} which is the
      id in the Mongo database; this generally should not be changed.}
    \item{\code{cid}:}{Object of class \code{"character"} which provides
    a unique identifier for the context.}
    \item{\code{name}:}{Object of class \code{"character"} which
      provides a human readable name for the context.}
    \item{\code{number}:}{Object of class \code{"integer"} which
      provides a numeric index for the context. }
    \item{\code{belongsTo}:}{Object of class \code{"character"} which
      gives a list of context IDs for context groups to which this
      context belongs.}
    \item{\code{doc}:}{Object of class \code{"character"} which provides
      extended documentation for the context group.}
    \item{\code{app}:}{Object of class \code{"character"} which a unique
      identifier for the application in which this context is applicable.}
  }
}
\section{Methods}{
  \describe{
    \item{as.jlist}{\code{signature(obj = "Context", ml = "list")}: Used
      in converting the object to JSON for storing in a Mongo database,
      see \code{\link[mongo]{as.json}}.}
    \item{belongsTo}{\code{signature(c = "Context")}: Returns the value
      of the \code{belongsTo} field }
    \item{belongsTo<-}{\code{signature(c = "Context")}: Sets the value
      of the \code{belongsTo} field. }
    \item{cid}{\code{signature(c = "Context")}: Returns the context ID. }
    \item{doc}{\code{signature(x = "Context")}: Returns the
      documentation string. }
    \item{name}{\code{signature(x = "Context")}: Returns the name of the
      context.}
    \item{number}{\code{signature(c = "Context")}: Returns the number id
      of the context.}
    \item{number<-}{\code{signature(c = "Context")}: Sets the numeric id
      of the context.}
    \item{app}{\code{signature(x = "Context")}: Returns the app
      identifier of the context.}
    \item{show}{\code{signature(object = "Context")}: Prints the object
      in a \dQuote{#<Context >} format.}
    \item{toString}{\code{signature(x = "Context",...)}: Converts the object
      to a string in a \dQuote{#<Context >} format.}
  }
}
\references{

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

  It seems natural to create a full inheritance hierarchy for contexts.
  Probably available in a future version.  For now, explicity listing
  all parents seems easier to implement.
}
\seealso{

  The \code{\linkS4class{ContextSet}} is an object which hold a
  collection of contexts.

  Other classes in the EIEvent system:
  \code{\linkS4class{EIEngine}}, \code{\linkS4class{Event}},
  \code{\linkS4class{Status}}, \code{\linkS4class{Rule}}.

  Methods for working with contexts:
  \code{\link{Context}}, \code{\link{applicableContexts}},
  \code{\link{parseContext}}, \code{\link{name}}, \code{\link{doc}},
  \code{\link{cid}}, \code{\link{number}}, \code{\link[Proc4]{app}}
}
\examples{
showClass("Context")
}
\keyword{classes}
