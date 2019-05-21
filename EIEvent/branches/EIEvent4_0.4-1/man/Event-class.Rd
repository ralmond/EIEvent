\name{Event-class}
\Rdversion{1.1}
\docType{class}
\alias{Event-class}
\alias{show,Event-method}
\alias{toString,Event-method}
\alias{all.equal.Event}
\title{Class \code{"Event"}}
\description{

  This class represents a generalize event happening in a simulation or
  a game.  All events have common metadata which identify what happened
  (\code{verb}), what was effected (\code{object}), the time of the
  action (\code{timestamp}) as well as the application \code{app}
  and user (\code{uid}).  However, the \code{\link[Proc4]{details}}
  field of the event will differ dependiing on the \code{app},
  \code{verb}, and \code{object}.

}
\section{Objects from the Class}{

Objects can be created by calls the the \code{\link{Event}} function,
although more typically they are passed to the
\code{\linkS4class{EIEngine}} by the presentation process.

The event object is a simplifcation of the xAPI events (Betts and Ryan,
2018).  In particular, in the xAPI format, both the \code{verb} and
\code{object} fields are complex objects which have a long URL-like
identifier to make sure they are unique across applications.  In the
EIEvent prototocl, only the \code{app} field is given a long URL-like
name.  The application should define the acceptable vocabulary for verbs
and objects, which should correpsond to the \code{verb} and
\code{object} fields of the \code{\linkS4class{Rule}} objects.

}
\section{Slots}{
  \describe{
    \item{\code{verb}:}{Object of class \code{"character"} which
      provides an identifier for the action which just occurred. }
    \item{\code{object}:}{Object of class \code{"character"} which
      provides an identifier for the direct object of the action which
      just occured.}
    \item{\code{_id}:}{Object of class \code{"character"} the Mongo
      database identifier; this should not be modified. }
    \item{\code{app}:}{Object of class \code{"character"} a unique
      identifier for the application.  This defines which
      \code{\linkS4class{EIEngine}} is used to handle the event. }
    \item{\code{uid}:}{Object of class \code{"character"} identifier for
      the user (student or examinee).}
    \item{\code{context}:}{Object of class \code{"character"} an
      identifier for the context, for applications in which the context
      is determined by the presentation process. }
    \item{\code{sender}:}{Object of class \code{"character"} the name of
      the process which generated the event, usually the presentation
      process. }
    \item{\code{mess}:}{Object of class \code{"character"} a title for
      the message, used by the \code{\linkS4class{P4Message}} dispatch
      system. }
    \item{\code{timestamp}:}{Object of class \code{"POSIXt"} giving the
      time at which the event occurred. }
    \item{\code{data}:}{Object of class \code{"list"} giving the
      contents of the message.  This will be details specific to the
      verb and object. }
  }
}
\section{Extends}{

  Class \code{"\linkS4class{P4Message}"}, directly.  All fields except
  \code{verb} and \code{object} are inherited from the parent.

  Note that the \code{Event} is the basic message sent from the
  presentation process to the evidence identification process in the
  four-process architecture (Almond, Steinberg and Mislevy, 2002).  It
  has been extended slightly, borrowing (and simplifying) the
  \code{verb} and \code{object} header fields from the xAPI format
  (Almond, Shute, Tingir, and Rahimi, 2018).

  Note that \code{Physics Playground} uses a slightly different
  architecture.  The presentation uses Learning Locker (HT2Labs, 2018)
  to log events in the xAPI format into a mongo database.  When the game
  level completes, a message is sent to the
  \code{\linkS4class{EIEngine}} which extracts the relevant messages
  from the Learning Locker database and simplifies them into the event
  format. 

}
\section{Methods}{
  \describe{
    \item{as.jlist}{\code{signature(obj = "Event", ml = "list")}: ... }
    \item{object}{\code{signature(x = "Event")}: Fetches the object component. }
    \item{verb}{\code{signature(x = "Event")}: Fetches the verb component. }
    \item{show}{\code{signature(object = "Event")}: Prints event object. }
    \item{toString}{\code{signature(x = "Event")}: Produces <>
      representation of object. } 
    \item{all.equal.Event}{\code{(target, current, ..., checkTimestamp=FALSE,
        check_ids=TRUE)}:  (S3 method) Checks for equality.  The
      \code{checkTimestamp} flag controls whether or not the timestamp is
      checked.  The \code{check_ids} flag controls whether or not the
      database IDs are checked.}
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

  Betts, B, and Smith, R. (2018). The Leraning Technology Manager's Guid
  to {xAPI}, Second Edition.  HT2Labs Research Report:
  \url{https://www.ht2labs.com/resources/the-learning-technology-managers-guide-to-the-xapi/#gf_26}.

  HT2Labs (2018). Learning Locker Documentation.
  \url{https://docs.learninglocker.net/welcome/}.
}

\author{Russell Almond}
\seealso{

  The event class is a subclass of
  \code{\link[Proc4]{P4Message}}. 

  Other classes in the EIEvent system:
  \code{\linkS4class{EIEngine}}, \code{\linkS4class{Context}},
  \code{\linkS4class{Status}}, \code{\linkS4class{Rule}}.

  Methods for working with events:
  \code{\link{Event}}, \code{\link{parseEvent}}

}
\examples{
showClass("Event")
}
\keyword{classes}
