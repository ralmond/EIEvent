\name{EIEngine-class}
\Rdversion{1.1}
\docType{class}
\alias{EIEngine-class}
\alias{notifyListeners,EIEngine-method}

\title{Class \code{"EIEngine"}}
\description{

  This is the main worker class for the EIEvent process.  It handles
  grapping messages and processing them.

}
\section{Extends}{

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}
\section{Methods}{
  \describe{
    \item{notifyListeners}{\code{signature(sender = "EIEngine")}: This
      sends a message to the registered listeners of the EIEngine.}
  }
}
\references{

  The document \dQuote{Rules Of Evidence} gives extensive documentation
  for the rule system.
  \url{https://pluto.coe.fsu.edu/Proc4/RulesOfEvidence.pdf}. 

}
\author{Russell Almond}
\note{

  A different EIEninge process is associated with each application
  (application ID), so different applicaitons can run in parallel.  More
  sophisticated concurancy checking is not currently available.
  
}
\section{Tracing}{

  The EIEngine uses the \code{\link[futile.logger]{flog.logger}}
  system.  In particular, setting the threshold for the \dQuote{EIEvent}
  logger, will change the amount of information sent to the log file.

}
\seealso{

  The function \code{\link{EIEngine}} tells about the construction and
  the relationship between the Engine and the database.

  The primary classes in the EIEvent system are:
  \code{\linkS4class{EIEngine}}, \code{\linkS4class{Context}},
  \code{\linkS4class{Status}}, \code{\linkS4class{Event}},
  \code{\linkS4class{Rule}}.

  The EIEngine class is a container for the following classes:
  \code{\linkS4class{UserRecordSet}}, \code{\linkS4class{RuleTable}},
  \code{\linkS4class{ContextSet}},  \code{\linkS4class{TestSet}} and
  \code{\link[Proc4]{ListenerSet}}, 

  \code{\link[futile.logger]{flog.logger}}

}
\examples{
showClass("EIEngine")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{app}:}{Object of class \code{character}.  This is the
      long url-like name of the application.  Note that it is a key to
      all of the database lookup.s }
    \item{\code{dburi}:}{Object of class \code{character} giving the URI
    for the link to the Mongo database}
    \item{\code{dbname}:}{Object of class \code{character} giving the
      name of the user database}
    \item{\code{userRecords}:}{Object of class \code{\linkS4class{UserRecordSet}}
      giving the user record set for the applcation. }
    \item{\code{rules}:}{Object of class \code{\linkS4class{RuleTable}} giving the
      rule set for the application}
    \item{\code{contexts}:}{Object of class
      \code{\linkS4class{ContextSet}} giving the 
      set of contexts supported by the application.}
    \item{\code{events}:}{Object of class \code{\linkS4class{MongoDB}} a
      mongo DB 
      connection which allows access to events.  Note that this is
      created when first requested, so should be accessed using the
      eventsdb() method. }
    \item{\code{ruleTests}:}{Object of class \code{\linkS4class{TestSet}} give a
      collection of tests for the rule system. }
    \item{\code{listenerSet}:}{Object of class \code{\link[Proc4]{ListenerSet}} giving
    the registered listeners for messages sent by the system.}
  }
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{initialize(app, listeners, username, password, host,
        port, dbname, ...)}:}{ This method sets up the object.  Note
      that the listeners object is passed in as an argument, as are the
      database credentials.}
    \item{\code{newStudent(uid)}:}{ Generates a new student record.
      (Call to \code{\linkS4class{UserRecordSet}$newStudent()})}
    \item{\code{getStatus(uid)}:}{ Feteches the current status for a
      student from the database.  (Call to
      \code{\linkS4class{UserRecordSet}$getStatus()})} 
    \item{\code{saveStatus(state)}:}{ Saves an updated status to the
      database. (Call to \code{\linkS4class{UserRecordSet}$saveStatus()})  }
    \item{\code{getContext(id)}:}{ This is a call to
      \code{\link{matchContext}}. } 
    \item{\code{addContexts(conmat)}:}{ This is a call to
      \code{\link{loadContexts}}. }
    \item{\code{clearContexts()}:}{ This is a call to
      \code{\linkS4class{ContextSet}$clearAll()}.  }
    \item{\code{eventdb()}:}{ This returns the event database handle,
      creating it if it is not yet created.  It is recommended to use
      this method rather than access the slot directly. }
    \item{\code{findRules(verb, object, context, phase)}:}{ This
      function finds the potentially applicable for the current verb,
      object, context and phase.}
    \item{\code{loadRules(rlist, stopOnDups)}:}{ This function loads
      rules into the rule table using \code{\link{loadRulesFromList}}.}
    \item{\code{loadAndTest(script, stopOnDups)}:}{ This function loads
      rules into the rule table from test scripts using
      \code{\link{testAndLoad}}.}
    \item{\code{runRule(state, event, rule, phase)}:}{ This function
      checks the condition of the rule, and if the condition is true,
      uses the predicate to update the state. }
    \item{\code{runTRule(state, event, rule)}:}{ Run Trigger Rule.  This
    function checks the condition of the rule, and if it is true,
    generates a message to be sent and notifies the listeners of the message.}
    \item{\code{runStatusRules(state, event)}:}{ This function finds the
    applicable status rules and runs them.}
    \item{\code{runObservableRules(state, event)}:}{ This function finds
      the applicable observable rules and then runs them. }
    \item{\code{runContextRules(state, event)}:}{ This function runs the
      context rules, until the context changes, at which point it returns
      the final state.}
    \item{\code{runTriggerRules(state, event)}:}{ This function runs the
      trigger rules, sending }
    \item{\code{runResetRules(state, event)}:}{ This function runs the
      reset rules. }

    \item{\code{handleEvent(event)}:}{ This function performs the
      following steps:
      \enumerate{
        \item{Fetches current state for student from database.}
        \item{Runs Status Rules.}
        \item{Runs Observable Rules.}
        \item{Runs Context Rules; note that oldContext might not equal
          context if the context has changed.}
        \item{Runs Trigger Rules, seding messages.}
        \item{If state@oldContext is not equal to context(state), run
          reset rules and then set oldContex to current context.}
        \item{Save Status.}
      } Note that if any of the steps return an error, the process is
      aborted and the updated state is not saved.}
    \item{\code{testRules(event)}:}{ Runs a test of the rules.}
  }
}
