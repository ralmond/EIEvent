\name{EIEngine-class}
\Rdversion{1.1}
\docType{class}
\alias{EIEngine-class}
\alias{notifyListeners,EIEngine-method}
\alias{app,EIEngine-method}

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
    \item{app}{\code{signature(sender = "EIEngine")}: Returns a string
      giving the application this Engine is supporting.}
  }
}
\section{Database Connections}{

  The \code{EIEngine} is an interprer for a rule-based langauge for
  processing events.  The code, the rules, are stored in a
  \code{\link[mongolite]{mongo}} database, as are the events (the input
  queue) and the status objects.  This database is determined by the
  \code{dburi} (note that this is determined with the \code{username},
  \code{password}, \code{host} and \code{port} of the
  \code{$initialize()} method or \code{\link{EIEngine}} function) and
  \code{dbname}.  Note that this database can be shared by several
  applications.  The \code{app} field is used to identify records
  belonging to this application.

  In the default configuration, the database is called
  \dQuote{EIRecords} and has the following collections:
  \describe{
    \item{\emph{Events}}{This contains the \code{\linkS4class{Event}s}
      to be processed.  It can be accessed through the function
      \code{$eventdb()}.  Note that each event has a
      \code{\link[Proc4]{processed}} field that makes this collection
      essentially a queue.  Other processes can queue events to 
      be processed by inserting event records into this collection.}
    \item{\emph{Messages}:}{This is used by the
      \code{\link[Proc4]{ListenerSet}} to record messages sent by
      trigger rules.  This collection can be accessed throught the
  \code{$listenerSet} field.}
    \item{\emph{Rules}:}{This contains the collection of
      \code{\linkS4class{Rule}}s.  This collection can be accessed
      through the \code{$ruleTable} field.}
    \item{\emph{States}:}{This contains the collection of
      \code{\linkS4class{Status}} objects.  This collection can be
      accessed through the \code{$userRecords} field.}
    \item{\emph{Context}:}{This contains the collection of
      \code{\linkS4class{Context}} objects.  This can be accessed
      through the \code{$contexts} field.}
    \item{\emph{Tests}:}{This contains the collection of
      \code{\linkS4class{RuleTest}} objects.  This can be accessed
      through the \code{$ruleTests} field.}
  }

  The \code{AuthorizedApps} collection in the \code{Proc4}
  database is used to signal when the EI process should shut down.  The
  initialization argument \code{admindbname} is used to set the name of
  this database, and the \code{admindb()} function to access the
  collection.  Note that all database collections should be accessed
  through the accessor function, which initialize the connection on
  demand, rather than by accessing the field directly.

  The class-based methods, \code{eng$activate()} and
  \code{eng$deactive()} turn on an off the flag in the database that
  indicates that this app is active.  The methods
  \code{eng$isActivated()} tests the status of that flag.  The methods
  \code{eng$shouldHalt()} and \code{eng$stopWhenFinished()} check for
  signals that the main loop should be terminated immediately or when
  finished. 
  
  The listener objects may also access databases.  This is the usual
  mechanism for the EI process to insert messages into the processing
  queue of other processes.  
  
}
\section{Activation and Termination}{

  The \code{\link{mainLoop}} searches the event database for unprocessed
  events, and processes them in chronological order.  There are three
  termination conditions.  First, if the \code{$processN} field (which
  is decrimented after each event is processed) reaches 0, then the main
  loop will exit.  Second, after each event is processed, it checks the
  \code{eng$shouldHalt()} condition; if this is true, it halts
  immediately.  Third, if the event queue is empty, the engine will
  check the \code{eng$stopWhenFinished()} flag and will stop if it is
  true.  Otherwise, it will wait for the \code{waittime} (in seconds)
  and check again.

  The method \code{eng$activate()} sets the activated flag for this app to
  true and \code{eng$deactivate()} clears it.  These are called from 
  \code{\link{mainLoop}}.  External processes can check this flag in the
  database by looking at the value of
  \code{db.AuthorizedApps.find(\{"app":\{"$regex":"P4test"\}\},
    \{"appStem":1,"EIactive":1\});}. The code is Mongo shell javascript,
  and \dQuote{P4test} should be
  replaced with any string uniquely identifying the app (i.e.,
  basename(app)). 

  The to signal to the process to stop, an external program needs to
  set the value of the \code{EIsignal} field for the appropriate
  application.  The javascript code 
  \code{db.AuthorizedApps.update(\{app:\{"$regex":"P4test"\}\},
    \{"$set":\{"EIsignal":<value>\}\});}, where \dQuote{P4test} should be
  replaced with any string uniquely identifying the app, run in the
  Mongo shell will work.  The \code{<value>} should be either
  \dQuote{finish} or \dQuote{halt}.  The former triggers the
  \code{eng$shopWhenFinished()} and the latter triggers
  \code{eng$halt()}.

}
\section{Events and the Event Queue}{

  Objects of class \code{\linkS4class{Event}} are stored in a database
  collection \code{$eventdb()}.  The \code{\link[Proc4]{processed}} flag
  and the \code{\link[Proc4]{timestamp}} ensure that at any time the
  system can find the oldest unprocessed event.  The function
  \code{$fetchNextEvent()} fetches this event.  When the event is
  processed, the function \code{$setProcessed()} can be used to mark it
  as processed an update the database. The function \code{$setError()}
  is used to add an error message to an event.

  A single cycle of the \code{\link{mainLoop}} consists of the following
  steps.
  \enumerate{
    \item{Fetch the next event with \code{eve <- eng$fetchNextEvent()}.}
    \item{Process the event with \code{out <- handleEvent(eng,eve)}.}
    \item{If an error was generated (out is of class \code{try-error}),
      then save the error in the database using \code{eng$setError(eve,
        out)}.}
    \item{Mark the event as processed using
      \code{eng$setProcessed(eve)}.}
  }
  Note that \code{$setProcessed()} still needs to be called even if
  \code{$setError()} is called.

}
\section{Rules}{

  Each application has a collection of rules or a
  \code{\linkS4class{RuleTable}}.  This rule table is stored in the
  \code{$rules} field of the \code{EIEngine} object.  Recall that a rule
  is applicable to a given event if the \code{\link{verb}} and
  \code{\link{object}} of the event match the verb and object of rule
  (or the rule has the special object \dQuote{ALL}) and the
  \code{\link{context}} of the rule is one of the
  \code{\link{applicableContexts}} of the current context of the state.

  In the current implementation, the rules are stored in the
  \code{Rules} database.  This solves two problems:  rule storage and
  rule search.

  First, the database is where the rules come from when the application
  starts.  The method \code{$loadRules()} stores a list of rules in the
  database and \code{$clearAllRules()} removes all rules.  Note that
  trying to add a second rule with the same name will produce an error,
  unless the second argument (\code{stopOnDups}) is set to \code{FALSE}.
  So generally rules need to be cleared before being reloaded.
  (For finer replacement see the \code{\linkS4class{RuleTable}} object.)  
  Typically, loading the rules is done in three steps:
  \enumerate{
    \item{Load in the rules from a JSON file:  \code{ruleList <- lapply(
        fromJSON(} \emph{filename} \code{, FALSE), \link{parseRule})}.  The
      function \code{\link{parseRule}} creates a rule object from the
      output of \code{\link[jsonlite]{fromJSON}}.}
    \item{Clear the old rules using \code{eng$clearAllRules()}.}
    \item{Load the new rules using \code{eng$loadRules(ruleList)}.}
  }
    
  Second, the search for applicable rules can use standard database
  queries.  The function \code{$findRules()} searches the database for
  rules matching the supplied details.  Note that the \code{EIEngine}
  version of this function differs from the
  \code{\linkS4class{RuleTable}} version in that it accepts a string
  as input to for the \code{context} field and finds a context object
  to match, while the \code{RuleTable} version expects an object of
  class \code{\linkS4class{Context}}.  The \code{phase} argument is
  optional.  The first version of the main loop, used the database to
  separate out rules of different types.  However, this was rather
  inefficient when there were not applicable methods for a rule (the
  most common case).  The current version now searches for rules that
  would be applicable in any phase of processing and then separates
  the resulting rule lists.

  The \code{$findRules()} and rule processing functions do a fair
  amount of logging, so they can be debugged by setting
  \code{\link[futile.logger]{flog.threshold}} to a lower value.  At
  the \code{DEBUG} level, the number of rules returned for each event
  is logged.  At the \code{TRACE} level, the queries used as well as
  the names of the returned rules are logged.

}
\section{Context Sets}{

  Rules must be applicable to both the current event and in the current
  \code{\linkS4class{Status}}.  The \code{\link{verb}} and
  \code{\link{object}} fields of the rule can either be a constant,
  which is matched as an exact string, or the special constant
  \dQuote{ALL} which indicates that the rule is applicable to all verbs
  or objects.

  For \code{\linkS4class{Context}}s, a second intermediate level is
  introduced:  the \emph{context set}.  The context object defines which
  context sets it belongs to through the \code{\link{belongsTo}} field.
  Context objects are stored in the database and are matched to the
  \code{context} field of events using the \code{name} field. (The
  context field of events is trimmed on input to prevent issues with
  mismatches caused by trailing spaces, see
  \code{\link[Proc4]{parseMessage}}.)

  The field \code{$contexts} is a link to a
  \code{\linkS4class{ContextSet}} class which contains all of the
  contexts.  The function \code{$getContext()} which takes the name of
  the context as an argument, calls \code{\link{matchContext}} to find
  the context object, and returns the context object.  Note that if the
  string is not matched, it will behave like a context which only
  belongs to the universal context set \dQuote{ALL}.

  Contexts are loaded by parsing a context matrix, a
  \code{\link[base]{data.frame}} with the
  following columns: \code{cid} (character, required),
  \code{number} (numeric, required),  \code{name} (character, optional)
  and \code{doc} (character, required).  Additional columns should
  represent context sets, and should be 1 if the context row is in the
  set column and 0 otherwise.  See \code{\link{loadContexts}} for an
  example.  A special context with number 0 and cid \dQuote{*INITIAL*}
  is required to represent the initial state when the player first logs
  into the system.  The method \code{$addContexts()} adds the contexts
  in the matrix to the system.  The method \code{$clearContexts()}
  removes existing contexts.

  A typical initialization has the following steps:
  \enumerate{
    \item{Load the context set from a datafile:  \code{conMat
        <- read.csv(}\emph{filename}\code{)}.}
    \item{Create the initial context:
      \code{initCon <- data.frame(CID="*INITIAL*", Name="*INITIAL*",
        Number=0)}.}
    \item{Clear Old contexts:  \code{eng$clearContexts()}.}
    \item{Load context mattrix:  \code{eng$addContexts(conMat)}.}
    \item{Load initial context:  \code{eng$addContexts(initCon)}.}
  }

}
\section{Status and Default Status}{

  The \code{$userReocrds} field of the engine points to a
  \code{\linkS4class{UserRecordSet}} collection.  This is a set of
  \code{\linkS4class{Status}} records for the students.  Once again, it
  is stored in the database to give the status persistence across
  invocations of the engine.

  The method \code{$getStatus()} fetches a status from the database for
  the specified user.  If no status is found, then the method
  \code{$newUser()} is called to create the inital status. This will look
  for a status for a user called \dQuote{*DEFAULT*}, and will copy that
  status if available.  Otherwise, a blank status (one with the
  \code{\link{timer}s}, \code{\link{flag}s} and
  \code{\link{obs}servables} fields empty.

  The method \code{$saveStatus()} saves the status back to the
  database.  Note that the return value should be captured, as this will
  update the \code{_id} (see \code{\link[Proc4]{m_id}}) field of the
  record.  The method \code{$clearStatusRecords(clearDefault)} will
  remove old status records, so that all players start fresh.  With the
  argument set to true, it will also remove the default record.

  The following steps can be used to set up the default record.
  \enumerate{
    \item{Clear old records including default:
      \code{eng$clearStatusRecords(TRUE)}.}
    \item{Create a new blank status:  \code{defaultRec <-
        eng$newUser("*DEFAULT*")}.}
    \item{Initialize fields on that record:}
    \itemize{
      \item{Flag:  \code{flag(defaultRec,} \emph{name} \code{) <-}
        \emph{value}. }
      \item{Observable:  \code{obs(defaultRec,} \emph{name} \code{) <-}
        \emph{value}. }
      \item{Timers:  \code{timer(defaultRec,} \emph{name} \code{) <- Timer(}
        \emph{name}\code{)}. }
    }
    \item{Save it back to the database: \code{defaultRec <-
        eng$saveStatus(defaultRec)}.}
  }

}
\section{Listeners and Messages}{

  The output of the EI process is through the
  \code{\link[Proc4]{Listener}} objects.  The \code{$listenerSet} field
  holds an object of class \code{\link[Proc4]{ListenerSet}} which in
  turn holds the listeners.  When a trigger rule fires, the
  \code{\link{!send}} predicate creates a message which is then sent to
  the ListenerSet though the \code{\link[Proc4]{notifyListeners}}
  method.  This, in turn, calls the \code{\link[Proc4]{receiveMessage}}
  method on each of the listeners.

  If the \code{dburi} field in the ListenerSet is set, then the
  ListenerSet maintains a connection to the \code{Messages} (or other
  named) collection in the database used by the \code{EIEngine}.  In
  this case, all messages are logged to this database.  The listeners,
  in contrast, generally only record messages whose message matches a
  given criteria.  Many of the listeners connect to a database other
  than the internal one:  They could either insert messages in the queue
  for another process or update a record in a database that tracks
  player status.

  The \code{\link[Proc4]{CaptureListener}} is particularly useful for
  testing.  It stores all messages in a list and has a specific method
  for viewing the latest one.  

}
\section{Main Event Loop}{

  The goal of the \code{EIEngine} is to act as a server process.  It
  continually scans the event queue, looking for unprocessed events.  It
  processes the oldest unprocessed event, marks it as processed and then
  looks for the next event.  If no unprocessed events are found it
  sleeps for a bit and then tries again.  The field \code{$waittime} is
  the amount of time in seconds that the main loop will wait before
  checking again.
  
  The function \code{\link{mainLoop}} implements the main processing
  loop.  Generally, it should be the last call in a script for running
  the process (see \code{EIEvent.R} in the \code{config} subdirectory of
  the package for an example).  It terminates under one of three
  conditions:  (1) When the event queue is empty, it checks the
  \code{$stopWhenFinished()} method which checks the flag in the
  \code{AuthorizedApps} collection in the \code{Proc4} database. If the
  function returns false, the main loop will terminate.  (2) The
  \code{$processN} field is decremented with every event processed.  If
  it is given a finite value (the default is infinity) then it will
  process that many records and then stop.  (3) If the
  \code{$shouldHalt()} method returns true, the process stops rather
  than go onto the next method.  (This allows for an emergency shutdown
  if an error is found in the configuration).

  Running \code{\link{mainLoop}} with \code{$processN} set to a positive
  integer is useful for testing.  In one mode a collection of events can
  be read into the database from a test file, then the \code{$processN}
  field can be set to the number of new events (see \code{EIEvent.R} for
  an example).  This will then run the test events and then stop.
  Alternatively, \code{$processN} can be set to a number lower than the
  total number of events and then the logging threshold can be set
  higher, or the processing can be run step by step to isolate
  problematic events.

}
\section{Logging and Error Handling}{

  Logging is handled using the \code{\link[futile.logger]{flog.logger}}
  framework.  This provides a large number of tools for specifying the
  details of the logging.  The \code{EIEngine} executes the rules in the
  \code{\link[Proc4]{withFlogging}} environment.  This means that the
  default behavior for rules which generate errors is to log the error
  and move to the next rule.  The error message is also added to the
  event in the event database (\code{\link[Proc4]{markAsError}}).

  The amount of logging done, particularly the amount of detail
  supplied, is controled by the
  \code{\link[futile.logger]{flog.threshold}} function.  The amount of
  detail provided at various levels is as follows:
    \describe{
    \item{TRACE}{
      \itemize{
        \item{The results of \code{checkCondition} are reported.}
        \item{The specific rules found from each query are reported.}
        \item{A message is logged as each rule is run.}
    }}
    \item{DEBUG}{
      \itemize{
        \item{When an error occurs information about the state, event
          and rule where the error occurred as well a stack trace are
          logged.}
        \item{Each event is logged as it is processed.}
        \item{Rule searches are logged and the number of rules
          reported.}
        \item{A message is logged when each phase starts.}
        \item{A message is logged when the context changes.}
    }}
    \item{INFO}{Minimal information about which events are being
  processed is logged.}
    \item{WARN and above}{If an error occurs the error is logged along
      with context information.}
  }

  When running the \code{EIEngine} as a server, generally the logging
  should be done as a file.  This can be done by running:
  \code{flog.appender(appender.file(logfile))}.  When running in
  interactive mode, it may be useful to have log messages sent to both
  the console and the log file.  The command for this is:
  \code{flog.appender(appender.file(logfile))}.

}
\section{Test Sets}{

  This is mostly a placeholder for future capability.  Along with the
  rules, which are after all a program, there should be a test suite.
  The goal is to provide a collection of tests, so that the test suite
  can be rerun when rules are modified, creating a unit testing facility
  for the \emph{EI-Event} language.

  This has not yet been implemented, but the function
  \code{\link{testRuleScript}} provides a mechanism for writing test
  scripts.  

}
\references{

  The document \dQuote{Rules Of Evidence} gives extensive documentation
  for the rule system.
  \url{https://pluto.coe.fsu.edu/Proc4/RulesOfEvidence.pdf}. 

  There is a \dQuote{quick start} document which describes how to set up the
  engine.  This is available at 
  \url{https://pluto.coe.fsu.edu/Proc4/EIQuickStart.pdf}. 
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
    \item{\code{admindbname}:}{Name of the database which contains the
      \code{AuthorizedApps} collection used to start and stop the
      engine.} 
    \item{\code{adminDB}:}{Object of class \code{\linkS4class{MongoDB}} referencing
      the \code{AuthorizedApps} collection used to start and stop the
      engine. This field should not be referenced directly, instead,
      the \code{$admindb()} should be used as it will initialize the
      connection if needed.}
    \item{\code{waittime}:}{The number of seconds to wait before
      rechecking the queue when it is empty.}
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
    \item{\code{processN}:}{An integer counting how many events the
      \code{\link{mainLoop}} should process before stopping.  If
      infinite, the main loop will not stop until the authorized app
      flag is cleared.}
  }
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{initialize(app, listenerSet, dburi, dbname, admindbname,
        waittime, processN, ...)}:}{ This method sets up
      the object.  Note that the listeners object is passed in as an
      argument, as are the database credentials.}
    \item{\code{admindb()}:}{Returns a handle to the \code{AuthorizedApps}
      collection in the Proc4 database.  Initializes if needed.}
    \item{\code{isActivated()}:}{Returns true if the active flag is set
      on this applicaiton (\code{app}) in the \code{AuthorizedApps}
      collection.} 
    \item{\code{activate()}:}{Sets the active flag to true for this
      \code{app} in the \code{AuthorizedApps} collection.}
    \item{\code{deactivate()}:}{Clears the active flag to true for this
      \code{app} in the \code{AuthorizedApps} collection.}
    \item{\code{shouldHalt()}:}{ This function checks the database to
      see whether or not the flag is set to cause the process to halt
      after processing the current record.. }
    \item{\code{stopWhenFinished()}:}{ This function checks the database to
      see whether or not the flag is set to cause the process to stop
      when the event queue is empty. }
    \item{\code{show()}:}{Provides a short string identifying the
      engine. }
    \item{\code{newUser(uid)}:}{ Generates a new student record.
      (Call to \code{\linkS4class{UserRecordSet}$newStudent()})}
    \item{\code{getStatus(uid)}:}{ Feteches the current status for a
      student from the database.  (Call to
      \code{\linkS4class{UserRecordSet}$getStatus()})} 
    \item{\code{saveStatus(state)}:}{ Saves an updated status to the
      database. (Call to \code{\linkS4class{UserRecordSet}$saveStatus()})  }
    \item{\code{clearStatusRecords(clearDefault)}:}{Clears the status
      records, including the default record if the argument is TRUE.  
      (Call to \code{\linkS4class{UserRecordSet}$clearAll()}).}
    \item{\code{getContext(id)}:}{ This is a call to
      \code{\link{matchContext}}. } 
    \item{\code{addContexts(conmat)}:}{ This is a call to
      \code{\link{loadContexts}}. }
    \item{\code{clearContexts()}:}{ This is a call to
      \code{\linkS4class{ContextSet}$clearAll()}.  }
    \item{\code{eventdb()}:}{ This returns the event database handle,
      creating it if it is not yet created.  It is recommended to use
      this method rather than access the slot directly. }
    \item{\code{setProcessed(mess)}:}{This sets the
      \code{\link[Proc4]{processed}} flag on its argument and updates
      the database.}
    \item{\code{setError(mess,e)}:}{This sets the
      \code{\link[Proc4]{processingError}} flag on \code{mess} to
      \code{e} and updates the database..}
    \item{\code{fetchNextEvent()}:}{This returns the unprocessed event
      with the oldest timestamp, or NULL if there are no unprocessed events.}
    \item{\code{findRules(verb, object, context, phase=NULL)}:}{ This
      function finds the potentially applicable for the current verb,
      object, context and phase.  If \code{phase==NULL}, the get rules
      for all phases.}
    \item{\code{loadRules(rlist, stopOnDups)}:}{ This function loads
      rules into the rule table using \code{\link{loadRulesFromList}}.}
    \item{\code{loadAndTest(script, stopOnDups)}:}{ This function loads
      rules into the rule table from test scripts using
      \code{\link{testAndLoad}}.}

  }
}
