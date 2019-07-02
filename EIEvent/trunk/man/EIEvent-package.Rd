\name{EIEvent-package}
\alias{EIEvent-package}
\alias{EIEvent}
\docType{package}
\title{
\packageTitle{EIEvent}
}
\description{
\packageDescription{EIEvent}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{EIEvent}
\packageIndices{EIEvent}

The package runs a \code{EIEngine} which is a server process for
processing events according the the \code{EI-Event} rules stored in a
database.  

}
\author{
\packageAuthor{EIEvent}

Maintainer: \packageMaintainer{EIEvent}
}
\references{

  The document \dQuote{Rules Of Evidence} gives extensive documentation
  for the rule system.
  \url{https://pluto.coe.fsu.edu/Proc4/RulesOfEvidence.pdf}. 

  There is a \dQuote{quick start} document which describes how to set up the
  engine.  This is available at 
  \url{https://pluto.coe.fsu.edu/Proc4/EIQuickStart.pdf}. 

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
\keyword{ package }
\seealso{

  The package \code{\link[Proc4:Proc4-package]{Proc4}} provides low
  level support for the database connectivity, which is handled through
  the \code{\link[mongolite]{mongo}} function.  Logging is supplied by
  the \code{\link[futile.logger]{flog.logger}} system.
 
}
\section{Configuration}{

  The \dQuote{quick start} document
  \url{https://pluto.coe.fsu.edu/Proc4/EIQuickStart.pdf} describes most
  of the configuration steps.  The directory
  \code{file.path(help(library="EIEvent")$Path,"conf")} contains a
  number of files to assist with configuration.

  The process assumes that Mongo (\url{https://www.mongodb.com/}) is
  installed on the system (or that it is available via a network).
  The files \code{setupMongo.js} and \code{setupUsers.js} (in the
  \code{conf} directory) are to be run in the mongo shell.  In
  particular, the \code{setupMongo.js} sets up the indexes.  Running
  these scripts is option, but recommended.  See also the configuration
  instructions in the \code{\link[Proc4:Proc4-package]{Proc4}} package.

  It is also strongly recommended to set up a directory for
  configuration scripts for the packages in the Proc4 family.  The
  recommended location on Unix systems is
  \code{/usr/local/share/Proc4}.  The files \code{EIEvent},
  \code{EIEvent.R}, \code{EIini.R}, and \code{EILoader.R} should be
  copied to that directory (or to a \code{bin} subdirectory), and edited
  for local prferences.  In particular, the \code{EIini.R} file needs to
  be updated with local details about the database and passwords, as
  well as directories for configuration files.  The other files need to
  be updated to point to the \code{EIini.R} file.

  The \code{EILoader.R} script loads a scoring model into
  the engine (see the following section).  The \code{EIEvent.R} script
  runs the scoring engine (see Launching and termination, below).  The
  shell script \code{EIEvent} runs the \code{EIEvent.R} script as a
  server process.
  
}
\section{Rule Sets and Context Sets}{

  In many respects, the \code{\linkS4class{EIEngine}} is an interpreter
  for a rule-based programming langugage.  The document \emph{Rules of
  Evidence} (\url{https://pluto.coe.fsu.edu/Proc4/RulesOfEvidence.pdf})
  describes that language specification.  Programs are stored in the
  database.  So there are two steps:  loading the rules and contexts
  (described here), and executing the program on the event queue
  (described below).  

  The engine processes a queue of \code{\linkS4class{Event}} objects
  (see the next section) using the following steps:
  \enumerate{
    \item{When the first event for a user arrives, that user is defined
      an initial \code{\linkS4class{Status}} based on the default status
      for that application.}
    \item{The \code{\link{applicableContexts}} for the current status is
      determined by consulting the \code{\linkS4class{ContextSet}}
      associated with the application.  The idea is that a specific
      context (e.g., a game level) belongs to several larger context
      sets (e.g., the set of levels with a comon solution strategy).}
    \item{The \code{\linkS4class{RuleTable}} is consulted to find the set
      of rules applicable to the current event and context.}
    \item{The \code{\link{Conditions}} of the rules are run.  If the
      condition is true, the the \code{\link{Predicates}} of the rules
      are run as well.}
    \item{If running the rules updates the \code{\linkS4class{Status}},
      then the updated status is saved.}
    \item{If a trigger rule is run and
      \code{\link[Proc4]{P4Message}} is built (\code{\link{!send}}),
      then the message is sent to the \code{\link[Proc4]{ListenerSet}}.  }
    \item{The event is marked as processed, and then loop goes on to
      process the next event.}
  }
    
  The program consists of three parts:
  \describe{
    \item{\code{\linkS4class{RuleTable}}}{A set of
      \code{\linkS4class{Rule}} objects which are all stored in the
      database.}
    \item{\code{\linkS4class{ContextSet}}}{A set of context descriptions
      that play a role in determining when rules are applicable.}
    \item{A defaul \code{\linkS4class{Status}}}{The initial status for
      the application.}
  }
  
  The script \code{\link{EILoader.R}} illustrates the necessary steps.

  Assuming the \code{\linkS4class{EIEngine}} is in an object called
  \code{eng} the following steps load the details.

  \enumerate{
    \item{Load in the rules from a JSON file:  \code{ruleList <- lapply(
        fromJSON(} \emph{filename} \code{, FALSE), \link{parseRule})}.  The
      function \code{\link{parseRule}} creates a rule object from the
      output of \code{\link[jsonlite]{fromJSON}}.}
    \item{Clear the old rules using \code{eng$clearAllRules()}.}
    \item{Load the new rules using \code{eng$loadRules(ruleList)}.}
    \item{Load the context set from a datafile:  \code{conMat
        <- read.csv(}\emph{filename}\code{)}.}
    \item{Create the initial context:
      \code{initCon <- data.frame(CID="*INITIAL*", Name="*INITIAL*",
        Number=0)}.}
    \item{Clear Old contexts:  \code{eng$clearContexts()}.}
    \item{Load context mattrix:  \code{eng$addContexts(conMat)}.}
    \item{Load initial context:  \code{eng$addContexts(initCon)}.}
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
\section{Events and the Event Queue}{

  Objects of class \code{\linkS4class{Event}} are stored in a database
  collection (by default, the \code{Events} collection in the
  \code{EIRecords} database).  Each event has a
  \code{\link[Proc4]{processed}} flag and the
  \code{\link[Proc4]{timestamp}} ensure that at any time the 
  system can find the oldest unprocessed event.  When run in server
  mode, new events can be added to the collection and the
  \code{\link{EIEngine}} will process them as they are added.

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

  Events can be added to the system using the \code{mongoimport}
  external function.  To call this from R, use
  \code{system2("mongoimport",
          sprintf('-d \%s -c Events --jsonArray', eng$dbname),
          stdin=eventFile)}


}
\section{Listeners and Messages}{

  The output of the EI process is through the
  \code{\link[Proc4]{Listener}} objects.  Trigger rules generate objects
  of \code{\link[Proc4]{P4Message}} (see \code{\link{!send}}) which are
  sent to the \code{\link[Proc4]{ListenerSet}} associated with the
  \code{\linkS4class{EIEngine}}.  When a trigger rule fires, the
  \code{\link{!send}} predicate creates a message which is then sent to
  the ListenerSet though the \code{\link[Proc4]{notifyListeners}}
  method.  This, in turn, calls the \code{\link[Proc4]{receiveMessage}}
  method on each of the listeners.

  One of the most common actions in response to a listener firing is to
  insert or update a record in another database.  This is the mechanism
  by which the EI process can send its output into the input queue for
  the EA process.

}
\section{Launching and termination}{

  Because the program is already stored in the database, launching the
  \code{\linkS4class{EIEngine}} as a server process takes three steps:
  \enumerate{
    \item{Create an instance of \code{\linkS4class{EIEngine}}
      (\code{eng}).} 
    \item{Create set the \code{active} flag for the process to true by
      calling, \code{eng$activate()}.}
    \item{Run the \code{\link{mainLoop}} process.  This will run until
      the \code{active} flag is cleared.}
  }

  The script \code{EIEvent.R} runs through these steps.  It calls the
  \code{EIini.R} script to get details of configuration, then creates
  the engine and runs the main loop.  The bash shell script
  \code{EIEvent} runs the \code{EIEvent.R} script in a server process
  (\code{R --slave}).

  When run from the command line, \code{EIEvent} takes four arguments.
  (These should be specified as \code{name=value}).  They are as
  follows:
  \describe{
    \item{\code{app} (Required)}{This is the name of the application to
      be run.  It is usually a URL-like string such as:
      \code{ecd://epls.coe.fsu.edu/P4test}}.
    \item{\code{level} (Default \code{INFO})}{This is the default
      logging level (see Logging below).}
    \item{\code{clean} (default \code{FALSE})}{If true, then old
      statuses and messages will be cleaned out before starting.
      Otherwise, the current run will be a continuation of the old run.}
    \item{\code{evidence} (Optional, filename)}{If this argument is
      supplied, then the contents of the file will be read into the
      event queue before processing.  The value of \code{eng$processN}
      will be set to the number of events, so the function will stop
      after the events are processed.}
  }

  The arguments are only relevant when the process is run as a server.
  The \code{EIEvent.R} script can also be run line-by-line in an R
  developement environment (e.g., R Studio), in which case the arguments
  are replaced by constants set at the beginning of the script.

  Unless an event file is given, the script will run an infinite loop.
  On a *nix system, this usually means that it should run as a
  background process, e.g., the call should be \code{nohup EIEvent args
  &}.  This requires a graceful way to close the process down.  This is
  done by setting the \code{active} field for the record corresponding
  to the application to false in the \code{AuthorizedApps} collection of
  the \code{Proc4} database.  A mongo shell call to do this is:
  \code{db.AuthorizedApps.update(\{app:\{"$regex":"appname"\}\},
    \{"$set":\{active:false\}\});}, where \dQuote{appname} can be any
  string that uniquely identifies the application.

  If the event file is given, it should be a JSON file will a collection
  of \code{\linkS4class{Event}} objects.  The events will be loaded into
  the database and processed.  The script will stop when the events are
  done processing.  Often it is more convenient to run this in the
  interactive mode as R will still be active after the completion, so
  that the results can be inspected.

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
\section{Concurrency}{

  Note that several applications can share the same database.  The
  \code{\link[Proc4]{app}} field of the \code{\linkS4class{EIEngine}} is
  part of the key for all of the collections.  Generally, database
  commands will use the \code{app} field as part of the queries, so two
  engines with different applications ids will have different rule sets,
  context sets, status sets and event queues.

  Because of this separation, it is possible to run multiple
  \code{\link{EIEngine}} processes focused on different applications.
  This gives a limited form of parallel processing.

  In theory, processing for each \code{\link{uid}} could be handled
  separately.  This has not been coded in this version.  Future versions
  may use a language other than R which supports richer tools for
  multi-threaded computing.


}
\examples{

\dontrun{

## Initialize the Engine
app <- "ecd://epls.coe.fsu.edu/P4test"
loglevel <- "DEBUG"
cleanFirst <- TRUE
eventFile <- "/home/ralmond/Projects/EvidenceID/c081c3.core.json"

## These configuration steps are generally done in EIini.R
## These are application generic parameters
EIeng.common <- list(host="localhost",username="EI",password="secret",
                     dbname="EIRecords",P4dbname="Proc4",waittime=.25)

appstem <- basename(app)
## These are for application specific parameters
EIeng.params <- list(app=app)

## File for loading configuraitons
config.dir <- "/home/ralmond/ownCloud/Projects/NSFCyberlearning/EvidenceID"

## Location of logfile
logfile <- file.path("/usr/local/share/Proc4/logs",
                     paste("EI_",appstem,"0.log",sep=""))

EI.listenerSpecs <-
  list("InjectionListener"=list(sender=paste("EI",appstem,sep="_"),
            dbname="EARecords",dburi="mongodb://localhost",
            colname="EvidenceSets",messSet="New Observables"))

## Setup logging
flog.appender(appender.file(logfile))
flog.threshold(loglevel)

## Setup Listeners
listeners <- lapply(names(EI.listenerSpecs),
                    function (ll) do.call(ll,EI.listenerSpecs[[ll]]))
names(listeners) <- names(EI.listenerSpecs)
if (interactive()) {
  cl <- new("CaptureListener")
  listeners <- c(listeners,cl=cl)
}

## Load the Rules
ruleList <- lapply(fromJSON("rulefile.json", FALSE), parseRule)
eng$clearAllRules()
eng$loadRules(ruleList)

## Load Contexts
conMat <- read.csv("contextTable.csv")
initCon <- data.frame(CID="*INITIAL*", Name="*INITIAL*", Number=0)
eng$clearContexts()
eng$addContexts(conMat)
eng$addContexts(initCon)

## Setup default status.
eng$clearStatusRecords(TRUE) ## Clears default record
defaultRec <- eng$newUser("*DEFAULT*")
obs(defaultRec,"bankBalance") <- 0
defaultRec <- eng$saveStatus(defaultRec)

## Clean out old records from the database.
if (cleanFirst) {
  eng$eventdb()$remove(buildJQuery(app=app(eng)))
  eng$userRecords$clearAll(FALSE)   #Don't clear default
  eng$listenerSet$messdb()$remove(buildJQuery(app=app(eng)))
  for (lis in eng$listenerSet$listeners) {
    if (is(lis,"UpdateListener") || is(lis,"InjectionListener"))
      lis$messdb()$remove(buildJQuery(app=app(eng)))
  }
}
## Process Event file if supplied
if (!is.null(eventFile)) {
  system2("mongoimport",
          sprintf('-d \%s -c Events --jsonArray', eng$dbname),
          stdin=eventFile)
  ## Count the number of unprocessed events
  NN <- eng$eventdb()$count(buildJQuery(app=app(eng),processed=FALSE))
  ## This can be set to a different number to process only a subset of events.
  eng$processN <- NN
}

## Activate engine (if not already activated.)
eng$activate()
mainLoop(eng) ## This will not terminate unless processN was set to a
              ## finite value.

## This shows the details of the last message.  If the test script is
## set up properly, this should be the observables.
if (!is.null(eventFile) && TRUE) {
  details(cl$lastMessage())
}

}
}
\section{Acknowledgements}{

  Work on the Proc4, EIEvent and EABN packages has been supported by the
  National Science foundation grants \emph{DIP: Game-based Assessment and
  Support of STEM-related Competencies} (#1628937, Val Shute, PI) and
  \emph{Mathematical Learning via Architectual Design and Modeling Using
  E-Rebuild.} (#1720533, Fengfeng Ke, PI).

  The EIEvent package developement was led by Russell Almond (Co-PI).
}