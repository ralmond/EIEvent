\name{mainLoop}
\alias{mainLoop}
\title{Runs the \code{EIEngine} as a server.}
\description{

  The \code{mainLoop} is used when the \code{\linkS4class{EIEngine}} is
  used as a server.  It checks the database, for unprocessed
  \code{\linkS4class{Event}} objects, and calls
  \code{\link{handleEvent}} on them in the order of their
  \code{\link[Proc4]{timestamp}}s.  As a server, this is potentially an
  infinite loop, see details for ways of gracefully terminating the loop.

}
\usage{
mainLoop(eng)
}
\arguments{
  \item{eng}{An instance of \code{\linkS4class{EIEngine}} which will be
    used as a server.}
}
\details{

  The \code{\linkS4class{EIEngine}} class uses the Events collection in
  the database (\code{eng$eventdb()}) as a queue.  All events have a
  \code{\link[Proc4]{processed}} field which is set to true when the
  event is processed.  Thus the main loop iterates over the following
  three statements:
  \enumerate{
    \item{Fetch the oldest unprocessed Event: \code{eve <-
        eng$fetchNextEvent()}.}
    \item{Process the event: \code{out <- handleEvent(eng,eve)}.  (Note:
      this expression will always return.  If it generates an error, the 
      error will be logged and an object of class \code{try-error} will be
      returned.)} 
    \item{Mark the event as processed: \code{eng$setProcessed(eve)}.}
  }

  At its simplest level, the funciton produces an infinite loop over
  these three statements, with some additional steps related to
  logging and control.

  Before processsing the next field, the mainLoop checks the
  \code{eng$shouldHalt()} method to see if a halt signal has been sent
  (by setting the \code{EIsignal} field in the \code{AuthorizedApps}
  collection to \dQuote{halt}).

  If the event queue is empty, the process sleeps for a time
  given by \code{eng$waittime} and then checks the queue again.  At the
  same time, it checks status of the active flag for the process using
  the \code{eng$stopWhenFinished()} call.  By default this checks the
  \code{EIsignal} field of the record corresponding to \code{app(eng)} in
  the collection \code{AuthorizedApps} in the database \code{Proc4}.
  Setting that field to \dQuote{finish} manually will result in the
  \code{mainLoop} terminating when the queue is empty.  As R is running
  in server mode when this happens, this often needs to be done using an
  external process.  The following command issues from the Mongo shell
  will shut down the server for an application containing the string
  "appName" as part of its name.

  \code{db.AuthorizedApps.update(\{app:\{$regex:"appName"\}\},
    \{$set:\{EIsignal:"finish"\}\});}

  To facilitate testing, the field \code{eng$processN} can be set to a
  finite value.  This number is decremented at every cycle, and when
  it reaches 0, the \code{mainLoop} is terminated, whether or not
  their are any remaining events to be processed.  Setting
  \code{eng$processN} to an infinite value, will result in an infinite
  loop that can only be stopped by using the active flag (or
  interrupting the process).

}
\value{

  There is no return value.  The function is used entirely for its side
  effects. 

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
\seealso{

  The class \code{\linkS4class{EIEngine}} describes the setup of the
  engine, and the function \code{\link{handleEvent}} describes the
  processing that occurs for each event.
  
}
\examples{

\dontrun{
## From EIEvent.R script
app <- "ecd://epls.coe.fsu.edu/P4test"
loglevel <- "DEBUG"
cleanFirst <- TRUE
eventFile <- "/home/ralmond/Projects/EvidenceID/c081c3.core.json"

## Initialization details (from EIini.R script)
EIeng.common <- list(host="localhost",username="EI",password="secret",
                     dbname="EIRecords",P4dbname="Proc4",waittime=.25)
appstem <- basename(app)
EIeng.params <- list(app=app)
logfile <- file.path("/usr/local/share/Proc4/logs",
                     paste("EI_",appstem,"0.log",sep=""))
EI.listenerSpecs <-
  list("InjectionListener"=list(sender=paste("EI",appstem,sep="_"),
            dbname="EARecords",dburi="mongodb://localhost",
            colname="EvidenceSets",messSet="New Observables"))

## Setup logging
if (interactive()) {
  flog.appender(appender.tee(logfile))
} else {
  flog.appender(appender.file(logfile))
}
flog.threshold(loglevel)
## Setup Listeners
listeners <- lapply(names(EI.listenerSpecs),
                    function (ll) do.call(ll,EI.listenerSpecs[[ll]]))
names(listeners) <- names(EI.listenerSpecs)
if (interactive()) {
  cl <- new("CaptureListener")
  listeners <- c(listeners,cl=cl)
}
EIeng.params <-
  c(EI.config$EIEngine,
    EIeng.local[setdiff(names(EIeng.local),names(EI.config$EIEngine))])

EIeng.params$listenerSet <-
    ListenerSet(sender= sub("<app>",sapp,EI.config$sender),
                dbname=EIeng.local$dbname, dburi=EIeng.local$dburi,
                listeners=listeners,
                colname=EI.config$lscolname)
EIeng.params$app <- app

## Make the EIEngine
eng <- do.call(EIEngine,EIeng.params)

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
          sprintf('-d %s -c Events --jsonArray', eng$dbname),
          stdin=eventFile)
  ## Count the number of unprocessed events
  NN <- eng$eventdb()$count(buildJQuery(app=app(eng),processed=FALSE))
}
if (!is.null(eventFile)) {
  ## This can be set to a different number to process only a subset of events.
  eng$processN <- NN
}
## Activate engine (if not already activated.)
eng$activate()
mainLoop(eng)

## Depending on value of NN, this may not terminate!
}
}
\keyword{ database }
\keyword{ manip }
