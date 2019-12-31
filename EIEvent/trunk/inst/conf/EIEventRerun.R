library(R.utils)
library(EIEvent)

if (interactive()) {
  ## Edit these for the local application
  app <- "ecd://epls.coe.fsu.edu/P4test"
  loglevel <- "DEBUG"
  cleanFirst <- TRUE
} else {
  app <- cmdArg("app",NULL)
  if (is.null(app) || !grepl("^ecd://",app))
    stop("No app specified, use '--args app=ecd://...'")
  loglevel <- cmdArg("level","INFO")
  cleanFirst <- as.logical(cmdArg("clean",FALSE))
}
eventFile <- NULL

## Adjust the path here as necessary
source("/usr/local/share/Proc4/EIini.R")

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
flist <- c(uid = "character", context = "character",
           blowerManip="numeric", BouncinessRun="logical",
           agentsUsed="list",lastAgent="factor('lever','pendulum','ramp','springboard')",
           badge="ordered('none','silver','gold')", attempts="numeric",
           NumberPufferManip="numeric", massManip="numeric",
           airManip="numeric",gravityManip="numeric",levelTime="difftime",
           bankBalance="numeric")
tl <- TableListener(name=paste(ppObs,basename(app),sep="."),
                    flist,"New Observables")
listeners <- c(listeners,tl=tl)

## Make the EIEngine
eng <- do.call(EIEngine,c(EIeng.params,list(listeners=listeners),
                          EIeng.common))

## Clean out old records from the database.
if (cleanFirst) {
  eng$eventdb()$update(buildJQuery(app=app(eng)),
                       '{"$set":{"processed":false}}',
                       multiple=TRUE)
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
}
if (!is.null(eventFile)) {
  ## This can be set to a different number to process only a subset of events.
  NN <- eng$eventdb()$count(buildJQuery(app=app(eng),processed=FALSE))
  eng$processN <- NN
}


## Activate engine (if not already activated.)
eng$activate()
mainLoop(eng)

## This is for running the loop by hand.
if (interactive() && FALSE) {
  eve <- eng$fetchNextEvent()
  out <- handleEvent(eng,eve)
  eng$setProcessed(eve)
}

## This shows the details of the last message.  If the test script is
## set up properly, this should be the observables.
ppObs <- returnDF(tl)
write.csv(ppobs,paste(name(tl),"csv",sep="."))


