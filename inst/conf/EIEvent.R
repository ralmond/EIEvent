library(R.utils)
library(EIEvent)

if (interactive()) {
  app <- "ecd://epls.coe.fsu.edu/P4test"
  loglevel <- "DEBUG"
  cleanFirst <- TRUE
  eventFile <- "/home/ralmond/Projects/EvidenceID/tester2.json"
} else {
  app <- cmdArg("app",NULL)
  if (is.null(app) || !grepl("^ecd://",app))
    stop("No app specified, use '--args app=ecd://...'")
  loglevel <- cmdArg("level","INFO")
  cleanFirst <- as.logical(cmdArg("clean",FALSE))
  eventFile <- cmdArg("events",NULL)
}

source("/usr/local/share/Proc4/EIini.R")

flog.appender(appender.tee(logfile))
flog.threshold(loglevel)

listeners <- lapply(names(EI.listenerSpecs),
                    function (ll) do.call(ll,EI.listenerSpecs[[ll]]))
names(listeners) <- names(EI.listenerSpecs)
eng <- do.call(EIEngine,c(EIeng.params,list(listeners=listeners),
                          EIeng.common))

if (cleanFirst) {
  eng$eventdb()$remove(buildJQuery(app=app(eng)))
  eng$userRecords$clearAll(FALSE)   #Don't clear default
  eng$listenerSet$messdb()$remove(buildJQuery(app=app(eng)))
  for (lis in eng$listenerSet$listeners) {
    if (is(lis,"UpdateListener") || is(lis,"InjectionListener"))
      lis$messdb()$remove(buildJQuery(app=app(eng)))
  }

}
if (!is.null(eventFile)) {
  system2("mongoimport",
          sprintf('-d %s -c Events --jsonArray', eng$dbname),
          stdin=eventFile)
  NN <- eng$evidenceSets()$count(buildJQuery(app=app(eng),processed=FALSE))
}


if (interactive() && !is.null(eventFile)) {
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



