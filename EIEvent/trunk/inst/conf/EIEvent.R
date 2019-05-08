library(R.utils)
library(EIEvent)

if (interactive()) {
  app <- "ecd://epls.coe.fsu.edu/P4test"
  loglevel <- "DEBUG"
} else {
  app <- cmdArg("app",NULL)
  if (is.null(app) || !grepl("^ecd://",app))
    stop("No app specified, use '--args app=ecd://...'")
  loglevel <- cmdArg("level","INFO")
}

source("/usr/local/share/Proc4/EIini.R")

flog.appender(appender.tee(logfile))
flog.threshold(loglevel)

listeners <- lapply(names(EI.listenerSpecs),
                    function (ll) do.call(ll,EI.listenerSpecs[[ll]]))
names(listeners) <- names(EI.listenerSpecs)
eng <- do.call(EIEngine,c(EIeng.params,list(listeners=listeners),
                          EIeng.common))

## Activate engine (if not already activated.)
eng$activate()
mainLoop(eng)



