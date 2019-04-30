library(R.utils)
library(EIEvent)

app <- cmdArg("app",NULL)
if (is.null(app) || !grepl("^ecd://",app))
  stop("No app specified, use '--args app=ecd://...'")
loglevel <- cmdArg("level","INFO")

source("/usr/local/share/Proc4/EIini.R")

flog.appender(appender.tee(logfile))
flog.threshold(loglevel)

listeners <- sapply(names(EI.listenerSpecs),
                    function (ll) do.call(ll,EI.listenerSpecs[[ll]]))

eng <- do.call(EIEngine,c(EIeng.params,list(listeners=listeners),
                          EIeng.common))

## Activate engine (if not already activated.)
eng$P4db()$update(buildJQuery(app=app(eng)),'{"$set":{"active":true}}')
mainLoop(eng)



