library(R.utils)
library(EIEvent)

app <- cmdArg("app",NULL)
if (is.null(app) || !grepl("^ecd://",app))
  stop("No app specified, use '--args app=ecd://...'")

appstem <- basename(app)
logfile <- cmdArg("log",file.path("/usr/local/share/Proc4/log",
                                  paste(appstem,"log",sep=".")))

waittime <- as.numeric(cmdArg("waittime",.25))

loglevel <- cmdArg("level","DEBUG")

cat("App: ",app,"\n")
cat("Logfile: ",logfile,"\n")
cat("Waiting time: ",waittime, " Logging Level: ", loglevel,"\n")

flog.appender(appender.tee(logfile))
flog.threshold(loglevel)

cl <- new("CaptureListener")

eng <- EIEngine(app=app,listeners=list(capture=cl),waittime=waittime)
## Activate engine (if not already activated.)
eng$P4db()$update(buildJQuery(app=app(eng)),'{"$set":{"active":true}}')
mainLoop(eng)



