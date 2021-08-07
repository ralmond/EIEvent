library(utils)
library(EIEvent)

source("/usr/local/share/Proc4/EIini.R")

if (interactive()) {
  ## Edit these for the local application
  loglevel <- ""
  override <- FALSE
} else {
  loglevel <- cmdArg("level","")
  override <- as.logical(cmdArg("noprep",FALSE))
}

EI.config <- fromJSON(file.path(config.dir,"config.json"),FALSE)


appStem <- as.character(EI.config$appStem)

apps <- as.character(Proc4.config$apps[appStem])
if (length(apps)==0L || any(apps=="NULL")) {
  stop("Could not find apps for ",appStem)
}

ruledir <- ifelse(!is.null(EI.config$ruledir),
                  EI.config$ruledir,"Rules")

logfile <- (file.path(logpath, sub("<app>","Loader",EI.config$logname)))
## Let command line override configuration.
if (nchar(loglevel)==OL) loglevel <- EI.config$logLevel
if (interactive()) {
  flog.appender(appender.tee(logfile))
} else {
  flog.appender(appender.file(logfile))
}
flog.threshold(EI.config$loglevel)



lockfile <- file.path(config.dir,ruledir,"ruleloader.lock")
file.create(lockfile)

## Loop over apps
tryCatch(
{
    for (app in apps)
      doLoad(app, EI.config, EIeng.local, config.dir)
},
finally=unlink(lockfile))



