library(R.utils)
library(EIEvent)
library(futile.logger)

if (interactive()) {
  ## Edit these for the local application
  appStem <- "P4test"
  loglevel <- ""
  noprep <- FALSE
  override <- FALSE
} else {
  appStem <- cmdArg("app",NULL)
  if (is.null(app) || !grepl("^ecd://",app))
    stop("No app specified, use '--args app=ecd://...'")
  loglevel <- cmdArg("level","")
  noprep <- as.logical(cmdArg("noprep",FALSE))
  override <- as.logical(cmdArg("override",FALSE))
}

source("/usr/local/share/Proc4/EIini.R")

## Example in PP-EI project
EI.config <- jsonlite::fromJSON(file.path(config.dir,"config.json"),FALSE)


app <- as.character(Proc4.config$apps[appStem])
if (length(app)==0L || any(app=="NULL")) {
  stop("Could not find app for ",appStem)
}
if (!(appStem %in% EI.config$appStem)) {
  stop("Configuration not set for app ",appStem)
}


logfile <- (file.path(logpath, sub("<app>",appStem,EI.config$logname)))
## Let command line override configuration.
if (nchar(loglevel)==OL) loglevel <- EI.config$logLevel

if (interactive()) {
  flog.appender(appender.tee(logfile))
} else {
  flog.appender(appender.file(logfile))
}
flog.threshold(loglevel)

## Load extensions.
for (ext in EI.config$extensions) {
  if (is.character(ext) && nchar(ext) > 0L) {
    if (file.exists(file.path(config.dir,ext))) {
      source(file.path(config.dir,ext))
    } else {
      flog.error("Can't find extension file %s.", ext)
    }
  }
}

eng <- doRunrun(app,EI.config,EIeng.local,config.dir,outdir,
                logfile=logfile, override=override, noprep=noprep)

