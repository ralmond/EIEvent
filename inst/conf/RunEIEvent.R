library(R.utils)
library(EIEvent)

appStem <- cmdArg("app",NULL)
if (FALSE) {
  appStem <- "SummerCamp"
}

source("/usr/local/share/Proc4/EIini.R")

EI.config <- fromJSON(file.path(config.dir,"config.json"),FALSE)

app <- as.character(Proc4.config$apps[appStem])
if (length(app)==0L || any(app=="NULL")) {
  stop("Could not find app for ",appStem)
}
if (!(appStem %in% EI.config$appStem)) {
  stop("Configuration not set for app ",appStem)
}

logfile <- (file.path(logpath, sub("<app>",appStem,EI.config$logname)))
if (interactive()) {
  flog.appender(appender.tee(logfile))
} else {
  flog.appender(appender.file(logfile))
}
flog.threshold(EI.config$loglevel)

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

doRunrun(app,EI.config,EIeng.local,config.dir,outdir)
