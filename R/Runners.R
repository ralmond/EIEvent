

doLoad <- function(app, EI.config,EIeng.local, config.dir,override=FALSE) {
  ruledir <- file.path(config.dir,
                       ifelse(!is.null(EI.config$ruledir),EI.config$ruledir,
                              "rules"))
  sapp <- basename(app)
  dburi <- EIeng.local$dburi
  if (!is.null(dburi) && dburi == "") dburi <- NULL
  sslops <- EIeng.local$ssloptions
  if (is.null(sslops)) sslops <- mongolite::ssl_options()
  dbname <- EIeng.local$dbname
  if (is.null(dbname)) dbname<-"EIRecords"
  admindbname <- EIeng.local$admindbname
  if (is.null(admindbname)) admindbname<-"Proc4"
  lscolName <- EIeng.local$lscolName
  if (is.null(lscolName)) lscolName <- "Messages"
  registrycol <- EIeng.local$registrycol
  if (is.null(registrycol)) registrycol <- "OutputFiles"
  cl <- new("CaptureListener")

  flog.info("Building Engine for application %s.",sapp)
  EIeng.params <-
    c(EI.config$EIEngine,app=app,dbname=dbname,admindbname=admindbname,
      dburi=dburi)
  EIeng.params$sslops <- sslops # Don't want R unlisting this
  EIeng.params$aacol <- ifelse(is.null(EIeng.local$aacol),
                               "AuthorizedApps",
                               EIEng.local$aacol)
  EIeng.params$eventcol <- ifelse(is.null(EIeng.local$eventcol),
                                  "Events",
                                  EIEng.local$eventcol)
  EIeng.params$rulecol <- ifelse(is.null(EIeng.local$rulecol),
                                 "Rules",
                                 EIEng.local$rulecol)
  EIeng.params$urcol <- ifelse(is.null(EIeng.local$urcol),
                               "States",
                               EIEng.local$urcol)
  EIeng.params$contextcol <- ifelse(is.null(EIeng.local$contextcol),
                               "Contexts",
                               EIEng.local$contextcol)
  EIeng.params$testcol <- ifelse(is.null(EIeng.local$testcol),
                               "Tests",
                               EIEng.local$testcol)
  EIeng.params$mongoverbose <- isTRUE(EIeng.local$mongoverbose)

  ## Build a listener set with no listeners as we need to register our output file,
  ## but not do any other processing.
  EIeng.params$listenerSet <-
    withFlogging({
      buildListenerSet(sender= sub("<app>",sapp,EI.config$sender),
                        list(),app,
                        EI.config$colnames$listenerSetLog,
                        dburi,sslops,EI.config$colnames$registry,
                        admindbname,mongoverbose=FALSE)
      }, context="Building listener set.")
    if (is(EIeng.params$listenerSet,'try-error')) {
      flog.fatal("Could not build listener set: %s",EIeng.params$listenerSet)
      stop(EIeng.params$listenerSet)
    }

  EIeng.params$app <- app
  eng <- do.call(EIEngine,EIeng.params)

  if (eng$isActivated()) {
    flog.warn("EI Engine for application %s already running.",sapp)
    if (!override) stop("EI Engine already running:", sapp)
  }

  flog.info("Rebuilding contexts for application %s.",sapp)
  eng$clearContexts()
  initCon <- data.frame(CID="*INITIAL*",Name="*INITIAL*",Number=0)
  eng$addContexts(initCon)
  for (contextFile in EI.config$contextDesriptions) {
    contex <- sprintf("Loading context file %s.",contextFile)
    flog.info(contex)
    withFlogging({
      conts <- read.csv(file.path(ruledir,paste(contextFile,"csv",sep=".")))
      eng$addContexts(conts)
    }, context=contex)
  }

  flog.info("Rebuilding Rule Sets for application %s.",sapp)
  eng$rules$clearAll()

  for (ruleFile in EI.config$rules) {
    contex <- sprintf("Loading rule file %s.",ruleFile)
    flog.info(contex)
    withFlogging({
      rules <- lapply(fromJSON(file.path(ruledir,
                                         paste(ruleFile,"json",sep=".")),
                               FALSE),
                      parseRule)
      eng$loadRules(rules)
    }, context=contex)
  }

  for (ruleFile in EI.config$rulesWithTests) {
    contex <- sprintf("Loading and Tesing rule file %s.",ruleFile)
    flog.info(contex)
    withFlogging({
      eng$loadAndTest(file.path(ruledir,paste(ruleFile,"json",sep=".")))
      }, context=contex)
  }

  contex <- sprintf("Rebuilding default student record %s.",sapp)
  flog.info(contex)
  withFlogging({
    defaultRec <- eng$newUser("*DEFAULT*")
    defaultRec@observables <- EI.config$defaultRecordData
    eng$saveStatus(defaultRec)
  }, context=contex)

}

doRunrun <- function (app, EI.config,  EIeng.local, config.dir,
                      outdir=config.dir,
                      override = FALSE, logfile="", noprep=FALSE) {

  ruledir <- file.path(config.dir,
                       ifelse(!is.null(EI.config$ruledir),EI.config$ruledir,
                              "Rules"))
  sapp <- basename(app)
  dburi <- EIeng.local$dburi
  if (!is.null(dburi) && dburi == "") dburi <- NULL
  sslops <- EIeng.local$ssloptions
  if (is.null(sslops)) sslops <- mongolite::ssl_options()
  dbname <- EIeng.local$dbname
  if (is.null(dbname)) dbname<-"EIRecords"
  admindbname <- EIeng.local$admindbname
  if (is.null(admindbname)) admindbname<-"Proc4"
  lscolName <- EIeng.local$lscolName
  if (is.null(lscolName)) lscolName <- "Messages"
  registrycol <- EIeng.local$registrycol
  if (is.null(registrycol)) registrycol <- "OutputFiles"
  mongoverbose <- isTRUE(EAeng.local$mongoverbose)
  flog.info("Building and configuring engine.")


  EIeng.params <-
    c(EI.config$EIEngine,app=app,dbname=dbname,admindbname=admindbname,
      dburi=dburi)
  EIeng.params$sslops <- sslops # Don't want R unlisting this
  EIeng.params$aacol <- ifelse(is.null(EIeng.local$aacol),
                               "AuthorizedApps",
                               EIEng.local$aacol)
  EIeng.params$eventcol <- ifelse(is.null(EIeng.local$eventcol),
                                  "Events",
                                  EIEng.local$eventcol)
  EIeng.params$rulecol <- ifelse(is.null(EIeng.local$rulecol),
                                 "Rules",
                                 EIEng.local$rulecol)
  EIeng.params$urcol <- ifelse(is.null(EIeng.local$urcol),
                               "States",
                               EIEng.local$urcol)
  EIeng.params$contextcol <- ifelse(is.null(EIeng.local$contextcol),
                               "Contexts",
                               EIEng.local$contextcol)
  EIeng.params$testcol <- ifelse(is.null(EIeng.local$testcol),
                               "Tests",
                               EIEng.local$testcol)
  EIeng.params$mongoverbose <- isTRUE(EIeng.local$mongoverbose)


  EIeng.params$listenerSet <-
    withFlogging({
      buildListenerSet(sender= sub("<app>",sapp,EI.config$sender),
                       config=EI.config$listeners,
                       appid=app,
                       lscol=lscolName,
                       dbname=dbname,
                       dburi=dburi,
                       sslops=sslops,
                       registrycol=registrycol,
                       registrydbname=admindbname,
                       mongoverbose=mongoverbose)
    }, context="Building listener set.")
  if (is(EIeng.params$listenerSet,'try-error')) {
    flog.fatal("Could not build listener set: %s",EIeng.params$listenerSet)
    stop(EIeng.params$listenerSet)
  }
  if (nchar(logfile)>0L) {
    registerOutput(EIeng.params$listenerSet,basename(logfile),logfile,
                   app,"EI","log")
  }

  EIeng.params$app <- app
  eng <- do.call(newEngine,EIeng.params)

  if (eng$isActivated()) {
    flog.warn("EI Engine for application %s already running.",sapp)
    if (!override) stop("EI Engine already running:", sapp)
  }

  if (dburi != "" && !noprep) {
    flog.info("Preparing Database.")
    if (isTRUE(EI.config$filter$doRemove)) {
      cleanMessageQueue(eng$eventq(),EI.config$filter$remove)
    }
    if (isTRUE(EI.config$SRreset)) {
      flog.debug("Clearing old student records.")
      eng$userRecords()$clearAll(FALSE)   #Don't clear default
    }

    ## Import
    data.dir <- EI.config$dataDir
    if (is.null(data.dir)) data.dir <- config.dir
    importMessages(eng$eventq(),EI.config$importFile,data.dir)

    ## Purging Unused message
    if (isTRUE(EI.config$filter$doPurge)) {
      cleanMessageQueue(eng$eventq(),EI.config$filter$purge)
    }

    ## Clearing Processed Flag
    if (isTRUE(EI.config$filter$doReprocess)) {
      resetProcessedMessages(eng$eventq(),EI.config$filter$reprocess)
    }
  }
  if (!is.null(EI.config$listenerReset) && !noprep) {
    flog.info("Reseting Listners.")
    resetListeners(eng$listenerSet,as.character(EI.config$listenerReset),app)
  }

  if (EI.config$limitNN=="ALL") {
    eng$processN <- eng$eventq()$count()
  } else {
    eng$processN <- as.numeric(EI.config$limitNN)
  }
  flog.info("Beginning EI for application %s.",basename(app))
  if (is.finite(eng$processN)) {
    flog.info("%d messages queued.",eng$processN)
  } else {
    flog.info("Running in server mode.")
  }
  tryCatch({
    withFlogging(mainLoop(eng))
  }, finally={
    eng$deactivate()
  })

 Proc4::generateListenerExports(eng$listenerSet,EI.config$listenerExports,
                          app,outdir)

  invisible(eng)
}
