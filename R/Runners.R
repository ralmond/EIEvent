

doLoad <- function(app, EI.config,EIeng.local, config.dir,override=FALSE) {
  ruledir <- file.path(config.dir,
                       ifelse(!is.null(EI.config$ruledir),EI.config$ruledir,
                              "rules"))
  sapp <- basename(app)
  cl <- new("CaptureListener")

  ## <<HERE>> This should use configuration from EI.config and EIeng.local.
  flog.info("Building Engine for application %s.",sapp)
  EIeng.params <- c(EI.config$EIEngine,EIeng.local)
  EIeng.params$listenerSet <-
    ListenerSet(sender= sub("<app>",sapp,EI.config$sender),
                dbname=EI.config$dbname, dburi=EIeng.local$dburi,
                listeners=listeners,
                colname=EI.config$lscolname)
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
    flog.info("Loading context file %s.",contextFile)
    conts <- read.csv(file.path(ruledir,paste(contextFile,"csv",sep=".")))
    eng$addContexts(conts)
  }

  flog.info("Rebuilding Rule Sets for application %s.",sapp)
  eng$rules$clearAll()

  for (ruleFile in EI.config$rules) {
    flog.info("Loading rule file %s.",ruleFile)
    rules <- lapply(fromJSON(file.path(ruledir,paste(ruleFile,"json",sep=".")),
                             FALSE),
                    parseRule)
    eng$loadRules(rules)
  }

  for (ruleFile in EI.config$rulesWithTests) {
    flog.info("Loading and Tesing rule file %s.",ruleFile)
    eng$loadAndTest(file.path(ruledir,paste(ruleFile,"json",sep=".")))
  }

  flog.info("Rebuilding default student record %s.",sapp)
  defaultRec <- eng$newUser("*DEFAULT*")
  defaultRec@observables <- EI.config$defaultRecordData
  eng$saveStatus(defaultRec)


}

doRunrun <- function (app, EI.config,  EIeng.local, config.dir,
                      outdir=config.dir,
                      override = FALSE) {

  ruledir <- file.path(config.dir,
                       ifelse(!is.null(EI.config$ruledir),EI.config$ruledir,
                              "rules"))
  sapp <- basename(app)
  flog.info("Building and configuring engine.")
  listeners <- lapply(EI.config$listeners, buildListener,app,dburi)
  names(listeners) <- sapply(listeners,listenerName)

  EIeng.params <- c(EI.config$EIEngine,EIeng.local)
  EIeng.params$listenerSet <-
    ListenerSet(sender= sub("<app>",sapp,EI.config$sender),
                dbname=EI.config$dbname, dburi=EIeng.local$dburi,
                listeners=listeners,
                colname=EI.config$lscolname)

  EIeng.params$app <- app
  eng <- do.call(EIEngine,EIeng.params)

  if (eng$isActivated()) {
    flog.warning("EI Engine for application %s already running.",sapp)
    if (!override) stop("EI Engine already running:", sapp)
  }

  flog.info("Preparing Database.")
  if (dburi != "") {
    if (EI.config$filter$doRemove) {
      flog.debug("Clearing old events.")
      remquery <- EI.config$filter$remove
      if (!is.null(names(remquery)))
        remquery <- list(remquery)      #Single query make it multiple.
      for (rq in remquery) {
        rquery <- do.call(buildJQuery,c(list(app=app),rq))
        flog.trace("Removing %s",rquery)
        eng$eventdb()$remove(rquery)
      }
    }
    if (isTRUE(EI.config$SRreset)) {
      flog.debug("Clearing old student records.")
      eng$userRecords()$clearAll(FALSE)   #Don't clear default
    }
    for (fil in EI.config$importFile) {
      flog.info("Importing from file  %s.", EI.config$importFile)
      impf <- file.path(config.dir,fil)
      if (!file.exists(impf)) {
        flog.warn("File %s does not exist, skipping import.",
                  EI.confg$importFile)
      } else {
        status <-
          system2("mongoimport",c("--jsonArray",
                                  "-d",EIeng.local$dbname,
                                  "-c","Events",
                                  impf), stdout=TRUE, stderr=TRUE)
        if (!is.null(attr(status,"status"))) {
          flog.error("Got error when loading import file.")
          flog.error("Error:",status,capture=TRUE)
        }
      }
    }
    if (EI.config$filter$doPurge) {
      flog.debug("Purging Unused events.")
      purquery <- EI.config$filter$purge
      if (!is.null(names(purquery)))
        purquery <- list(purquery)      #Single query make it multiple.
      for (pq in purquery) {
        pquery <- do.call(buildJQuery,c(list(app=app),pq))
        flog.trace("Purging %s",pquery)
        eng$eventdb()$remove(pquery)
      }
    }
    if (isTRUE(EI.config$filter$doReprocess)) {
      flog.debug("Clearing reprocessed flags.")
      rquery <- buildJQuery(c(list(app=app,EI.config$filter$update)))
      eng$eventdb()$update(rquery,'{"$set":{"processed":false}}',multiple=TRUE)
    }
  }
  flog.info("Reseting Listners.")
  if (!is.null(EI.config$listenerReset)) {
    resetListeners(eng$listenerSet,as.character(EI.config$listenerReset),app)
  }

  if (EI.config$limitNN=="ALL") {
    eng$processN <- eng$evidenceSets()$count(buildJQuery(app=app,
                                                         processed=FALSE))
  } else {
    eng$processN <- as.numeric(EI.config$limitNN)
  }
  flog.info("Beginning EI for application %s.",basename(app))
  if (is.finite(eng$processN)) {
    flog.info("%d messages queued.",eng$processN)
  } else {
    flog.info("Running in server mode.")
  }
  mainLoop(eng)

  for (tl in listeners) {
    if (!is(tl,"TableListeners")) next
    flog.info("Building output Table for %s.",name(tl))
    tab <- tl$returnDF()
    write.csv(tab,file.path(outdir,paste(name(tl),"csv",sep=".")))
  }

}
