EIEngine <-
   setRefClass("EIEngine",
               fields=c(app="character",
                        dburi="character",
                        dbname="character",
                        admindbname="character",
                        adminDB="MongoDB",
                        fileDB="MongoDB",
                        waittime="numeric",
                        userRecords="UserRecordSet",
                        rules="RuleTable",
                        contexts="ContextSet",
                        events="MongoDB",
                        ruleTests="TestSet",
                        listenerSet="ListenerSet",
                        processN="numeric"),
               methods = list(
                   initialize =
                     function(app="default",listenerSet=NULL,
                              dburi="",dbname="EIRecords",admindbname="Proc4",
                              waittime=.25,processN=Inf,...) {
                       flog.info("Connecting to database %s/%s\n",dburi,dbname)
                       evnts <- NULL # mongo("Events",dbname,dburi)
                       aDB <- NULL # mongo("AuthorizedApps",adminbname,dburi)
                       rls <- RuleTable$new(app,dbname,dburi)
                       urecs <- UserRecordSet$new(app,dbname,dburi)
                       ctxts <- ContextSet$new(app,dbname,dburi)
                       rTests <- TestSet$new(app,dbname,dburi)
                       callSuper(app=app,dbname=dbname,dburi=dburi,
                                 listenerSet=listenerSet, events=evnts,
                                 rules=rls,userRecords=urecs,
                                 contexts=ctxts,ruleTests=rTests,
                                 admindbname=admindbname,adminDB=aDB,
                                 waittime=waittime,
                                 processN=processN,
                                 ...)
                     },
                  admindb = function () {
                     if(is.null(adminDB) && length(dburi)>0L)
                       adminDB <<- mongo("AuthorizedApps",admindbname,dburi)
                     adminDB
                   },
                  filedb = function () {
                     if(is.null(fileDB) && length(dburi)>0L)
                       fileDB <<- mongo("OutputFiles",admindbname,dburi)
                     fileDB
                   },
                  activate = function() {
                    if (length(admindb()$find(buildJQuery(app=app)))==0L) {
                      admindb()$insert(buildJQuery(app=app,
                                                   appStem=basename(app),
                                                   EIactive=TRUE,
                                                   EIsignal="running"))
                    } else {
                      admindb()$update(buildJQuery(app=app),
                                       '{"$set":{"EIactive":true,"EIsignal":"running"}}')
                    }
                  },
                  deactivate = function() {
                    if (length(admindb()$find(buildJQuery(app=app)))==0L) {
                      admindb()$insert(buildJQuery(app=app,
                                                   appStem=basename(app),
                                                   EIactive=FALSE))
                    } else {
                      admindb()$update(buildJQuery(app=app),
                                    '{"$set":{"EIactive":false}}')
                    }
                  },
                  isActivated = function() {
                    rec <- admindb()$find(buildJQuery(app=app),limit=1)
                    if (length(rec)==0L) return(FALSE)
                    return (isTRUE(as.logical(rec$EIactive)))
                  },
                  shouldHalt = function() {
                    rec <- admindb()$find(buildJQuery(app=app),limit=1)
                    if (length(rec)==0L) return(FALSE)
                    if (toupper(rec$EIsignal)=="HALT") return(TRUE)
                    FALSE
                  },
                  stopWhenFinished = function() {
                    rec <- admindb()$find(buildJQuery(app=app),limit=1)
                    if (length(rec)==0L) return(TRUE)
                    if (toupper(rec$EIsignal)!="RUNNING") return(TRUE)
                    FALSE
                  },
                  regFile = function (name,filename,type="data",
                                      timestamp=Sys.time(),doc="") {
                    fdb <- filedb()
                    if (!is.null(fdb)) {
                      fdb$replace(buildJQuery(app=basename(app),name=name),
                                  buildJQuery(app=basename(app),
                                              process="EI", type=type,
                                              name=name, filename=filename,
                                              timestamp=timestamp,
                                              doc=doc),
                                  upsert=TRUE)
                    }
                  },
                  show=function() {
                    methods::show(paste("<EIEvent: ",app,">"))
                  }))


## Student Record Methods
EIEngine$methods(
             newUser = function (uid) {
               state <- userRecords$newUser(uid)
               state
             },
             getStatus = function (uid) {
               state <- userRecords$getStatus(uid)
               if (is.null(state)) {
                 flog.warn("Didn't find expected status for %s, using default.",
                           uid)
                 state <- newUser(uid)
               }
               flog.trace("Fetching status for %s, timestamp= %s",
                          uid(state), toString(timestamp(state)))
               state
             },
             saveStatus = function(state) {
               flog.trace("Saving status for %s, timestamp=%s",
                          uid(state), toString(timestamp(state)))
               userRecords$saveStatus(state)
             },
             clearStatusRecords = function(clearDefault=FALSE) {
               userRecords$clearAll(clearDefault)
             }
             )

## Context Methods
EIEngine$methods(
             getContext = function(id) {
               conObj <- matchContext(id,contexts)
               if (is.null(conObj)) {   #Missing from databalse
                 flog.warn("Context %s missing from database.",id)
                 conObj <- Context(id,id,NA_integer_,
                                   doc="Autogenerated context.",
                                   app=app)
               }
               conObj
             },
             clearContexts = function () {
               contexts$clearAll()
             },
             addContexts = function (conmat) {
               loadContexts(conmat,contexts,app)
             }
          )

##Rule Methods
EIEngine$methods(
             findRules = function (verb,object,context,phase=NULL) {
               appcon <- applicableContexts(getContext(context))
               rules$findRules(verb,object,appcon,phase)
             },
             clearAllRules = function() {
               rules$clearAll()
             },
             loadRules = function (rlist,stopOnDups=TRUE) {
               loadRulesFromList(rules,rlist,stopOnDups)
             },
             loadAndTest=function(script,stopOnDups=FALSE) {
               testAndLoad(rules,script,stopOnDups)
             })

## Event Methods
EIEngine$methods(
             eventdb = function() {
               if (is.null(events)) {
                 events <<- mongo("Events",dbname,dburi)
               }
              events
             },
             setProcessed= function (mess) {
               markAsProcessed(mess,eventdb())
             },
             setError= function (mess,e) {
               markAsError(mess,eventdb(),e)
             },
             fetchNextEvent = function() {
               getOneRec(buildJQuery(app=app,processed=FALSE),
                         eventdb(),parseEvent,sort = c(timestamp = 1))
             }
        )


EIEngine <- function(app="default",dburi=makeDBuri(),
                     listenerSet=NULL,
                     dbname="EIRecords",admindbname="Proc4",
                     processN=Inf, waittime=.25,
                     ...) {
  new("EIEngine",app=app,dburi=dburi,listenerSet=listenerSet,
      dbname=dbname, admindbname=admindbname,processN=processN,
      waittime=waittime,...)
}

## Listener notification.
setMethod("notifyListeners","EIEngine",
           function(sender,mess) {
             sender$listenerSet$notifyListeners(mess)
           })


setMethod("app","EIEngine", function (x) x$app)


##########################################
## Running Rules.
## Logic is slightly different for each rule type, so separate functions.

runStatusRules <- function(eng,state,event,rules) {
  ruleList <- rules[sapply(rules,function(r) ruleType(r)=="Status")]
  flog.debug("%d Status Rules for %s:%s", length(ruleList),
             uid(event),toString(timestamp(event)))
  for (rl in ruleList) {
    flog.trace("Running Rule %s.",name(rl))
    out <- runRule(state,event,rl,"Status")
    if (is(out,'try-error')) return (out)
    else state <- out
  }
  state
}

runObservableRules <- function(eng,state,event,rules) {
  ruleList <- rules[sapply(rules,function(r) ruleType(r)=="Observable")]
  flog.debug("%d Observable Rules for %s:%s",length(ruleList),
             uid(event), toString(timestamp(event)))
  for (rl in ruleList) {
    flog.trace("Running Rule %s.",name(rl))
    out <- runRule(state,event,rl,"Observable")
    if (is(out,'try-error')) return (out)
    else state <- out
  }
  state
}

runResetRules <- function(eng,state,event,rules) {
  if (length(rules)==0) return (state)
  ruleList <- rules[sapply(rules,function(r) ruleType(r)=="Reset")]
  flog.debug("%d Rest Rules for %s:%s",length(ruleList),
             uid(event), toString(timestamp(event)))
  for (rl in ruleList) {
    flog.trace("Running Rule %s.",name(rl))
    out <- runRule(state,event,rl,"Reset")
    if (is(out,'try-error')) return (out)
    else state <- out
  }
  state
}

runContextRules <- function(eng,state,event,rules) {
  ruleList <- rules[sapply(rules,function(r) ruleType(r)=="Context")]
  flog.debug("%d Context Rules for %s:%s",length(ruleList),
             uid(event), toString(timestamp(event)))
  for (rl in ruleList) {
    flog.trace("Running Rule %s.",name(rl))
    out <- runRule(state,event,rl,"Context")
    if (is(out,'try-error')) return (out)
    else state <- out
    if (oldContext(state) != context(state)) {
      names(context(state)) <- NULL     #This seems to be causing problems.
      flog.debug("New context for %s: %s",uid(event),
                 context(state))
      return(state)
    }
  }
  state
}

runTriggerRules <- function(eng,state,event,rules) {
  ruleList <- rules[sapply(rules,function(r) ruleType(r)=="Trigger")]
  flog.debug("%d Trigger Rules for %s:%s",length(ruleList),
             uid(event), toString(timestamp(event)))
  for (rl in ruleList) {
    flog.trace("Running Rule %s.",name(rl))
    out <- runTRule(state,event,rl,eng$listenerSet)
    if (is(out,'try-error')) return (out)
  }
  state
}

processEvent <- function (eng,state,event) {
  flog.debug("")                        #Blank line for visibility.
  flog.info("New Event for %s: (%s %s) %s",uid(event),
             verb(event),object(event),
             toString(timestamp(event)))
  rules <- eng$findRules(verb(event),object(event),context(state))
  flog.info("%d rules for event", length(rules))
  if (length(rules)==0L) {
    return (NULL)
  }
  out <- runStatusRules(eng,state,event,rules)
  if (is(out,'try-error')) return (out)
  else state <- out
  out <- runObservableRules(eng,state,event,rules)
  if (is(out,'try-error')) return (out)
  else state <- out
  out <- runContextRules(eng,state,event,rules)
  if (is(out,'try-error')) return (out)
  else state <- out
  runTriggerRules(eng,state,event,rules)
  if (oldContext(state) != context(state)) {
    ## Need to reset according to new context.  [Bug 82]
    rules <- eng$findRules(verb(event),object(event),context(state),
                           "Reset")
    out <- runResetRules(eng,state,event,rules)
    if (is(out,'try-error')) return (out)
    else state <- out
    state@oldContext <- context(state)
  }
  state@timestamp <- timestamp(event)
  state
}

handleEvent <-  function (eng,event) {
  state <- eng$getStatus(uid(event))
  out <- processEvent(eng,state,event)
  if (is.null(out)) {
    flog.debug("No rules, doing nothing.")
    return(state)
  }
  if (is(out,'try-error')) {
    flog.warn("Got error %s, not saving status.",toString(out))
    return (out)
  }
  eng$saveStatus(out)
  return (out)
}

mainLoop <- function(eng) {
  flog.info("Evidence Identification Engine %s starting.", app(eng))
  eng$activate()
  on.exit({
    flog.info("EI Engine %s was deactivated.", app(eng))
    eng$deactivate()})
  withFlogging({
    active <- TRUE
    while (active) {
      if (eng$shouldHalt()) {
        flog.fatal("EI Engine %s halted because of user request.",
                   basename(app(eng)))
        break
      }
      eve <- eng$fetchNextEvent()
      if (is.null(eve)) {
        ## Queue is empty, wait and check again.
        Sys.sleep(eng$waittime)
        ## Check for deactivation signal.
        if (eng$stopWhenFinished()) {
          flog.info("EA Engine %s stopping because queue is empty.",
                    basename(app(eng)))
          active <- FALSE
        } else {
          active <- TRUE
        }
      } else {
        if (is.finite(eng$processN))
          flog.debug("Processing event %d.",eng$processN)
        out <- handleEvent(eng,eve)
        if (is(out,'try-error')) {
          eng$setError(eve,out)
        }
        eng$setProcessed(eve)
        eng$processN <- eng$processN -1
        active <- eng$processN > 0
      }
    }
  },
  context=sprintf("Running EI Application %s",app(eng)))
  flog.info("Evidence Identification Engine %s stopping.",app(eng))
}


