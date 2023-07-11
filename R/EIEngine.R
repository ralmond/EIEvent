EIEngine <-
   setRefClass("EIEngine",
               fields=c(app="character",
                        adminDB="JSONDB",
                        waittime="numeric",
                        userRecords="UserRecordSet",
                        rules="RuleTable",
                        contexts="ContextSet",
                        events="MessageQueue",
                        ruleTests="TestSet",
                        listenerSet="ListenerSet",
                        processN="numeric"),
               methods = list(
                   initialize =
                     function(app="default",
                              waittime=.25,processN=Inf,
                              listenerSet=NULL,
                              eidbname="EARecords",
                              dburi=mongo::makeDBuri(),
                              admindbname="Proc4",
                              events=new("MongoQueue",app,
                                         messDB=MongoDB("Events",eidbname),
                                         parseEvent),
                              adminDB = MongoDB("AuthorizedApps","Proc4"),
                              rules = newRuleTable(app),
                              userRecords = newUserRecordSet(app),
                              contexts = newContextSet(app),
                              ruleTests = newTestSet(app, rules=rules,
                                                     contexts=contexts),
                              ...){
                       callSuper(app=app,waittime=waittime,
                                 processN=processN,
                                 listenerSet=listenerSet, events=events,
                                 rules=rules,userRecords=userRecords,
                                 contexts=contexts,ruleTests=ruleTests,
                                 adminDB=adminDB,
                                 ...)
                     },
                  admindb = function () {
                     adminDB
                   },
                  activate = function() {
                    if (length(mdbFind(admindb(),buildJQuery(app=app)))==0L) {
                      mdbInsert(admindb(),buildJQuery(app=app,
                                                     appStem=basename(app),
                                                     EIactive=TRUE,
                                                     EIsignal="running"))
                    } else {
                      mdbUpdate(admindb(),buildJQuery(app=app),
                                       '{"$set":{"EIactive":true,"EIsignal":"running"}}')
                    }
                  },
                  deactivate = function() {
                    if (length(mdbFind(admindb(),buildJQuery(app=app)))==0L) {
                      mdbInsert(admindb(),buildJQuery(app=app,
                                                   appStem=basename(app),
                                                   EIactive=FALSE))
                    } else {
                      mdbUpdate(admindb(),buildJQuery(app=app),
                                    '{"$set":{"EIactive":false}}')
                    }
                  },
                  isActivated = function() {
                    rec <- mdbFind(admindb(),buildJQuery(app=app),limit=1)
                    if (length(rec)==0L) return(FALSE)
                    return (isTRUE(as.logical(rec$EIactive)))
                  },
                  shouldHalt = function() {
                    rec <- mdbFind(admindb(),buildJQuery(app=app),limit=1)
                    if (length(rec)==0L) return(FALSE)
                    if (toupper(rec$EIsignal)=="HALT") return(TRUE)
                    FALSE
                  },
                  stopWhenFinished = function() {
                    rec <- mdbFind(admindb(),buildJQuery(app=app),limit=1)
                    if (length(rec)==0L) return(TRUE)
                    if (toupper(rec$EIsignal)!="RUNNING") return(TRUE)
                    FALSE
                  },
                  regFile = function (name,filename,type="data",
                                      doc="") {
                    registerOutput(listenerSet,name,filename,app,
                                   processe="EI",type=type,doc=doc)
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
             eventq = function() {
              events
             },
             setProcessed= function (mess) {
               markAsProcessed(eventq(),mess)
             },
             setError= function (mess,e) {
               markAsError(eventq(),mess,e)
             },
             fetchNextEvent = function() {
               fetchNextMessage(eventq())
             }
        )


newEngine <- function(app="default",
                      dbname="EIRecords",
                      admindbname="Proc4",
                      waittime=.25,processN=Inf,
                      dburi=character(),
                      sslops=mongolite::ssl_options(),
                      noMongo=length(dburi)==0L,
                      mongoverbose=FALSE,
                      listenerSet=NULL,
                      eventcol="Events",
                      events=new("MongoQueue",app,
                                 MongoDB(eventcol,dbname,dburi,
                                         mongoverbose,noMongo,sslops)),
                      aacol="AuthorizedAppps",
                      adminDB=MongoDB(aacol,admindbname,dburi,
                                      mongoverbose,noMongo,sslops),
                      rulecol="Rules",
                      rules=newRuleTable(app,rulecol,dbname,dburi,
                                         mongoverbose,noMongo,sslops),
                      urcol="States",
                      userRecords=newUserRecordSet(app,urcol,dbname,dburi,
                                                   mongoverbose,noMongo,sslops),
                      contextcol="Contexts",
                      contexts=newContextSet(app,contextcol,dbname,dburi,
                                             mongoverbose,noMongo,sslops),
                      testcol="Tests",
                      ruleTests=newTestSet(app,testcol,dbname,dburi,
                                           mongoverbose,noMongo,sslops)
                      ) {
  new("EIEngine",app,waittime,processN,listenerSet,
              events,adminDB,rules,userRecords,contexts,
              ruleTests)
}

## Listener notification.
setMethod("notifyListeners","EIEngine",
           function(sender,message) {
             sender$listenerSet$notifyListeners(message)
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


