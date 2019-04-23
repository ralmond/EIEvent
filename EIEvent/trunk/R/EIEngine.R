EIEngine <-
   setRefClass("EIEngine",
               fields=c(app="character",
                        dburi="character",
                        dbname="character",
                        userRecords="UserRecordSet",
                        rules="RuleTable",
                        contexts="ContextSet",
                        events="MongoDB",
                        ruleTests="TestSet",
                        listenerSet="ListenerSet"),
               methods = list(
                   initialize =
                     function(app="default",listeners=list(),
                              username="",password="",host="localhost",
                              port="",dbname="EIRecords",
                              ...) {
                       security <- ""
                       if (nchar(username) > 0L) {
                         if (nchar(password) > 0L)
                           security <- paste(username,password,sep=":")
                         else
                           security <- username
                       }
                       if (nchar(port) > 0L)
                         host <- paste(host,port,sep=":")
                       else
                         host <- host
                       if (nchar(security) > 0L)
                         host <- paste(security,host,sep="@")
                       dburl <- paste("mongodb:/",host,sep="/")
                       flog.info("Connecting to database %s/%s\n",dburl,dbname)
                       evnts <- NULL # mongo("Events",dbname,dburi)
                       ls <- ListenerSet(sender= paste("EIEngine[",app,"]"),
                                         dbname=dbname,
                                         dburi=dburl,
                                         listeners=listeners,
                                         colname="Messages",
                                          ...)
                       rls <- RuleTable$new(app,dbname,dburl)
                       urecs <- UserRecordSet$new(app,dbname,dburl)
                       ctxts <- ContextSet$new(app,dbname,dburl)
                       rTests <- TestSet$new(app,dbname,dburl)
                       callSuper(app=app,dbname=dbname,dburi=dburl,
                                 listenerSet=ls, events=evnts,
                                 rules=rls,userRecords=urecs,
                                 contexts=ctxts,ruleTests=rTests,
                                 ...)
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
                          uid(state), timestamp(state))
               state
             },
             saveStatus = function(state) {
               flog.trace("Saving status for %s, timestamp=%s",
                          uid(state), timestamp(state))
               userRecords$saveStatus(state)
             }
             )

## Context Methods
EIEngine$methods(
             getContext = function(id) {
               matchContext(id,contexts)
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
             findRules = function (verb,object,context,phase) {
               appcon <- applicableContexts(getContext(context))
               rules$findRules(verb,object,appcon,phase)
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
            }
        )


EIEngine <- function(app="default",listeners=list(),
                     username="",password="",host="localhost",
                     port="",dbname="EIRecords",
                     ...) {
  new("EIEngine",app=app,listeners=listeners,username=username,
      password=password,host=host,port=port,dbname=dbname,...)
}

## Listener notification.
setMethod("notifyListeners","EIEngine",
           function(sender,mess) {
             sender$listenerSet$notifyListeners(mess)
           })


##########################################
## Running Rules.
## Logic is slightly different for each rule type, so separate functions.

runStatusRules <- function(eng,state,event) {
  flog.debug("Status Rules for %s:%s",uid(event),timestamp(event))
  ruleList <- eng$findRules(verb(event),object(event),context(state),
                            "Status")
  for (rl in ruleList) {
    flog.trace("Running Rule %s.",name(rl))
    out <- runRule(state,event,rl,"Status")
    if (is(out,'try-error')) return (out)
    else state <- out
  }
  state
}

runObservableRules <- function(eng,state,event) {
  flog.debug("Observable Rules for %s:%s",uid(event),
             timestamp(event))
  ruleList <- eng$findRules(verb(event),object(event),context(state),
                            "Observable")
  for (rl in ruleList) {
    flog.trace("Running Rule %s.",name(rl))
    out <- runRule(state,event,rl,"Observable")
    if (is(out,'try-error')) return (out)
    else state <- out
  }
  state
}

runResetRules <- function(eng,state,event) {
  flog.debug("Reset Rules for %s:%s",uid(event),timestamp(event))
  ruleList <- eng$findRules(verb(event),object(event),context(state),
                            "Reset")
  for (rl in ruleList) {
    flog.trace("Running Rule %s.",name(rl))
    out <- runRule(state,event,rl,"Reset")
    if (is(out,'try-error')) return (out)
    else state <- out
  }
  state
}

runContextRules <- function(eng,state,event) {
  flog.debug("Context Rules for %s:%s",uid(event),timestamp(event))
  ruleList <- eng$findRules(verb(event),object(event),context(state),
                            "Context")
  for (rl in ruleList) {
    flog.trace("Running Rule %s.",name(rl))
    out <- runRule(state,event,rl,"Context")
    if (is(out,'try-error')) return (out)
    else state <- out
    if (oldContext(state) != context(state)) {
      flog.debug("New context for %s: %s",uid(event),
                 context(state))
      return(state)
    }
  }
  state
}

runTriggerRules <- function(eng,state,event) {
  flog.debug("Trigger Rules for %s:%s",uid(event),timestamp(event))
  ruleList <- eng$findRules(verb(event),object(event),context(state),
                        "Trigger")
  for (rl in ruleList) {
    flog.trace("Running Rule %s.",name(rl))
    out <- runTRule(state,event,rl,eng$listenerSet)
    if (is(out,'try-error')) return (out)
  }
  state
}

processEvent <- function (eng,state,event) {
  flog.debug("New Event %s: %s",uid(event),timestamp(event))
  out <- runStatusRules(eng,state,event)
  if (is(out,'try-error')) return (out)
  else state <- out
  out <- runObservableRules(eng,state,event)
  if (is(out,'try-error')) return (out)
  else state <- out
  out <- runContextRules(eng,state,event)
  if (is(out,'try-error')) return (out)
  else state <- out
  runTriggerRules(eng,state,event)
  if (oldContext(state) != context(state)) {
    out <- runResetRules(eng,state,event)
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
  if (is(out,'try-error')) return (out)
  eng$saveStatus(out)
}
