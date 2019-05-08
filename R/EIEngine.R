EIEngine <-
   setRefClass("EIEngine",
               fields=c(app="character",
                        dburi="character",
                        dbname="character",
                        P4dbname="character",
                        p4db="MongoDB",
                        waittime="numeric",
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
                              port="",dbname="EIRecords",P4dbname="Proc4",
                              waittime=.25,...) {
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
                       p4DB <- NULL # mongo("AuthorizedApps",P4dbname,dburi)
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
                                 P4dbname=P4dbname,p4db=p4DB,
                                 waittime=waittime, ...)
                     },
                   P4db = function () {
                     if(is.null(p4db))
                       p4db <<- mongo("AuthorizedApps",P4dbname,dburi)
                     p4db
                   },
                   isActivated = function() {
                     rec <- P4db()$find(buildJQuery(app=app(eng)),limit=1)
                     if (length(rec)==0L) return(FALSE)
                     rec$active
                   },
                   activate = function() {
                     if (length(P4db()$find(buildJQuery(app=app(eng))))==0L) {
                       P4db()$insert(buildJQuery(app=app(eng),active=TRUE))
                     } else {
                       P4db()$update(buildJQuery(app=app(eng)),
                                     '{"$set":{"active":true}}')
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


setMethod("app","EIEngine", function (x) x$app)


##########################################
## Running Rules.
## Logic is slightly different for each rule type, so separate functions.

runStatusRules <- function(eng,state,event) {
  flog.debug("Status Rules for %s:%s",uid(event),toString(timestamp(event)))
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
             toString(timestamp(event)))
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
  flog.debug("Reset Rules for %s:%s",uid(event),toString(timestamp(event)))
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
  flog.debug("Context Rules for %s:%s",uid(event),toString(timestamp(event)))
  ruleList <- eng$findRules(verb(event),object(event),context(state),
                            "Context")
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

runTriggerRules <- function(eng,state,event) {
  flog.debug("Trigger Rules for %s:%s",uid(event),toString(timestamp(event)))
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
  flog.debug("")                        #Blank line for visibility.
  flog.info("New Event for %s: (%s %s) %s",uid(event),
             verb(event),object(event),
             toString(timestamp(event)))
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
  if (interactive() && FALSE) recover()
  if (is(out,'try-error')) {
    flog.warn("Got error %s, not saving status.",toString(out))
    return (out)
  }
  if (interactive() && FALSE) recover()
  eng$saveStatus(out)
  if (interactive() && TRUE) recover()
}

mainLoop <- function(eng) {
  withFlogging({
    flog.info("Application Engine %s starting.", app(eng))
    active <- eng$isActivated()
    while (active) {
      eve <- eng$fetchNextEvent()
      if (is.null(eve)) {
        ## Queue is empty, wait and check again.
        Sys.sleep(eng$waittime)
        ## Check for deactivation signal.
        active <- eng$isActivated()
      } else {
        out <- handleEvent(eng,eve)
        if (is(out,'try-error')) {
          eng$setError(eve,out)
        }
        eng$setProcessed(eve)
      }
    }
  flog.info("Application Engine %s was deactivated.",
            app(eng))
  },
  context=sprintf("Running Application %s",app(eng)))
  flog.info("Application Engine %s stopping.",app(eng))
}


