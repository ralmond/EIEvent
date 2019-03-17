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
               }))


## Listener notification.
setMethod("notifyListeners","EIEngine",
           function(sender,mess) {
             sender$listenerSet$notifyListeners(mess)
           })


## Student Record Methods
EIEngine$methods(
             newStudent = function (uid) {
               state <- userRecords$newStudent(uid)
               state
             },
             getStatus = function (uid) {
               state <- userRecords$getStatus(uid)
               if (is.null(state)) {
                 flog.warn("Didn't find expected status for %s, using default.",
                           uid)
                 state <- newStudent(uid)
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

## Rule Methods
EIEngine$methods(
             findRules = function (verb,object,context,phase) {
               appcon <- applicableContexts(getContext(context))
               rules$findRules(verb,object,appcon,phase)
             },
             runRule = function (state,event,rule,phase) {
               withFlogging({
                 satisfied <- checkCondition(condition(rule),state,event)
                 flog.trace("Condition for rule %s for %s: %s",
                            name(rule),uid(state),as.character(satisfied))
                 if (isTRUE(satisfied)) {
                   state <- executePredicate(predicate(rule),state,event)
                   flog.trace("New state",state,capture=TRUE)
                 }
                 state
               },state=state,event=event,rule=rule,phase=phase,
               context=paste("Running rule",name(rule),"for",uid(state)))
             },
             runTRule = function (state,event,rule) {
               withFlogging({
                 satisfied <- checkCondition(condition(rule),state,event)
                 flog.trace("Condition for rule %s for %s: %s",
                            name(rule),uid(state),as.character(satisfied))
                 if (isTRUE(satisfied)) {
                   messes <- buildMessages(predicate(rule),state,event)
                   lapply(messes,listenerSet$notifyListeners)
                 }
               },state=state,event=event,rule=rule,phase="Trigger",
               context=paste("Running rule",name(rule),"for",uid(state)))
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


## Handle Event
EIEngine$methods(
             handleEvent = function (event) {
               flog.debug("New Event %s: %s",uid(event),timestamp(event))
               state <- getStatus(uid(event))
               out <- runStatusRules(state,event)
               if (is(out,'try-error')) return (out)
               else state <- out
               out <- runObservableRules(state,event)
               if (is(out,'try-error')) return (out)
               else state <- out
               out <- runContextRules(state,event)
               if (is(out,'try-error')) return (out)
               else state <- out
               runTriggerRules(state,event)
               if (oldContext(state) != context(state)) {
                 out <- runResetRules(state,event)
                 if (is(out,'try-error')) return (out)
                 else state <- out
                 state@oldContext <- context(state)
               }
               state@timestamp <- timestamp(event)
               saveStatus(state)
             },
             runStatusRules = function(state,event) {
               flog.debug("Status Rules for %s:%s",uid(event),timestamp(event))
               ruleList <- findRules(verb(event),object(event),context(state),
                                     "Status")
               for (rl in ruleList) {
                 flog.trace("Running Rule %s.",name(rl))
                 out <- runRule(state,event,rl,"Status")
                 if (is(out,'try-error')) return (out)
                 else state <- out
               }
               state
             },
             runObservableRules = function(state,event) {
               flog.debug("Observable Rules for %s:%s",uid(event),
                          timestamp(event))
               ruleList <- findRules(verb(event),object(event),context(state),
                                     "Observable")
               for (rl in ruleList) {
                 flog.trace("Running Rule %s.",name(rl))
                 out <- runRule(state,event,rl,"Observable")
                 if (is(out,'try-error')) return (out)
                 else state <- out
               }
               state
             },
             runResetRules = function(state,event) {
               flog.debug("Reset Rules for %s:%s",uid(event),timestamp(event))
               ruleList <- findRules(verb(event),object(event),context(state),
                                     "Reset")
               for (rl in ruleList) {
                 flog.trace("Running Rule %s.",name(rl))
                 out <- runRule(state,event,rl,"Reset")
                 if (is(out,'try-error')) return (out)
                 else state <- out
               }
               state
             },
             runContextRules = function(state,event) {
               flog.debug("Context Rules for %s:%s",uid(event),timestamp(event))
               ruleList <- findRules(verb(event),object(event),context(state),
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
             },
             runTriggerRules = function(state,event) {
               flog.debug("Trigger Rules for %s:%s",uid(event),timestamp(event))
               ruleList <- findRules(verb(event),object(event),context(event),
                                     "Trigger")
               for (rl in ruleList) {
                 flog.trace("Running Rule %s.",name(rl))
                 out <- runTRule(state,event,rl)
                 if (is(out,'try-error')) return (out)
                 else state <- out
               }
               state
             }
)

## Rule Tests
EIEngine$methods(
              testRules = function (state, event) {
                flog.debug("Testing Event %s: %s",uid(event),timestamp(event))
                out <- runStatusRules(state,event)
                if (is(out,'try-error')) return (out)
                else state <- out
                out <- runObservableRules(state,event)
                if (is(out,'try-error')) return (out)
                else state <- out
                out <- runContextRules(state,event)
                if (is(out,'try-error')) return (out)
                else state <- out
                runTriggerRules(state,event)
                if (oldContext(state) != context(state)) {
                  out <- runResetRules(state,event)
                  if (is(out,'try-error')) return (out)
                  else state <- out
                  state@oldContext <- context(state)
                }
                state@timestamp <- timestamp(event)
                state
              })

