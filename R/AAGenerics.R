## Put all of the generic functions in a place they will be loaded first.

## These are used so that the database is initialized at first use and
## not during load.


## Contexts
setGeneric("doc", function (x) standardGeneric("doc"))
setGeneric("name", function (x) standardGeneric("name"))

## Rules
setGeneric("ruleType",function(x) standardGeneric("ruleType"))
setGeneric("priority",function(x) standardGeneric("priority"))
setGeneric("condition",function(x) standardGeneric("condition"))
setGeneric("predicate",function(x) standardGeneric("predicate"))

## Rule Tests
setGeneric("initial",function(x) standardGeneric("initial"))
setGeneric("event",function(x) standardGeneric("event"))
setGeneric("rule",function(x) standardGeneric("rule"))
setGeneric("queryResult",function(x) standardGeneric("queryResult"))
setGeneric("final",function(x) standardGeneric("final"))


## Timers
setGeneric("start",
           function(timer,time,runningCheck=TRUE)
             standardGeneric("start"))
setGeneric("pause",
           function(timer,time,runningCheck=TRUE)
             standardGeneric("pause"))
setGeneric("resume",function(timer,time) standardGeneric("resume"))
setGeneric("isRunning",function(timer) standardGeneric("isRunning"))
setGeneric("totalTime",function(timer) standardGeneric("totalTime"))
setGeneric("timeSoFar",function(timer,time) standardGeneric("timeSoFar"))
setGeneric("timeSoFar<-",function(timer,time,value)
  standardGeneric("timeSoFar<-"))
setGeneric("reset",function(timer) standardGeneric("reset"))

## Status
setGeneric("timer",function(x,name) standardGeneric("timer"))
setGeneric("timer<-",function(x,name,value) standardGeneric("timer<-"))
setGeneric("setTimer",function(x,timerID,time,running,now)
  standardGeneric("setTimer"))
setGeneric("timerTime",function(x,name,now) standardGeneric("timerTime"))
setGeneric("timerTime<-",function(x,name,now,value)
  standardGeneric("timerTime<-"))
setGeneric("timerRunning",function(x,name,now) standardGeneric("timerRunning"))
setGeneric("timerRunning<-",function(x,name,now,value)
  standardGeneric("timerRunning<-"))
setGeneric("flag",function(x,name) standardGeneric("flag"))
setGeneric("flag<-",function(x,name,value) standardGeneric("flag<-"))
setGeneric("obs",function(x,name) standardGeneric("obs"))
setGeneric("obs<-",function(x,name,value) standardGeneric("obs<-"))
setGeneric("copy",function(prototype,uid) standardGeneric("copy"))

setGeneric("context<-",function(x,value) standardGeneric("context<-"))
setGeneric("oldContext",function(x) standardGeneric("oldContext"))

## RuleTest
setGeneric("initial",function(x) standardGeneric("initial"))
setGeneric("event", function(x) standardGeneric("event"))
setGeneric("rule", function(x) standardGeneric("rule"))
setGeneric("queryResult", function(x) standardGeneric("queryResult"))
setGeneric("final", function(x) standardGeneric("final"))


