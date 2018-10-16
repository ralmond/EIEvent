## Put all of the generic functions in a place they will be loaded first.

## Contexts
setGeneric("name", function (x) standardGeneric("name"))
setGeneric("doc", function (x) standardGeneric("doc"))

## Rules
setGeneric("ruleName",function(x) standardGeneric("RuleName"))
setGeneric("ruleType",function(x) standardGeneric("ruleType"))
setGeneric("priority",function(x) standardGeneric("priority"))
setGeneric("condition",function(x) standardGeneric("condition"))
setGeneric("predicate",function(x) standardGeneric("predicate"))

## Timers
setGeneric("start",
           function(timer,time,runningCheck=TRUE)
             standardGeneric("start"))
setGeneric("pause",
           function(timer,time,runningCheck=TRUE)
             standardGeneric("start"))
setGeneric("resume",function(timer,time) standardGeneric("resume"))
setGeneric("isRunning",function(timer) standardGeneric("isRunning"))
setGeneric("totalTime",function(timer) standardGeneric("totalTime"))
setGeneric("timeSoFar",function(timer,now) standardGeneric("timeSoFar"))
setGeneric("timeSoFar<-",function(timer,now,value)
  standardGeneric("timeSoFar<-"))
setGeneric("reset",function(timer) standardGeneric("reset"))

## Status
setGeneric("timer",function(x,name) standardGeneric("timer"))
setGeneric("timer<-",function(x,name,value) standardGeneric("timer<-"))
setGeneric("setTimer",function(x,name,value,running,now)
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



