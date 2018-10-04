### This is an object which tracks the status of the system.


### A timer is an object which keeps track of how long the user has
### spent in a particular class of activity.
setClass("Timer",
         slots=c(name="character",
                 startTime="POSIXct",
                 totalTime="difftime"))
Timer <- function(name) {
  new("Timer",name=name,startTime=as.POSIXct(NA),
      totalTime=as.difftime(0,units="secs"))
}

setMethod("start","Timer",
          function(timer,time,restartOK=FALSE) {
            if (!restartOK && isRunning(timer)) {
              stop("Timer ",name,"is already running.")
            }
            timer@startTime <- as.POSIXct(time)
            timer})

setMethod("pause",c("Timer","POSIXt"),
          function(timer,time,runningCheck=TRUE) {
            if (runningCheck && !isRunning(timer)) {
              stop("Timer ",name,"is not running.")
            }
            timer@totalTime <- timer@totalTime +
              as.POSIXct(time) - timer@startTime
            timer@startTime <- as.POSIXct(NA)
            timer})

setMethod("resume","Timer", function(timer,time) start(timer,time))

setMethod("isRunning","Timer",
          function(timer)
            !is.na(timer@startTime))

setMethod("totalTime","Timer", function(timer) timer@totalTime)

setMethod("timeSoFar","Timer",
          function(timer,now) {
            if (isRunning(timer)) {
              timer@totalTime + as.POSIXct(now) - timer@startTime
            } else {
              timer@totalTime
            }})

setMethod("reset","Timer" function(timer) {
  timer@startTime <- as.POSIXct(NA)
  timer@totalTime <- as.difftime(0,units="secs")
  timer
}

############################################################
### Status -- Maintains state for one user in one context.

setClass("Status",
         slots=c(app="character",
                 uid="character",
                 context="character",
                 timers="list",
                 flags="list",
                 observables="list",
                 timestamp=POSIXt))
Status <- function (uid,context,timerNames=character(),
                    flags=list(),observables=list(),timestamp=Sys.time(),
                    app="default") {
  timers <- sapply(timerNames, Timer)
  new("Status",app=app,uid=uid,context=context,timers=timers,
      flags=flags,observables=observables,timestamp=timestamp)
})

setMethod("uid","Status", function(x) x@uid)
setMethod("context","Status", function(x) x@context)

setMethod("timer","Status", function(x,name) x@timers[[name]])
setMethod("timerTime","Status", function(x,name,name)
  timeSoFar(x@timers[[name]],now))
setMethod("timer<-","Status", function(x,name,val) {
  if (!is.null(val) && !is(val,"Timer")) {
    stop("Attempting to set timer ",name," to non-timer object")
  }
  x@timers[[name]] <- val
  x
})

setMethod("startTimer","Status", function(x,name,time,restartOK=FALSE)
{
  timer(x,name) <- start(timer(x,name),time,restartOK)
  x
})
setMethod("resumeTimer","Status", function(x,name,time) {
  timer(x,name) <- resume(timer(x,name),time)
  x
}
setMethod("pauseTimer","Status", function(x,name,time,runningCheck=TRUE)
{
  timer(x,name) <- pause(timer(x,name),time,runningCheck)
  x
})

setMethod("flag","Status", function(x,name) x@flags[[name]])
setMethod("flag<-","Status", function(x,name,val) {
  x@flags[[name]] <- val
  x
})

setMethod("obs","Status", function(x,name) x@observables[[name]])
setMethod("obs<-","Status", function(x,name,val) {
  x@observables[[name]] <- val
  x
})

setMethod("copy","Status", function(prototype,uid) {
  new("Status",uid=uid,context=prototype@context,
      timers=prototype@timers, flags=prototype@flags,
      observables=prototype@observables,
      timestamp=prototype@timestamp)
})

splitfield <- function (field) {
  strsplit(gsub("]","",field),"[.[]")[[1]]
}


getJS <- function (field,state,event) {
  fieldexp <- splitfield(field)
  switch(fieldexp[1]
         state=
           switch(fieldexp[2],
                  context=context(state),
                  oldContext=oldContext(state),
                  flags=getJSfield(flag(state,fieldexp[3]),fieldexp[-(1:3)]),
                  observables=getJSfield(obs(state,fieldexp[3]),
                                         fieldexp[-(1:3)]),
                  timers=timerTime(timerTime(state,fieldexp[3],
                                             timestamp(event))),
                  uid=uid(event),
                  stop("Unrecognized field ",field)
                  ),
         event=
           switch(fieldexp[2],
                  data=getJSfield(data(event),fieldexp[-(1:2)]),
                  object=object(event),
                  verb=verb(event),
                  timestamp=timestamp(event),
                  context=context(event),
                  mess=mess(event),
                  sender=sender(event),
                  uid=uid(event),
                  app=app(event),
                  stop("Unrecognized field ",field)
                  ),
         stop("Field name must start with 'state' or 'event':",field)
         )
}

setJS <- function (field,state,value) {
  fieldexp <- splitfield(field)
  if (fieldexp[1] != "state")
    stop("Only fields of the state object can be set",field)
  if (fieldexp[2]=="context") {
    context(state) <- value
  } else if (fieldexp[2]=="flags") {
    if (length(fieldexp)<3L)
      stop("No flag name supplied:", field)
    flag(state,fieldexp[3]) <-
      setJSfield(flag(state,fieldexp[3]), fieldexp[-(1:3)], value)
  } else if (fieldexp[2]=="observables") {
    if (length(fieldexp)<3L)
      stop("No observable name supplied:", field)
    obs(state,fieldexp[3]) <-
      setJSfield(obs(state,fieldexp[3]), fieldexp[-(1:3)], value)
  } else {
    stop("Unrecognized field ",field)
  }
  state
}

getJSfield <- function(obj,fieldlist) {
  if (length(fieldlist) == 0L) return (obj)
  ## Convert integer fields into integers.
  field1 <- suppressWarnings(as.numeric(fieldlist[1]))
  if (is.na(field1) || field1 != as.integer(field1)) {
    field1 <- fieldlist[1]              #Not an integer, use string value.
  }
  getJSfield(obj[[field1]],fieldlist[-1])
}


setJSfield <- function (target,fieldlist,value) {
  if (length(fieldlist==OL) return (value)
  if (length(fieldlist==1L)) {
    target[[fieldlist]] <- value
  } else {
    target[[fieldlist[1]]] <-
      setJSaux(fieldlist[-1],target[[fieldlist[1]]],value)
  }
  target
}









