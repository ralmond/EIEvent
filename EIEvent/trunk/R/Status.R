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
         slots=c(uid="character",
                 context="character",
                 timers="list",
                 flags="list",
                 observables="list"))
Status <- function (uid,context,timerNames=character(),
                    flags=list(),observables=list()) {
  timers <- sapply(timerNames, Timer)
  new("Status",uid=uid,context=context,timers=timers,
      flags=flags,observables=observables)
})

setMethod("uid","Status", function(x) x@uid)
setMethod("context","Status", function(x) x@context)

setMethod("timer","Status", function(x,name) x@timers[[name]])
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
      observables=prototype@observables)
})











