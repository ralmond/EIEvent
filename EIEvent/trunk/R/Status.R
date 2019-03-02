### This is an object which tracks the status of the system.


### A timer is an object which keeps track of how long the user has
### spent in a particular class of activity.
setOldClass("difftime")
setClass("Timer",
         slots=c(name="character",
                 startTime="POSIXt",
                 totalTime="difftime"))
Timer <- function(name) {
  new("Timer",name=name,startTime=as.POSIXct(NA),
      totalTime=as.difftime(0,units="secs"))
}

setMethod("start",c("Timer","POSIXt"),
          function(timer,time,runningCheck=TRUE) {
            if (runningCheck && isRunning(timer)) {
              stop("Timer ",timer@name,"is already running.")
            }
            timer@startTime <- as.POSIXct(time)
            timer})

setMethod("pause",c("Timer","POSIXt"),
          function(timer,time,runningCheck=TRUE) {
            if (runningCheck && !isRunning(timer)) {
              stop("Timer ",timer@name,"is not running.")
            }
            timer@totalTime <- timer@totalTime +
              as.POSIXct(time) - timer@startTime
            timer@startTime <- as.POSIXlt(NA)
            timer})

setMethod("resume",c("Timer","POSIXt"), function(timer,time) start(timer,time))

setMethod("isRunning","Timer",
          function(timer)
            !is.na(timer@startTime))

setMethod("totalTime","Timer", function(timer) timer@totalTime)

setMethod("timeSoFar",c("Timer","POSIXt"),
          function(timer,time) {
            if (isRunning(timer)) {
              timer@totalTime + as.POSIXlt(time) - timer@startTime
            } else {
              timer@totalTime
            }})
setMethod("timeSoFar<-",c("Timer","POSIXt"),
          function(timer,time,value) {
            if (isRunning(timer)) {
              timer@startTime <- as.POSIXlt(time)
            }
            timer@totalTime <- value
            timer})


setMethod("reset","Timer", function(timer) {
  timer@startTime <- as.POSIXlt(NA)
  timer@totalTime <- as.difftime(0,units="secs")
  timer
})

###
## Serialization.  Note timers are not stored directly in database,
## so they don't need IDs
setMethod("as.jlist",c("Timer","list"), function(obj,ml, serialize=TRUE) {
  ml$class <- NULL
  ## Additional work
  ml$name <- unbox(ml$name)
  ml$startTime <- unboxer(ml$startTime)
  ml$totalTime <- list(time=unboxer(as.numeric(ml$totalTime)),
                       units=unboxer(units(ml$totalTime)))

  ml
  })

parseTimer <- function (rec) {
  if (is.null(rec$totalTime)) {
    tt <- as.difftime(NA_real_,units="secs")
  } else {
    tim <- as.numeric(rec$totalTime[[pmatch("tim",names(rec$totalTime))]])
    units <- as.character(rec$totalTime$units)
    tt <- as.difftime(tim,units=units)
  }
  if (is.null(rec$startTime) || is.na(rec$startTime) ||
      (is.character(rec$startTime) && rec$startTime=="NA")) {
    st <- as.POSIXlt(NA)
  } else {
    if (is.list(rec$startTime) && !is.null(rec$startTime[['$date']])) {
      st <- jsonlite:::parse_date(rec$startTime[['$date']])
    } else {
      st <- as.POSIXlt(ununboxer(rec$startTime))
    }
  }
  new("Timer",name=ununboxer(rec$name),
      startTime=st, totalTime=tt)
}


############################################################
### Status -- Maintains state for one user in one context.

setClass("Status",
         slots=c("_id"="character",
                 app="character",
                 uid="character",
                 context="character",
                 oldContext="character",
                 timers="list",
                 flags="list",
                 observables="list",
                 timestamp="POSIXt"))
Status <- function (uid,context,timerNames=character(),
                    flags=list(),observables=list(),timestamp=Sys.time(),
                    app="default") {
  timers <- sapply(timerNames, Timer)
  new("Status",app=app,uid=uid,context=context,oldContext=context,
      timers=timers, flags=flags,observables=observables,timestamp=timestamp,
      "_id"=c(oid=NA_character_))
}

setMethod("uid","Status", function(x) x@uid)
setMethod("context","Status", function(x) x@context)
setMethod("context<-","Status", function(x,value){
  x@context <- value
  x
})
setMethod("oldContext","Status", function(x) x@oldContext)
setMethod("app","Status", function(x) x@app)
setMethod("timestamp","Status", function(x) x@timestamp)

setMethod("timer","Status", function(x,name) x@timers[[name]])
setMethod("timer<-","Status", function(x,name,value) {
  if (!is.null(value) && !is(value,"Timer")) {
    stop("Attempting to set timer ",name," to non-timer object")
  }
  x@timers[[name]] <- value
  x
})

setMethod("setTimer",c("Status","character"),
          function(x,timerID,time,running,now) {
  fieldlist <- splitfield(timerID)
  if (fieldlist[1] != "state" || fieldlist[2] !="timers")
    stop ("Can only !start and !reset timers:  ",timerID)
  name <- fieldlist[3]
  if (is.null(timer(x,name))) {
    timer(x,name) <- Timer(name)
  }
  timerTime(x,name,now) <- time
  timerRunning(x,name,now) <- running
  x
})



setMethod("timerTime",c("Status","character"), function(x,name,now) {
  if (is.null(x@timers[[name]])) {
    stop("Did not find a timer named ",name)
  }
  timeSoFar(x@timers[[name]],now)
})
setMethod("timerTime<-", c("Status","character"), function(x,name,now,value){
  timeSoFar(timer(x,name),now)<-value
  x})
setMethod("timerRunning", c("Status","character"), function(x,name,now) {
  isRunning(timer(x,name))
})
setMethod("timerRunning<-", c("Status","character"), function(x,name,now,value) {
  if (value == TRUE) {
    timer(x,name) <- start(timer(x,name),now,FALSE)
  } else if (value == FALSE) {
    timer(x,name) <- pause(timer(x,name),now,FALSE)
  } else {
    stop ("Trying to set running state of timer ",name," to illogical value.")
  }
  x
})


setMethod("flag","Status", function(x,name) x@flags[[name]])
setMethod("flag<-","Status", function(x,name,value) {
  x@flags[[name]] <- value
  x
})

setMethod("obs","Status", function(x,name) x@observables[[name]])
setMethod("obs<-","Status", function(x,name,value) {
  x@observables[[name]] <- value
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

## !set for timers is treated specially.
## !set: {<timer>: <time>}  | {<timer>.time: <time>}
## !set: {<timer>: <true/false>} | {<timer>.running: <true/false>}

getJS <- function (field,state,event) {
  fieldexp <- splitfield(field)
  switch(fieldexp[1],
         state=
           switch(fieldexp[2],
                  context=context(state),
                  oldContext=oldContext(state),
                  flags=getJSfield(flag(state,fieldexp[3]),fieldexp[-(1:3)]),
                  observables=getJSfield(obs(state,fieldexp[3]),
                                         fieldexp[-(1:3)]),
                  timers={
                    if (length(fieldexp) == 3L) {
                      timerTime(state,fieldexp[3],
                                timestamp(event))
                    } else {
                      switch(fieldexp[4],
                             time=, value=
                                      timerTime(state,fieldexp[3],
                                                timestamp(event)),
                             run=, running=
                                      timerRunning(state,fieldexp[3],
                                                   timestamp(event)),
                             stop("Timer ",fieldexp[4],
                                  "only .time and .running allowed."))
                    }
                  },
                  uid=uid(state),
                  timestamp=timestamp(state),
                  stop("Unrecognized field ",field)
                  ),
         event=
           switch(fieldexp[2],
                  data=getJSfield(details(event),fieldexp[-(1:2)]),
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

setJS <- function (field,state,now,value) {
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
  } else if (fieldexp[2]=="timers") {
    if (length(fieldexp)<3L)
      stop("No timer name supplied:", field)
    if (is.null(timer(state,fieldexp[3])))
      stop("Timer ",fieldexp[3]," does not exist.")
    if (length(fieldexp) == 3L) {
      if (is.logical(value))
        timerRunning(state,fieldexp[3], now) <-value
      else if (is.difftime(value))
        timerTime(state,fieldexp[3],now) <- value
      else
        stop ("Timer ",fieldexp[3],"set to a value that is not a time or logical.")
    } else {
      switch(fieldexp[4],
             time=, value=
                      timerTime(state,fieldexp[3], now) <-
                      asif.difftime(value),
             run=, running=
                     timerRunning(state,fieldexp[3], now) <-value,
             stop("Timer ",fieldexp[4],
                  "only .time and .running allowed."))
    }
  } else {
    stop("Unrecognized field ",field)
  }
  state
}

removeJS <- function (field,state) {
  fieldexp <- splitfield(field)
  if (fieldexp[1] != "state")
    stop("Only fields of the state object can be set",field)
  if (fieldexp[2]=="flags") {
    if (length(fieldexp)<3L)
      stop("No flag name supplied:", field)
    flag(state,fieldexp[3]) <-
      removeJSfield(flag(state,fieldexp[3]), fieldexp[-(1:3)])
  } else if (fieldexp[2]=="observables") {
    if (length(fieldexp)<3L)
      stop("No observable name supplied:", field)
    obs(state,fieldexp[3]) <-
      removeJSfield(obs(state,fieldexp[3]), fieldexp[-(1:3)])
  } else if (fieldexp[2]=="timers") {
    if (length(fieldexp)<3L)
      stop("No timer name supplied:", field)
    if (length(fieldexp) == 3L) {
      timer(state,fieldexp[3]) <- NULL
    } else {
      stop("Timer subfields can't be removed.")
    }
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
  if (length(fieldlist)==0L) return (value)
  if (length(fieldlist)==1L) {
    field <- fieldlist
    if (!is.na(strtoi(field))) field <- strtoi(field)
    target[[field]] <- value
  } else {
    field <- fieldlist[1]
    if (!is.na(strtoi(field))) field <- strtoi(field)
    target[[field]] <- setJSfield(target[[field]],fieldlist[-1],value)
  }
  target
}

removeJSfield <- function (target,fieldlist) {
  if (length(fieldlist)==0L) return(NULL)
  if (length(fieldlist)==1L) {
    if (is.list(target)) {
      target[fieldlist] <- NULL
    } else if (length(target) > 0L) {
      ## Do I need something here for numeric "names"
      target <- target[names(target)!=fieldlist]
    } else {
      NULL
    }
  } else {
    target[[fieldlist[1]]] <-
      removeJSfield(fieldlist[-1],target[[fieldlist[1]]])
  }
  target
}


setMethod("as.jlist",c("Status","list"), function(obj,ml,serialize=TRUE) {
  ml$"_id" <- NULL
  ml$class <-NULL

  ml$uid <- unboxer(ml$uid)
  ml$context <- unboxer(ml$context)
  ml$timestamp <- unboxer(ml$timestamp)

  ml$timers <- unboxer(lapply(ml$timers,
                              function(tim) as.jlist(tim,attributes(tim))))
  ml$flags <- unparseData(ml$flags,serialize)
  ml$observables <- unparseData(ml$observables,serialize)

  ml
})

parseStatus<- function (rec) {
  if (is.null(rec$"_id")) rec$"_id" <- NA_character_
  names(rec$"_id") <- "oid"
  if (is.null(rec$oldContext)) rec$oldContext <- rec$context
  if (is.null(rec$app)) rec$app <- "default"
  if (is.null(rec$timestamp)) rec$timestamp <- Sys.time()
  new("Status","_id"=rec$"_id",
      uid=as.vector(rec$uid),context=as.vector(rec$context),
      timers=lapply(ununboxer(rec$timers), parseTimer),
      flags=parseData(ununboxer(rec$flags)),
      observables=parseData(ununboxer(rec$observables)),
      timestamp=as.POSIXlt(ununboxer(rec$timestamp)),
      app=as.vector(rec$app),oldContext=as.vector(rec$oldContext))
}

##We need an all.equal method as we need to suppress checking names on
##parts of the data fields which might be different.
all.equal.Status <- function (target, current, ...) {
  if (!is(current,"Status"))
    return(paste("Target is 'Status' and current is '",class(current),"'."))
  msg <- character()
  if ((is.na(target@"_id") && !is.na(current@"_id")) ||
      (!is.na(target@"_id") && !isTRUE(target@"_id" ==  current@"_id")))
    msg <- c(msg,"Database IDs do not match.")
  if (app(target) != app(current))
    msg <- c(msg,"Application IDs do not match.")
  if (uid(target) != uid(current))
    msg <- c(msg,"User IDs do not match.")
  if (context(target) != context(current))
    msg <- c(msg,"Contexts do not match.")
  if (oldContext(target) != oldContext(current))
    msg <- c(msg,"Contexts do not match.")
  ## Check Flags
  fnamet <- names(target@flags)
  fnamec <- names(current@flags)
  if (length(target@flags) != length(current@flags) ||
      !setequal(fnamet,fnamec)) {
    msg <- c(msg,"Names or number of flags differ.")
    if (length(setdiff(fnamet,fnamec)) > 0L)
      msg <- c(msg,paste("Flags in target but not in current:",
                         setdiff(fnamet,fnamec)))
    if (length(setdiff(fnamec,fnamet)) > 0L)
      msg <- c(msg,paste("Flags in current but not in target:",
                         setdiff(fnamec,fnamet)))
  }
  msgf <- all.equal(target@flags,current@flags,...,
                    check.attributes=FALSE)
  if (!isTRUE(msgf)) msg <- c(msg,msgf)
  ## Check Observables
  onamet <- names(target@observables)
  onamec <- names(current@observables)
  if (length(target@observables) != length(current@observables) ||
      !setequal(onamet,onamec)) {
    msg <- c(msg,"Names or number of observables differ.")
    if (length(setdiff(onamet,onamec)) > 0L)
      msg <- c(msg,paste("Observables in target but not in current:",
                         setdiff(onamet,onamec)))
    if (length(setdiff(onamec,onamet)) > 0L)
      msg <- c(msg,paste("Observables in current but not in target:",
                         setdiff(onamec,onamet)))
  }
  msgo <- all.equal(target@observables,current@observables,...,
                    check.attributes=FALSE)
  if (!isTRUE(msgo)) msg <- c(msg,msgo)
  ## Check timers
  tnamet <- names(target@timers)
  tnamec <- names(current@timers)
  if (length(target@timers) != length(current@timers) ||
      !setequal(tnamet,tnamec)) {
    msg <- c(msg,"Names or number of timers differ.")
    if (length(setdiff(tnamet,tnamec)) > 0L)
      msg <- c(msg,paste("Timers in target but not in current:",
                         setdiff(tnamet,tnamec)))
    if (length(setdiff(tnamec,tnamet)) > 0L)
      msg <- c(msg,paste("Timers in current but not in target:",
                         setdiff(tnamec,tnamet)))
  }
  msgt <- all.equal(target@timers,current@timers,...,
                    check.attributes=FALSE)
  if (!isTRUE(msgt)) msg <- c(msg,msgt)
  ## Timestamp
  msgt <- all.equal(timestamp(target),timestamp(current),...)
  if (!isTRUE(msgt)) msg <- c(msg,msgt)
    ## Return true if message list is empty.
  if (length(msg)==0L) TRUE
  else msg
}

##############################################################
## User Records (collection of status objects.

UserRecordSet <-
  setRefClass("UserRecordSet",
              fields=c(app="character",
                       dbname="character",
                       dburi="character",
                       db="MongoDB"),
              methods = list(
                  initialize =
                    function(app="default",dbname="EIRecords",
                             dburi="mongo://localhost:271017",
                             db = NULL,
                             ...)
                      callSuper(app=app,db=db,dbname=dbname,dburi=dburi,...)
              ))


## Student Record Methods
UserRecordSet$methods(
                  recorddb = function () {
                    if (is.null(db)) {
                      db <<- mongo("States",dbname,dburi)
                    }
                    db
                  },
             getStatus = function (uid) {
               getOneRec(buildJQuery(app=app,uid=uid),recorddb(),
                         parseStatus)
             },
             saveStatus = function (state) {
               saveRec(state,recorddb())
             },
             newStudent = function (uid) {
               rec <- getStatus(uid)
               if (!is.null(rec)) {
                 flog.debug("Found existing student record for  %s", uid)
                 return (rec)
               }
               rec <- getStatus("*DEFAULT*")
               if(!is.null(rec)) {
                 flog.debug("Found default student record for  %s", uid)
                 rec@uid <- uid
                 saveRec(rec,recorddb())
                 return(rec)
               }
               flog.debug("Making blank student record for  %s", uid)
               rec <- Status(uid=uid,context="*INITIAL*",app=app)
               rec <- saveRec(rec,recorddb())
               rec
             })


