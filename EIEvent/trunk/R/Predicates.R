
executePredicate <- function (predicate, state, event) {
  for (i in 1L:length(predicate)) {
    ## Need to do this by number as names might not be unique.
    state <- do.call(names(predicate)[[i]],list(predicate[[i]],state,event))
  }
  return (state)
}

buildMessages <- function (predicate, state, event) {
  ## These should all be !send, but might be something else.
  lapply(names(predicate),
         function (op) do.call(op,list(predicate[[op]],state,event)))
}


"!set" <- function (predicate, state, event) {
  for (name in names(predicate)) {
    value <- predicate[[name]]
    if (is.character(value) &&
        grepl("^(state|event)\\.",value) )
      value <- getJS(value,state,event)
    state <- setJS(name,state,timestamp(event),asif.difftime(value))
  }
  state
}

## The \texttt{!unset} operator is an inverse.  For this the
##   \textit{value} should be one of \texttt{"NULL"}, \texttt{"NA"}, or
##   \texttt{"Delete"}.  The first two set the values to NULL and NA (not
##   applicable) respectively.  The rules for these values are
##   implementation dependent.  The third removes the flag or observable
##   from the state object.

"!unset" <- function (predicate, state, event) {
  for (name in names(predicate)) {
    value <- predicate[[name]]
    if (is.character(value) &&
        grepl("^(state|event)\\.",value) )
      value <- asif.difftime(getJS(value,state,event))
    if (is.null(value) || toupper(value)=="NULL")
      state <- setJS(name,state,timestamp(event),NULL)
    else if (is.na(value) || toupper(value)=="NA")
      state <- setJS(name,state,timestamp(event),NA)
    else
      state <- removeJS(name,state)
  }
  state
}

modify <- function (predicate, state, event, op) {
  for (name in names(predicate)) {
    curr <- getJS(name,state,event)
    value <- predicate[[name]]
    if (is.character(value) &&
        grepl("^(state|event)\\.",value) )
      value <- asif.difftime(getJS(value,state,event))
    result <- do.call(op,list(curr,value))
    state <- setJS(name,state,timestamp(event),result)
  }
  state
}


"!incr" <- function (predicate, state, event)
  modify(predicate,state,event,"+")
"!decr" <- function (predicate, state, event)
  modify(predicate,state,event,"-")
"!mult" <- function (predicate, state, event)
  modify(predicate,state,event,"*")
"!div" <- function (predicate, state, event)
  modify(predicate,state,event,"/")
"!min" <- function (predicate, state, event)
  modify(predicate,state,event,"min")
"!max" <- function (predicate, state, event)
  modify(predicate,state,event,"max")

"!addToSet" <- function (predicate, state, event)
  modify(predicate,state,event,
         function(curr,val){
           if (val %in% curr)
             curr
           else
             c(curr,val)
         })

"!pullFromSet" <- function (predicate, state, event)
  modify(predicate,state,event,setdiff)
"!push" <- function (predicate, state, event)
  modify(predicate,state,event,
         function (curr,val) c(val,curr))
"!pop" <- function (predicate, state, event) {
  for (name in names(predicate)) {
    curr <- getJS(name,state,event)
    value <- predicate[[name]]
    if (is.character(value) &&
        grepl("^state\\.",value) ) {
      v1 <- getJS(paste(name,'[1]',sep=""),state,event)
      state <- setJS(value,state,timestamp(event),v1)
      value <- 1
    }
    value <- as.integer(value)
    state <- setJS(name,state,timestamp(event),curr[-(1:value)])
  }
  state
}

  ## Timers are treated specially by the \texttt{!set} operator.  In
  ## particular, there are two fields of the timer object which can be
  ## set, \texttt{state.timers.\textit{name}.value} and
  ## \texttt{state.timers.\textit{name}.running}.  The latter value can
  ## be set to \texttt{true} or \texttt{false} which causes it to resume
  ## or pause respectively.  The \texttt{.value} field of the timer sets
  ## the current time; this is assumed if the field is omitted.  This can
  ## be treated like a numeric value with the time in seconds, or it can
  ## be set as an object of the form \texttt{\{time:\textit{number},
  ##   units:\textit{unit}\}}, where \textit{unit} is one of
  ## \texttt{"sec"}, \texttt{"min"}, \texttt{"hr"}, \texttt{"day"} and so
  ## forth.  Example~\ref{json:time} gives a few examples:


## !start & !reset are treated specially.  A number of cases:
## !start: <timer>
## !start: {<timer>: <time>}
## !start: {<timer>: <true/false>}
## !start: {<timer>: {time:<time>, running:<true/false>}}


"!start" <- function (predicate, state, event) {
  if (is.character(predicate)) {
    for (name in predicate) {
      state <- setTimer(state,name,asif.difftime(0),TRUE,timestamp(event))
    }
  } else if (is.list(predicate)) {
    for (name in names(predicate)) {
      running <- TRUE
      value <- asif.difftime(0)
      val <- asif.difftime(predicate[[name]])
      if (is.character(val) && grepl("^(state|event)\\.",value))
        val <- asif.difftime(getJS(val,state,event))
      if (is.logical(val)) running <- val
      else if (is.difftime(val)) value <- val
      else if (is.list(val)) {
        if (is.null(names(val)))
          stop("Unrecognized value for timer ",name,":",predicate[[name]])
        for (field in names(val)) {
          switch (field,
                  value=, time= {value <- as.difftime(val[[field]])},
                  run=, running= {running<-as.logical(val[[field]])},
                  stop("Unrecognized value for timer ",name,":",
                       predicate[[name]]))
        }
      } else
        stop("Unrecognized value for timer ",name,":",predicate[[name]])
      state <- setTimer(state,name,value,running,timestamp(event))
    }
  } else {
    stop("Unknown timer ",predicate)
  }
  state
}

"!reset" <- function (predicate, state, event) {
  if (is.character(predicate)) {
    for (name in predicate) {
      state <- setTimer(state,name,asif.difftime(0),FALSE,timestamp(event))
    }
  } else if (is.list(predicate)) {
    for (name in names(predicate)) {
      running <- FALSE
      value <- asif.difftime(0)
      val <- asif.difftime(predicate[[name]])
      if (is.character(val) && grepl("^(state|event)\\.",value))
        val <- asif.difftime(getJS(val,state,event))
      if (is.logical(val)) running <- val
      else if (is.difftime(val)) value <- val
      else if (is.list(val)) {
        if (is.null(names(val)))
          stop("Unrecognized value for timer ",name,":",predicate[[name]])
        for (field in names(val)) {
          switch (field,
                  value=, time= {value <- as.difftime(val[[field]])},
                  run=, running= {running<-as.logical(val[[field]])},
                  stop("Unrecognized value for timer ",name,":",
                       predicate[[name]]))
        }
      } else
        stop("Unrecognized value for timer ",name,":",predicate[[name]])
      state <- setTimer(state,name,value,running,timestamp(event))
    }
  } else {
    stop("Unknown timer ",predicate)
  }
  state
}

"!setCall" <- function (predicate, state, event) {
  for (name in names(predicate)) {
    fun <- predicate[[name]]
    result <- do.call(fun,list(name,state,event))
    state <- setJS(name,state,timestamp(event),result)
  }
  state
}

#####################################################
## !send

## Frequently used idiom.
getJSval <- function(val,state,event,default) {
  if (is.character(val) && grepl("^(state|event)\\.",val)) {
    val <- getJS(val,state,event)
  }
  if (is.null(val)) {
    val <- default
  }
  val
}

## The "!send" predicate is slightly different as it returns a
## P4Message rather than a new state.
"!send" <- function (predicate, state, event) {
  app <- app(event)
  uid <- uid(state)
  context <- getJSval(predicate$context,state,event,oldContext(state))
  mess <-
    getJSval(predicate$mess,state,event,"Observables Available")
  timestamp <- timestamp(event)
  if (is.null(predicate$data) || length(predicate$data)==0L) {
    details <- state@observables
  } else {
    details <- lapply(predicate$data,
                      function (val) getJSval(val,state,event,NULL))
    names(details) <- names(predicate$data)
  }
  P4Message(uid,context,sender="EIEvent",mess,timestamp,details,app)
}

"!send1" <- function (predicate, state, event) {"!send"(predicate,state,event)}
"!send2" <- function (predicate, state, event) {"!send"(predicate,state,event)}


## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## difftime arithmatic.
## Difftime could be described as a list.  So we need a special method for that.
## This function takes a list all of whose components are
asif.difftime <- function (e2) {
  if (is.list(e2) || is.numeric(e2)) {
    units <- names(e2)
    if (!is.null(units) &&
        all(units %in% c("secs","mins","hours","days","weeks"))) {
      e2 <-do.call("+",
                   lapply(units,
                          function (u) as.difftime(e2[[u]],units=u)))
    }
  }
  e2
}

is.difftime <- function(x) {is(x,"difftime")}

