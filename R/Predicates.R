
executePredicate <- function (predicate, state, event) {
  for (op in names(predicate)) {
    state <- do.call(op,predicate[[op]],state,event)
  }
  return (state)
}

"!set" <- function (predicate, state, event) {
  for (name in names(predicate)) {
    value <- predicate[[name]]
    if (is.character(value) &&
        grepl("^(state|event)\\.",value) )
      value <- getJS(value,state,event)
    state <- setJS(name,state,asif.difftime(value))
  }
  state
}

## The \texttt{!unset} operator is an inverse.  For this the
##   \textit{value} should be one of \texttt{"NULL"}, \texttt{"NA"}, or
##   \texttt{"Delete"}.  The first two set the values to NULL and NA (not
##   applicable) respectively.  The rules for these values are
##   implementation dependent.  The third removes the flag or observable
##   from the state object.

"!Unset" <- function (predicate, state, event) {}

modify <- function (predicate, state, event, op) {
  for (name in names(predicate)) {
    curr <- getJS(name,state,event)
    value <- predicate[[name]]
    if (is.character(value) &&
        grepl("^(state|event)\\.",value) )
      value <- asif.difftime(getJS(value,state,event))
    result <- do.call(op,list(curr,value))
    state <- setJS(name,state,result)
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
        grepl("^(state|event)\\.",value) )
      value <- getJS(value,state,event)
    value <- as.integer(value)
    state <- setJS(name,state,curr[-(1:value)])
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


"!start" <- function (predicate, state, event) {



}
"!pause" <- function (predicate, state, event) {}
"!resume" <- function (predicate, state, event) {}

"!setCall" <- function (predicate, state, event) {
  for (name in names(predicate)) {
    fun <- predicate[[name]]
    result <- do.call(fun,list(name,state,event))
    state <- setJS(name,state,result)
  }
  state
}


## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## difftime arithmatic.
## Difftime could be described as a list.  So we need a special method for that.
## This function takes a list all of whose components are
asif.difftime <- function (e2) {
  if (is.list(e2)) {
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


