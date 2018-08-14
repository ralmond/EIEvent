###  Events -- Descriptions of state transitions
###  This roughy follow a combination of the xAPI format.

setClass("Event",
         slots=c(app="character",       #Application ID
                 uid="character",       #User (student) ID
                 verb="character",      #Action Identifier
                 object="string",       #Direct Object ID
                 context="string",      #If context is calculated by PP
                 timestamp="POSIXt",      #When action took place.
                 details="list"))        #More details.
setMethod("app","Event", function(x) x@app)
setMethod("uid","Event", function(x) x@uid)
setMethod("verb","Event", function(x) x@verb)
setMethod("object","Event", function(x) x@object)
setMethod("context","Event", function(x) x@context)
setMethod("timestamp","Event", function(x) x@timestamp)
setMethod("details","Event", function(x) x@details)

Event <- function(uid,verb,object="",timestamp=Sys.time(),
                  details=list(),app="default",context="") {
  new("Event",app=app,uid=uid,verb=verb,object=object,
      timestamp=timetimestamp,context=context,details=details)
}


