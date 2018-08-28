###  Events -- Descriptions of state transitions
###  This roughy follow a combination of the xAPI format.

setClass("Event",
         slots=c(verb="character",      #Action Identifier
                 object="string"),       #Direct Object ID
         contains="P4Message")


setGeneric("verb",function(x) standardGeneric("verb"))
setGeneric("object",function(x) standardGeneric("object"))

setMethod("verb","Event", function(x) x@verb)
setMethod("object","Event", function(x) x@object)


Event <- function(uid,verb,object="",timestamp=Sys.time(),
                  details=list(),app="default",context="") {
  new("Event",app=app,uid=uid,verb=verb,object=object,
      timestamp=timetimestamp,context=context,data=details)
}

setMethod("toString","Event", function(x, ...) {
  paste('Event:{ uid:',x@uid,', context:',x@context,
        ', (',x@verb,x@object')}')
})
setMethod("show","Event",function(object) {
  cat(toString(object),"\n")
})

setMethod("as.jlist","Event", function(obj,ml) {
  evl <- callNextMethod()
  evl$verb <- unbox(evl$verb)
  evl$object <- unbox(evl$object)
  evl
}

parseEvent<- function (rec) {
  new("Event","_id"=rec$"_id", app=rec$app, uid=rec$uid,
      context=rec$context,verb=rec$verb,object=rec$object,
      timestamp=rec$timestamp,
      data=parseData(rec$data))
}

