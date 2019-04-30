###  Events -- Descriptions of state transitions
###  This roughy follow a combination of the xAPI format.

setClass("Event",
         slots=c(verb="character",      #Action Identifier
                 object="character"),       #Direct Object ID
         contains="P4Message")


setGeneric("verb",function(x) standardGeneric("verb"))
setGeneric("object",function(x) standardGeneric("object"))

setMethod("verb","Event", function(x) x@verb)
setMethod("object","Event", function(x) x@object)


Event <- function(uid,verb,object="",timestamp=Sys.time(),
                  details=list(),app="default",context="",
                  processed=FALSE) {
  new("Event",app=app,uid=uid,verb=verb,object=object,
      timestamp=timestamp,context=context,data=details,
      "_id"=c(oid=NA_character_),processed=processed)
}

setMethod("toString","Event", function(x, ...) {
  paste('Event:{ uid:',x@uid,', context:',x@context,
        ', (',x@verb,",",x@object,')}')
})
setMethod("show","Event",function(object) {
  cat(toString(object),"\n")
})

all.equal.Event <- function (target, current, ...,checkTimestamp=FALSE,check_ids=TRUE) {
  if (!is(current,"Event"))
    return(paste("Target is 'Event' and current is '",class(current),"'."))
  msg <- all.equal.P4Message(target,current,...,checkTimestamp=checkTimestamp,
                             check_ids=check_ids)
  if (isTRUE(msg)) msg <- character()
  if (verb(target) != verb(current))
    msg <- c(msg,"Verbs do not match.")
  if (object(target) != object(current))
    msg <- c(msg,"Objects do not match.")
    ## Return true if message list is empty.
  if (length(msg)==0L) TRUE
  else msg
}



parseEvent<- function (rec) {
  rec <- cleanMessageJlist(rec)
  if (is.null(rec$verb) || length(rec$verb)==0L) rec$verb <- ""
  if (is.null(rec$object) || length(rec$object)==0L) rec$object <- ""
  new("Event","_id"=ununboxer(rec$"_id"),
      app=as.vector(rec$app),
      uid=as.vector(rec$uid),
      context=as.vector(rec$context),
      sender=as.vector(ununboxer(rec$sender)),
      mess=as.vector(ununboxer(rec$mess)),
      verb=as.vector(rec$verb),
      object=as.vector(rec$object),
      timestamp=as.POSIXlt(ununboxer(rec$timestamp)),
      processed=ununboxer(rec$processed),pError=rec$pError,
      data=parseData(ununboxer(rec$data)))
}

setMethod("as.jlist",c("Event","list"), function(obj,ml,serialize=TRUE) {
  ## Call Next Method
  as.p4jlist <- getMethod("as.jlist",c("P4Message","list"))
  ml <- do.call(as.p4jlist,list(obj,ml,serialize))
  ## Additional work
  ml$verb <- unbox(ml$verb)
  ml$object <- unbox(ml$object)

  ml
  })
