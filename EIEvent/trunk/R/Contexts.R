###
## This defines a context object

setClass("Context",
         slots=c("_id"="character",    #Mongo ID
                 cid="character",       #Context ID
                name="character",      #Longer name
                number="integer",       #Context #
                belongsTo="character",  #Contexts to which this belongs
                doc="character",
                app="character"))
Context <- function (cid,name,number,belongsTo=character(),
                     doc="", app="default") {
  new("Context","_id"=c(oid=NA_character_),cid=cid,
      name=name,number=as.integer(number),
      belongsTo=belongsTo,doc=doc, app=app)
}
setGeneric("cid", function(c) standardGeneric("cid"))
setGeneric("belongsTo", function(c) standardGeneric("belongsTo"))
setGeneric("belongsTo<-", function(c,value) standardGeneric("belongsTo<-"))
setGeneric("number", function(c) standardGeneric("number"))
setGeneric("number<-", function(c,value) standardGeneric("number<-"))

setMethod("cid","Context", function(c) c@cid)
setMethod("belongsTo","Context", function(c) c@belongsTo)
setMethod("belongsTo<-","Context", function(c,value) {
  c@belongsTo <- value
  c})
setMethod("number","Context", function(c) c@number)
setMethod("number<-","Context", function(c,value) {
  c@number <- as.integer(value)
  c})

setMethod("name","Context", function(x) x@name)
setMethod("doc","Context", function(x) x@doc)
setMethod("app","Context", function(x) x@app)


applicableContexts <- function (c) {
  if (length(belongsTo(c))==0L) c(cid(c),"ALL")
  c(cid(c), belongsTo(c), "ALL")
}

parseContext <- function(rec) {
  if (is.null(rec$"_id")) rec$"_id" <- NA_character_
  names(rec$"_id") <- "oid"
  new("Context","_id"=rec$"_id",
      cid=as.vector(rec$cid), name=as.vector(rec$name),
      number=as.integer(rec$number),belongsTo=as.vector(rec$belongsTo),
      doc=as.vector(rec$doc), app=as.vector(rec$app))
}

setMethod("as.jlist",c("Context","list"), function(obj,ml, serialize=TRUE) {
  ml$"_id" <- NULL
  ml$class <-NULL
  ## Use manual unboxing for finer control.
  ml$cid <- unboxer(ml$cid)
  ml$name <- unboxer(ml$name)
  ml$number <- unboxer(ml$number)
  ml$belongsTo <- unboxer(ml$belongsTo)
  ml$doc <- unboxer(ml$doc)
  ml$app <- unboxer(ml$app)
  ml
  })

###########################################################
## Context Set

ContextSet <-
  setRefClass("ContextSet",
              fields=c(app="character",
                       dbname="character",
                       dburi="character",
                       db="MongoDB"),
              methods = list(
                  initialize =
                    function(app="default",
                             dbname="EIRecrods",
                             dburi="mongodb://localhost",
                             db = NULL, #mongo("Contexts",dbname,dburi)
                             ...) {
                      callSuper(app=app,db=db,dbname=dbname,dburi=dburi,...)
                    },
                  numbered = function (num) {
                    con <- getOneRec(buildJQuery(app=app,number=num),db,
                                     parseContext)
                    if (is.null(con)) {
                      flog.warn("No context found for number %d",num)
                    } else {
                      flog.debug("Found context %s",name(con))
                    }
                    con
                  },
                  named = function (named) {
                    con <- getOneRec(buildJQuery(app=app,name=named),db,
                                     parseContext)
                    if (is.null(con)) {
                      flog.warn("No context found for %s",name)
                    } else {
                      flog.debug("Found context %s",name(con))
                    }
                    con
                  },
                  update = function (con) {
                    if (!is(con,"Context"))
                      stop("Argument to ContextSet$update must be a context.")
                    flog.debug("Updating context %s",name(con))
                    saveRec(con,db)
                  },
                  contextdb = function () {
                    if (is.null(db)) {
                      db <<- mongo("Contexts",dbname,dburi)
                    }
                    db
                  }
              ))



setGeneric("matchContext",function(con,set) standardGeneric("matchContext"))
setMethod("matchContext",c("integer","list"),
          function(con,set) set[[con]])
setMethod("matchContext",c("character","list"),
          function(con,set) set[[con]])
setMethod("matchContext",c("integer","list"),
          function(con,set) set$numbered(con))
setMethod("matchContext",c("character","list"),
          function(con,set) set$named(con))
