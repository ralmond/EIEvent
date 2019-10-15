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
setMethod("belongsTo","ANY", function(c) {
  character()
  })

setMethod("number","Context", function(c) c@number)
setMethod("number<-","Context", function(c,value) {
  c@number <- as.integer(value)
  c})

setMethod("name","Context", function(x) x@name)
setMethod("doc","Context", function(x) x@doc)
setMethod("app","Context", function(x) x@app)

setMethod("toString","Context", function(x, ...) {
  paste('#<Context (',x@number,'): ',x@cid,'>')
})
setMethod("show","Context",function(object) {
  cat(toString(object),"\n")
})


applicableContexts <- function (c) {
  if (length(belongsTo(c))==0L) c(cid(c),"ALL", "ANY")
  c(cid(c), belongsTo(c), "ALL", "ANY")
}

parseContext <- function(rec) {
  if (is.null(rec$"_id")) rec$"_id" <- NA_character_
  names(rec$"_id") <- "oid"
  new("Context","_id"=rec$"_id",
      cid=as.vector(rec$cid), name=as.vector(rec$name),
      number=as.integer(rec$number),belongsTo=as.character(rec$belongsTo),
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
                    con <- getOneRec(buildJQuery(app=app,number=num),
                                     contextdb(), parseContext)
                    if (is.null(con)) {
                      flog.debug("No context found for number %d",num)
                    } else {
                      flog.debug("Found context %s",name(con))
                    }
                    con
                  },
                  named = function (id) {
                    names(id) <- NULL   #Interpreted as an operator
                    con <- getOneRec(buildJQuery(app=app,name=id),
                                     contextdb(), parseContext)
                    if (is.null(con)) {
                      flog.debug("No context found for %s",id)
                    } else {
                      flog.debug("Found context %s",name(con))
                    }
                    con
                  },
                  withID = function (id) {
                    names(id) <- NULL   #Interpreted as an operator
                    con <- getOneRec(buildJQuery(app=app,cid=id),
                                     contextdb(), parseContext)
                    if (is.null(con)) {
                      flog.debug("No context found for %s",id)
                    } else {
                      flog.debug("Found context %s",name(con))
                    }
                    con
                  },
                  update = function (con) {
                    if (!is(con,"Context"))
                      stop("Argument to ContextSet$update must be a context.")
                    flog.debug("Updating context %s",name(con))
                    saveRec(con,contextdb())
                  },
                  contextdb = function () {
                    if (is.null(db)) {
                      db <<- mongo("Contexts",dbname,dburi)
                    }
                    db
                  },
                  clearAll = function () {
                    flog.info("Clearing Context database for %s",app)
                    contextdb()$remove(buildJQuery(app=app))
                  }
              ))



setGeneric("matchContext",function(id,set) standardGeneric("matchContext"))
setMethod("matchContext",c("numeric","list"),
          function(id,set) set[[id]])
setMethod("matchContext",c("character","list"),
          function(id,set) set[[id]])
setMethod("matchContext",c("numeric","ContextSet"),
          function(id,set) set$numbered(id))
setMethod("matchContext",c("character","ContextSet"),
          function(id,set) set$named(id))

setGeneric("updateContext", function(con,set)
  standardGeneric("updateContext"))

setMethod("updateContext",c("Context","list"), function (con, set) {
  set[[number(con)]] <- con
  if (is.null(names(set))) {
    names(set) <- rep("",length(set))
  }
  names(set)[number(con)] <- cid(con)
  set
})

setMethod("updateContext",c("Context","ContextSet"), function (con, set) {
  cidc <- set$named(cid(con))
  nc <- set$numbered(number(con))
  if (!is.null(cidc)) {
    con@'_id' <- cidc@'_id'
    flog.info("Replacing exisiting context named %s",name(cidc))
  }
  if (!is.null(nc)) {
    if (is.na(con@'_id')) {
      flog.info("Replacing existing context %d named %s",
                number(nc), name(nc))
      con@'_id' <- nc@'_id'
    } else {
      flog.info("Already a context numbered %d named %s; skipping.",
                number(nc), name(nc))
      return(invisible(set))
    }
  }
  set$update(con)
  set
})

loadContexts <- function (conmat, set, app) {
  N <- nrow(conmat)
  metadataI <- which(tolower(names(conmat)) %in%
                     c("cid","name","number","doc"))
  mdat <- conmat[,metadataI]
  names(mdat) <- tolower(names(mdat))
  bdat <- conmat[,-metadataI]
  for ( i in 1:N) {
    cid <- as.character(mdat$cid[i])
    if (is.null(mdat$name) || mdat$name[i]=="") {
      name <- cid
    } else {
      name <- as.character(mdat$name[i])
    }
    if (is.null(mdat$number) || is.na(mdat$number[i])) {
      number <- i
    } else {
      number <- as.numeric(mdat$number[i])
    }
    if (is.null(mdat$doc)) {
      doc <- ""
    } else {
      doc <- as.character(mdat$doc[i])
    }
    bto <- names(bdat)[as.logical(bdat[i,])]
    con <- Context(cid=cid, name=name, number=number, belongsTo=bto,
                   doc=doc, app=app)

    set <- updateContext(con,set)
  }
  invisible(set)
}

setGeneric("clearContexts",function(set)
  standardGeneric("clearContexts"))

setMethod("clearContexts","list", function (set) list())
setMethod("clearContexts","ContextSet", function (set) set$clearAll())
