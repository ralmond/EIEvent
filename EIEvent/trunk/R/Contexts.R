###
## This defines a context object

setClass("Context",
        slots=c(cid="character",       #Context ID
                name="character",      #Longer name
                number="integer",       #Context #
                belongsTo="character",  #Contexts to which this belongs
                doc="character"))
Context <- function (cid,name,number,belongsTo=character(),
                     doc="") {
  new("Context",cid=cid,name=name,number=number,
      belongsTo=belongsTo,doc=doc)
}
setGeneric("cid", function(c) standardGeneric("cid"))
setGeneric("belongsTo", function(c) standardGeneric("belongsTo"))
setGeneric("belongsTo<-", function(c,val) standardGeneric("belongsTo<-"))
setGeneric("number", function(c) standardGeneric("number"))
setGeneric("number<-", function(c,val) standardGeneric("number<-"))

setMethod("cid","Context", function(c) c@cid)
setMethod("belongsTo","Context", function(c) c@belongsTo)
setMethod("belongsTo<-","Context", function(c,val) {
  c@belongsTo <- val
  c})
setMethod("number","Context", function(c) c@number)
setMethod("number<-","Context", function(c,val) {
  c@number <- val
  c})

setMethod("name","Context", function(x) x@name)
setMethod("doc","Context", function(x) x@doc)


applicableContexts <- function (c) {c(cid(c),belongsTo(c))}

