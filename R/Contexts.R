###
## This defines a context object

setCass("Context",
        slots=c(cid="character",       #Context ID
                name="character",      #Longer name
                number="integer",       #Context #
                belongsTo="character",  #Contexts to which this belongs
                doc="character"))

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

setMethod("name","Context", function(c) c@name)
setMethod("doc","Context", function(c) c@doc)


applicableContexts <- function (c) {c(cid(c),belongsTo(c))}

