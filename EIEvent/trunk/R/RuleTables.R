## Rule Tables are collections of rules.

setClass("Rule",
         slots=c("_id"="character",     #Mongo ID
                 app="character",       #Application Identifier
                 name="character",      #Human idenfier
                 doc="character",       #Human description
                 context="character",   #Applicable context
                 verb="character",      #Applicable verb
                 object="character",    #Applicable object
                 ruleType="character",  #Type of rule
                 priority="numeric",    #Rule Sequence
                 condition="list",      #Rule Protaxis
                 predicate="list"       #Rule apodosis
                 ))

setMethod("context","Rule", function(x) x@context)
setMethod("verb","Rule", function(x) x@verb)
setMethod("object","Rule", function(x) x@object)
setMethod("name","Rule", function(x) x@name)
setMethod("doc","Rule", function(x) x@doc)
setMethod("ruleType","Rule", function(x) x@ruleType)
setMethod("priority","Rule", function(x) x@priority)
setMethod("condition","Rule", function(x) x@condition)
setMethod("predicate","Rule", function(x) x@predicate)

Rule <- function(context="ALL",verb="ALL",object="ALL",
                 ruleType=c("StatusRule","ObservableRule","ContextRule",
                            "TriggerRule","ResetRule"),
                 priority=5, doc="",
                 name=paste("When in",context,",",verb, object, ruleType),
                 condition=list(),predicate=list(),app="default") {
  new("Rule","_id"=c(oid=NA_character_),app=app,name=name,
      doc=doc,context=context,verb=verb,object=object,
      ruleType=ruleType,priority=priority,condition=condition,
      predicate=predicate)
}

setMethod("toString","Rule", function(x, ...) {
  paste('Rule:{',x@name,'}')
})
setMethod("show","Rule",function(object) {
  cat(toString(object),"\n")
})

setMethod("as.jlist",c("Rule","list"), function(obj,ml) {
  ml$"_id" <- NULL
  ml$class <-NULL

  ml$name <- unbox(ml$name)
  ml$verb <- unbox(ml$verb)
  ml$object <- unbox(ml$object)
  ml$ruleType <- unbox(ml$ruleType)
  ml$priority <- unbox(ml$priority)

  ml$condition <- unparseCondition(ml$condition)
  ml$predicate <- unparsePredicate(ml$predicate)

  ml
})

parseRule<- function (rec) {
  if (is.null(rec$"_id")) rec$"_id" <- NA_character_
  names(rec$"_id") <- "oid"
  new("Rule","_id"=rec$"_id",
      app=as.vector(rec$app),
      name=as.vector(rec$name),
      doc=as.vector(rec$doc),context=as.vector(rec$context),
      verb=as.vector(rec$verb),object=as.vector(rec$object),
      ruleType=as.vector(rec$ruleType),
      priority=as.vector(rec$priority),
      condition=parseCondition(rec$condition),
      predicate=parsePredicate(rec$predicate))
}

## Although in theory we could serialize

## This was the old code for parseData, it doesn't work for more
## complex objects, but it might work for conditions and predicates.
parseSimpleData <- function (messData) {
  ##Need to convert back from list to numeric/character
  for (i in 1:length(messData)) {
    datum <- messData[[i]]
    if (all(sapply(datum,is.character)) && all(sapply(datum,length)==1L)) {
      datum <- as.character(datum)
      names(datum) <- names(messData[[i]])
    }
    if (all(sapply(datum,is.logical)) && all(sapply(datum,length)==1L)) {
      datum <- as.logical(datum)
      names(datum) <- names(messData[[i]])
    }
    if (all(sapply(datum,is.numeric)) && all(sapply(datum,length)==1L)) {
      if (all(sapply(datum,is.integer))) {
        datum <- as.integer(datum)
      } else {
        datum <- as.numeric(datum)
      }
      names(datum) <- names(messData[[i]])
    }
    ## May need an extra step here to decode data which
    ## are not one of the primative vector types.
    messData[[i]] <- datum
  }
  messData
}

parseCondition <- Proc4::parseData
parsePredicate <- Proc4::parseData
unparseCondition <- Proc4::unparseData
unparsePredicate <- Proc4::unparseData


####################################################################
## Rule Table
## Rules are indexed by context, verb and object, where each could be
## replaced by the special value "ALL" indicating that all values
## should match.

setClass("RuleTable",
         slots=c(context="character",
                 verb="character",
                 object="character",
                 rules="list",
                 debug="logical"),
         contains="VIRTUAL")

setGeneric("RTdebug",function(x) standardGeneric("RTdebug"))
setMethod("RTdebug","RuleTable",function(x) x@debug)
setGeneric("RTdebug<-",function(x,value) standardGeneric("RTdebug<-"))
setMethod("RTdebug<-","RuleTable",function(x,value) {
  x@debug <- value
  x})

# setMethod("addRules",c("RuleTable","character","character","character","Rule"),
#           function (table, context, verb, object, rules) {
#             addRules(table,context,verb,object,list(rules))
#           })
#
# setMethod("addRules",c("RuleTable","character","character","character","list"),
#           function (table, context, verb, object, rules) {
#             nrules <- max(length(context), length(verb),
#                           length(object),length(rules))
#             ## Replicate out singletons to the maximum length
#             if (length(context) == 1L) context <- rep(context,nrules)
#             if (length(verb) == 1L) verb <- rep(verb,nrules)
#             if (length(object) == 1L) object <- rep(object,nrules)
#             if (length(rules) == 1L) rules <- rep(rules,nrules)
#             if (length(context)!=nrules || length(verb) != nrules ||
#                 length(object)!=nrules || length(rules) !=nrules) {
#               stop("All arguments to add rules must have the same length (or lenth 1)")
#             }
#             invalid <- which(!sapply(rules,function (r)
#               validRule(table,r)))
#             if (length(invalid) > 0L) {
#               stop("Rules at positions ",invalid," are not valid for ",
#                    class(table))
#             }
#             table@context <- c(table@context,context)
#             table@verb <- c(table@verb,verb)
#             table@object <- c(table@object)
#             table@rules <- c(table@rules)
#             table
#           })
#
# ### This internal method calculates a logical vector which indicates
# ### which rules are being sought.
# setMethod("affectedRules",c("RuleTable","character","character","character"),
#           function(table,context,verb,object) {
#             result <- rep(TRUE,length(table@context))
#             if (length(context) > 0L)
#               result <- result & table@context %in% context
#             if (length(verb) > 0L)
#               result <- result & table@verb %in% verb
#             if (length(object) > 0L)
#               result <- result & table@object %in% object
#             result
#           })
#
# setMethod("removeRules",c("RuleTable","character","character","character"),
#           function (table, context, verb, object, rules) {
#             if (length(table@context) == 0L) return (table)
#             affected <- affectedRules(table,verb,object,rules)
#             if (!any(affected)) return (table)
#             table@context <- table@context[!affected]
#             table@verb <- table@verb[!affected]
#             table@object <- table@object[!affected]
#             table@rules <- table@rules[!affected]
#             table
#           })
#
# setMethod("ruleSet",c("RuleTable","character","character","character"),
#           function (table, context, verb, object, includeAll=FALSE) {
#             if (includeALL) {
#               context <- c(context,"ALL")
#               verb <- c(verb,"ALL")
#               object <- c(object,"ALL")
#             }
#             table@rules[affectedRules(table,context,verb,object)]
#           })
#
# ### Abstract method, must be defined by implementing classes.
# setGeneric("validRule", function(table, rule)
#   standardGeneric("validRule"))


################################################################
## Subclasses of RuleTable.

setClass("StatusRuleTable",contains="RuleTable")

StatusRuleTable <- function() {
  new("StatusRuleTable",context=character(),verb=character(),
      object=character(),rules=list(),debug=FALSE)
}

# setMethod("validRule", function (table,rule) valid.StatusRule(rule))
#
# setMethod("runAll", function (table, current, event) {
#   applicable <-
#     ruleSet(table,table@context,current@verb,current@object,
#             includeALL=TRUE)
#   for (rule in applicable) {
#     current <- do.call(rule,list(current,event))
#   }
#   current
# })
#

setClass("ContextRuleTable",contains="RuleTable")

ContextRuleTable <- function() {
  new("ContextRuleTable",context=character(),verb=character(),
      object=character(),rules=list(),debug=FALSE)
}

# setMethod("validRule", function (table,rule) valid.ContextRule(rule))
#
# setMethod("calculate", function (table, current, event) {
#   applicable <-
#     ruleSet(table,table@context,current@verb,current@object,
#             includeALL=TRUE)
#   contexts <- sapply(applicable,
#                      function(rule)
#                         do.call(rule,list(current,event)))
#   ## do.call(c) removes null entries.
#   unqiue(do.call("c",contexts))
# })
#
# setClass("ObsRuleTable",contains="RuleTable")
#
# ObsRuleTable <- function() {
#   new("ObsRuleTable",context=character(),verb=character(),
#       object=character(),rules=list(),debug=FALSE)
# }
#
# setMethod("validRule", function (table,rule) valid.ObsRule(rule))
#
# setMethod("runAll", function (table, current, event, oldContext) {
#   applicable <-
#     ruleSet(table,table@context,current@verb,current@object,
#             includeALL=TRUE)
#   for (rule in applicable) {
#     current <- do.call(rule,list(current,event,oldContext))
#   }
#   current
# })

setClass("TriggerRuleTable",contains="RuleTable")

TriggerRuleTable <- function() {
  new("TriggerRuleTable",context=character(),verb=character(),
      object=character(),rules=list(),debug=FALSE)
}

# setMethod("validRule", function (table,rule) valid.TriggerRule(rule))
#
# setMethod("calcListeners", function (table, current, event, oldContext) {
#   applicable <-
#     ruleSet(table,table@context,current@verb,current@object,
#             includeALL=TRUE)
#   listeners <- sapply(applicable,
#                      function(rule)
#                         do.call(rule,list(current,event,oldContext)))
#   ## do.call(c) removes null entries.
#   unqiue(do.call("c",listeners))
# })






