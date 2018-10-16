## Rule Tables are collections of rules.

setClass("Rule",
         slots=c(app="character",       #Application Identifier
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
setMethod("ruleName","Rule", function(x) x@name)
setMethod("ruleType","Rule", function(x) x@ruleType)
setMethod("condition","Rule", function(x) x@condition)
setMethod("predicate","Rule", function(x) x@predicate)

Rule <- function(context="ALL",verb="ALL",object="ALL",
                 ruleType=c("StatusRule","ObservableRule","ContextRule",
                            "TriggerRule","ResetRule"),
                 priority=5, doc="",
                 name=paste("When in",context,",",verb, object, ruleType),
                 condition=list(),predicate=list(),app="default")
  new("Rule",app=app,name=name,doc=doc,context=contect,verb=verb,object=object,
      ruleType=ruleType,priority=priority,condition=condition,
      predicate=predicate)
}

setMethod("toString","Rule", function(x, ...) {
  paste('Rule:{',x@name,'}')
})
setMethod("show","Rule",function(object) {
  cat(toString(object),"\n")
})

setMethod("as.json","Rule", function(x) {
  jlist <- as.jlist(x,attributes(x))
  toJSON(jlist,POSIXt="mongo")
}

setMethod("as.jlist",c("Rule","list"), function(obj,ml) {
  ml$"_id" <- NULL
  ml$class <-NULL

  ml$name <- unbox(ml$name)
  ml$verb <- unbox(ml$verb)
  ml$object <- unbox(ml$object)
  ml$object <- unbox(ml$ruleType)
  ml$object <- unbox(ml$priority)
  ml
}

parseRule<- function (rec) {
  new("Rule",app=rec$app,name=rec$name,doc=rec$doc,context=rec$contect,
      verb=rec$verb,object=rec$object,ruleType=rec$ruleType,
      priority=rec$priority,condition=parseData(rec$condition),
      predicate=parseData(rec$predicate))
}


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

setMethod("debug","RuleTable",function(x) x@debug)
setMethod("debug<-","RuleTable",function(x,newval) {
  x@debug <- newval
  x})

setMethod("addRules",c("RuleTable","character","character","character","Rule"),
          function (table, context, verb, object, rules) {
            addRules(table,context,verb,object,list(rules))
          })

setMethod("addRules",c("RuleTable","character","character","character","list"),
          function (table, context, verb, object, rules) {
            nrules <- max(length(context), length(verb),
                          length(object),length(rules))
            ## Replicate out singletons to the maximum length
            if (length(context) == 1L) context <- rep(contect,nrules)
            if (length(verb) == 1L) verb <- rep(verb,nrules)
            if (length(object) == 1L) object <- rep(object,nrules)
            if (length(rules) == 1L) rules <- rep(rules,nrules)
            if (length(context)!=nrules || length(verb) != nrules ||
                length(object)!=nrules || length(rules) !=nrules) {
              stop("All arguments to add rules must have the same length (or lenth 1)")
            }
            invalid <- which(!sapply(rules,function (r)
              validRule(table,r)))
            if (length(invalid) > 0L) {
              stop("Rules at positions ",invalid," are not valid for ",
                   class(table))
            }
            table@context <- c(table@context,context)
            table@verb <- c(table@verb,verb)
            table@object <- c(table@object)
            table@rules <- c(table@rules)
            table
          })

### This internal method calculates a logical vector which indicates
### which rules are being sought.
setMethod("affectedRules",c("RuleTable","character","character","character"),
          function(table,context,verb,object) {
            result <- rep(TRUE,length(table@context))
            if (length(context) > 0L)
              result <- result & table@context %in% context
            if (length(verb) > 0L)
              result <- result & table@verb %in% verb
            if (length(object) > 0L)
              result <- result & table@object %in% object
            result
          })

setMethod("removeRules",c("RuleTable","character","character","character"),
          function (table, context, verb, object, rules) {
            if (length(table@context) == 0L) return (table)
            affected <- affectedRules(table,verb,object,rules)
            if (!any(affected)) return (table)
            table@context <- table@context[!affected]
            table@verb <- table@verb[!affected]
            table@object <- table@object[!affected]
            table@rules <- table@rules[!affected]
            table
          })

setMethod("ruleSet",c("RuleTable","character","character","character"),
          function (table, context, verb, object, includeAll=FALSE) {
            if (includeALL) {
              context <- c(context,"ALL")
              verb <- c(verb,"ALL")
              object <- c(object,"ALL")
            }
            table@rules[affectedRules(table,context,verb,object)]
          })

### Abstract method, must be defined by implementing classes.
setGeneric("validRule", function(table, rule)
  standardGeneric("validRule"))


################################################################
## Subclasses of RuleTable.

setClass("StatusRuleTable",contains="RuleTable")

StatusRuleTable <- function() {
  new("StatusRuleTable",context=character(),verb=character(),
      object=character(),rules=list(),debug=FALSE)
}

setMethod("validRule", function (table,rule) valid.StatusRule(rule))

setMethod("runAll", function (table, current, event) {
  applicable <-
    ruleSet(table,table@context,current@verb,current@object,
            includeALL=TRUE)
  for (rule in applicable) {
    current <- do.call(rule,list(current,event))
  }
  current
})


setClass("ContextRuleTable",contains="RuleTable")

ContextRuleTable <- function() {
  new("ContextRuleTable",context=character(),verb=character(),
      object=character(),rules=list(),debug=FALSE)
}

setMethod("validRule", function (table,rule) valid.ContextRule(rule))

setMethod("calculate", function (table, current, event) {
  applicable <-
    ruleSet(table,table@context,current@verb,current@object,
            includeALL=TRUE)
  contexts <- sapply(applicable,
                     function(rule)
                        do.call(rule,list(current,event)))
  ## do.call(c) removes null entries.
  unqiue(do.call("c",contexts))
})

setClass("ObsRuleTable",contains="RuleTable")

ObsRuleTable <- function() {
  new("ObsRuleTable",context=character(),verb=character(),
      object=character(),rules=list(),debug=FALSE)
}

setMethod("validRule", function (table,rule) valid.ObsRule(rule))

setMethod("runAll", function (table, current, event, oldContext) {
  applicable <-
    ruleSet(table,table@context,current@verb,current@object,
            includeALL=TRUE)
  for (rule in applicable) {
    current <- do.call(rule,list(current,event,oldContext))
  }
  current
})

setClass("TriggerRuleTable",contains="RuleTable")

TriggerRuleTable <- function() {
  new("TriggerRuleTable",context=character(),verb=character(),
      object=character(),rules=list(),debug=FALSE)
}

setMethod("validRule", function (table,rule) valid.TriggerRule(rule))

setMethod("calcListeners", function (table, current, event, oldContext) {
  applicable <-
    ruleSet(table,table@context,current@verb,current@object,
            includeALL=TRUE)
  listeners <- sapply(applicable,
                     function(rule)
                        do.call(rule,list(current,event,oldContext)))
  ## do.call(c) removes null entries.
  unqiue(do.call("c",listeners))
})






