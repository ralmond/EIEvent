## Rule Tables are collections of rules.

########################################
## Rules are just functions.
setClassUnion("Rule",c("function","character"))
valid.Rule <- function(object) {
  if (is.function(object)) TRUE
  if (length(object) >1)) {
    return(paste("Rules must be a single name"))
  }
  if (exists(object,mode="function")) TRUE
  paste("Name ",object,"is not bound to a function.")
}

setClass("StatusRule",contains="Rule")
valid.StatusRule <- function(object) {
  vr <- valid.Rule(object)
  if (vr != TRUE) return (vr)
  fun <- object
  if (is.character(object)) fun <- get(object,mode="function")
  expArgs <- c("current","status")
  if (all.equal(formalArgs(fun)==expArgs)) TRUE
  else paste("StatusRule must have args ",expArgs)
}


setClass("ContextRule",contains="Rule")
valid.ContextRule <- function(object) {
  vr <- valid.Rule(object)
  if (vr != TRUE) return (vr)
  fun <- object
  if (is.character(object)) fun <- get(object,mode="function")
  expArgs <- c("current","status")
  if (all.equal(formalArgs(fun)==expArgs)) TRUE
  else paste("ContextRule must have args ",expArgs)
  }

setClass("ObsRule",contains="Rule")
valid.ObsRule <- function(object) {
  vr <- valid.Rule(object)
  if (vr != TRUE) return (vr)
  fun <- object
  if (is.character(object)) fun <- get(object,mode="function")
  expArgs <- c("current","status","oldContext")
  if (all.equal(formalArgs(fun)==expArgs)) TRUE
  else paste("ObsRule must have args ",expArgs)
}

setClass("TriggerRule",contains="Rule")
valid.TriggerRule <- function(object) {
  vr <- valid.Rule(object)
  if (vr != TRUE) return (vr)
  fun <- object
  if (is.character(object)) fun <- get(object,mode="function")
  expArgs <- c("current","status","oldContext")
  if (all.equal(formalArgs(fun)==expArgs)) TRUE
  else paste("TriggerRule must have args ",expArgs)
}

### These tests are fairly weak as they don't test the output or input
### types, but that is hard to do given R's polymorphism.

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






