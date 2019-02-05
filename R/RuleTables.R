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
                 ruleType=c("Status","Observable","Context",
                            "Trigger","Reset"),
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

setMethod("as.jlist",c("Rule","list"), function(obj,ml,serialize=TRUE) {
  ml$"_id" <- NULL
  ml$class <-NULL

  ml$name <- unbox(ml$name)
  ml$verb <- unbox(ml$verb)
  ml$object <- unbox(ml$object)
  ml$ruleType <- unbox(ml$ruleType)
  ml$priority <- unbox(ml$priority)

  ml$condition <- unparseCondition(ml$condition,serialize)
  ml$predicate <- unparsePredicate(ml$predicate,serialize)

  ml
})

parseRule<- function (rec) {
  if (is.null(rec$"_id")) rec$"_id" <- NA_character_
  names(rec$"_id") <- "oid"
  if (is.null(rec$app)) rec$app <- "default"
  if (is.null(rec$context)) rec$context <- "ALL"
  if (is.null(rec$verb)) rec$verb <- "ALL"
  if (is.null(rec$object)) rec$object <- "ALL"
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

parseCondition <- Proc4::parseData
parsePredicate <- Proc4::parseData
unparseCondition <- Proc4::unparseData
unparsePredicate <- Proc4::unparseData


####################################################################
## Rule Table
## Rules are indexed by context, verb and object, where each could be
## replaced by the special value "ALL" indicating that all values
## should match.

RuleTable <-
  setRefClass("RuleTable",
              fields=c(app="character",
                       dbname="character",
                       dburi="character",
                       db="MongoDB"),
              methods = list(
                  initialize =
                    function(app="default",
                             dbname="EIRecords",
                             dburi="mongo://localhost",
                             db = NULL, # mongo("Rules",dbname,dburi)
                             ...) {
                      callSuper(app=app,db=db,dbname=dbname,dburi=dburi,...)
                    }
              ))

RuleTable$methods(
  updateRule = function (rule) {
    if (!is(rule,"Rule"))
      stop("Argument to RuleTable$update must be a rule.")
    flog.debug("Updating rule %s",name(con))
    saveRec(con,db)
  },
  findRules = function (verb,object,context,phase) {
    if (length(verb)==1L && verb!="ANY") verb <- c(verb,"ANY")
    if (length(object)==1L && object!="ANY") object <- c(object,"ANY")
    rules <- getManyRecs(jquery(verb=verb,object=object,
                                context=context,ruleType=phase),
                         db,parseRule,sort=c("priority"=-1))
    flog.debug("Found %d rules")
    if (length(rules) > 0L) {
      flog.trace("Rules: %s",
                 paste(sapply(rules,name),collapse=", "))
    }
    rules
  },
  ruledb = function () {
    if (is.null(db)) {
      db <<- mongo("Rules",dbname,dburi)
    }
    db
  }
)







