## Rule Tables are collections of rules.

setClass("Rule",
         slots=c("_id"="character",     #Mongo ID
                 app="character",       #Application Identifier
                 name="character",      #Human identifier
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
setMethod("app","Rule", function(x) x@app)
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

  ml$app <- unboxer(ml$app)
  ml$name <- unboxer(ml$name)
  ml$doc <- unboxer(ml$doc)
  ml$verb <- unboxer(ml$verb)
  ml$object <- unboxer(ml$object)
  ml$context <- unboxer(ml$context)
  ml$ruleType <- unboxer(ml$ruleType)
  ml$priority <- unboxer(ml$priority)

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

parseCondition <- mongo::parseData
parsePredicate <- mongo::parseData
unparseCondition <- mongo::unparseData
unparsePredicate <- mongo::unparseData


##We need an all.equal method as we need to suppress checking names on
##parts of the data fields which might be different.
all.equal.Rule <- function (target, current, ...) {
  if (!is(current,"Rule"))
    return(paste("Target is 'Rule' and current is '",class(current),"'."))
  msg <- character()
  if ((is.na(target@"_id") && !is.na(current@"_id")) ||
      (!is.na(target@"_id") &&
       !isTRUE(all.equal(target@"_id", current@"_id"))))
    msg <- c(msg,"Database IDs do not match.")
  if (app(target) != app(current))
    msg <- c(msg,"Application IDs do not match.")
  if (name(target) != name(current))
    msg <- c(msg,"Names do not match.")
  if (doc(target) != doc(current))
    msg <- c(msg,"Doc strings do not match.")
  if (context(target) != context(current))
    msg <- c(msg,"Contexts do not match.")
  if (verb(target) != verb(current))
    msg <- c(msg,"Verbs do not match.")
  if (object(target) != object(current))
    msg <- c(msg,"Objects do not match.")
  if (ruleType(target) != ruleType(current))
    msg <- c(msg,"Rule types do not match.")
  if (priority(target) != priority(current))
    msg <- c(msg,"Priorities do not match.")
  cmsg <- all.equal(condition(target),condition(current))
  if (!isTRUE(cmsg))
    msg <- c(msg,paste("Condition mismatch:",cmsg))
  pmsg <- all.equal(predicate(target),predicate(current))
  if (!isTRUE(pmsg))
    msg <- c(msg,paste("Predicate mismatch:",pmsg))
  ## Return true if message list is empty.
  if (length(msg)==0L) TRUE
  else msg
}

###################################################################
## Running Rules
runRule <- function (state,event,rule,phase) {
  withFlogging({
    satisfied <- checkCondition(condition(rule),state,event)
    flog.trace("Condition for rule %s for %s: %s",
               name(rule),uid(state),as.character(satisfied))
    if (isTRUE(satisfied)) {
      state <- executePredicate(predicate(rule),state,event)
      flog.trace("New state",state,capture=TRUE)
    }
    state
  },state=state,event=event,rule=rule,phase=phase,
  context=paste("Running rule",name(rule),"for",uid(state)))
}

runTRule <- function (state,event,rule,listeners) {
  withFlogging({
    satisfied <- checkCondition(condition(rule),state,event)
    flog.trace("Condition for rule %s for %s: %s",
               name(rule),uid(state),as.character(satisfied))
    if (isTRUE(satisfied)) {
      messes <- buildMessages(predicate(rule),state,event)
      lapply(messes,function(m) receiveMessage(listeners,m))
    }
  },state=state,event=event,rule=rule,phase="Trigger",
  context=paste("Running rule",name(rule),"for",uid(state)))
}




####################################################################
## Rule Table
## Rules are indexed by context, verb and object, where each could be
## replaced by the special value "ALL" indicating that all values
## should match.

RuleTable <-
  setRefClass("RuleTable",
              fields=c(app="character",
                       db="JSONDB",
                       stoponduplicate="logical"),
              methods = list(
                  initialize =
                    function(app="default",
                             db = MongoDB("Rules","EIRecords"),
                             stoponduplicate=FALSE,
                             ...) {
                      callSuper(app=app,db=db,
                                stoponduplicate=stoponduplicate,...)
                    }
              ))

RuleTable$methods(
  skipDuplicate = function (newval=NULL) {
    if (missing(newval) || is.null(newval)) stoponduplicate
    else stoponduplicate <<- newval
  },
  updateRule = function (rule) {
    if (!is(rule,"Rule"))
      stop("Argument to RuleTable$update must be a rule.")
    old <- findRuleByName(name(rule))
    if (!is.null(old)) {
      if (!is.na(rule@'_id') && rule@'_id' != old@'_id') {
        flog.error("Two rules with name %s, old id %s, new id %s.  Not added.",
                   name(rule),old@'_id', rule@'_id')
        return (rule)
      }
      if (stoponduplicate) {
        flog.error("Two rules with name %s, skipping.", name(rule))
        return (rule)
      } else {
        flog.warn("Replacing rule named %s.",name(rule))
        rule@'_id' <- old@'_id'
      }
    }
    rule@app <- app
    flog.debug("Updating rule %s",name(rule))
    saveRec(ruledb(),rule)
  },
  findRuleByName = function (name) {
    getOneRec(ruledb(),buildJQuery(name=name,app=app),parseRule)
  },
  findRules = function (verb,object,context,phase=NULL) {
    flog.debug("Searching for rules v=%s, o=%s, c=%s, ph=%s",
               verb, object, context, phase)
    verb <- unique(c(verb,"ANY","ALL"))
    object <- unique(c(object,"ANY","ALL"))
    if (!is.null(phase)) {
      query <- buildJQuery(verb=verb,object=object,
                           context=context,ruleType=phase,
                           app=app)
    } else {
      query <- buildJQuery(verb=verb,object=object,
                           context=context,app=app)
    }
    rules <- getManyRecs(ruledb(),query,parseRule,sort=c("priority"=1))
    flog.debug("Found %d rules",length(rules))
    if (length(rules) > 0L) {
      flog.trace("Rules: %s",
                 paste(sapply(rules,name),collapse=", "))
    }
    rules
  },
  ruledb = function () {
    db
  },
  clearAll = function () {
    flog.info("Clearing Rule database for %s",app)
    mdbRemove(ruledb(),buildJQuery(app=app))
  }
)

newRuleTable <- function(app="default",colname="Rules",
                          dbname="EIRecords",
                          dburi=character(),
                          sslops=mongolite::ssl_options(),
                          noMongo=length(dburi)==0L,
                          mongoverbose=FALSE,
                          db=NULL,
                          stoponduplicate=FALSE) {
  flog.trace("Database = %s:%s:%s, verbose=%s,noMongo=%s",
             dburi,dbname,colname,mongoverbose,noMongo)
  flog.trace("SSLops:",sslops,capture=TRUE)
  if (missing(db) || ! is(db,"MongoDB")) {
    db <- MongoDB(colname,dbname,dburi,
                  mongoverbose,noMongo,sslops)
  }
  RuleTable$new(app,db,stoponduplicate)
}

loadRulesFromList <- function(set, rulelist, stopOnDups=TRUE) {
  rnames <- sapply(rulelist,name)
  dups <- duplicated(rnames)
  if (any(dups)) {
    ruleset <- rulelist[!dups]
    names(ruleset) <- rnames[!dups]
    for (dup in rulelist[dups]) {
      orig <- ruleset[[name(dup)]]
      match <- all.equal(orig,dup)
      if (isTRUE(match)) {
        flog.warn("Duplicate rule named %s.",name(dup))
      } else {
        flog.error("Two rules named %s which are different.",name(dup))
        flog.debug("Difference:",match,capture=TRUE)
        if (stopOnDups) stop("Duplicate rule name",name(dup))
        flog.info("Keeping newer version.")
        ruleset[[name(dup)]] <- dup
      }
    }
  } else {
    ruleset <- rulelist
  }
  olddup <- set$skipDuplicate()
  set$skipDuplicate(stopOnDups)
  for (rl in ruleset) {
    set$updateRule(rl)
  }
  set$skipDuplicate(olddup)
  invisible(set)
}

testAndLoad <- function (set, filename, stopOnDups=FALSE) {
  if (!file.exists(filename)) {
    stop("Cannot find file ",filename)
  }
  script <- fromJSON(filename,FALSE)
  ruleset <- list()
  for (i in 1L:length(script)) {
    test <- withFlogging(parseRuleTest(script[[i]]),
                         context=paste("Parsing test ",i),
                         json=script[[i]])
    if (is(test,"try-error")) next
    result <- testRule(test)
    if (isTRUE(result)) {
      rule <- rule(test)
      dup <- ruleset[[name(rule)]]
      if (!is.null(dup)) {
        match <- all.equal(rule,dup)
        if (isTRUE(match)) {
          flog.info("Already loaded rule named %s.",name(dup))
        } else {
          flog.error("Two rules named %s which are different.",name(dup))
          flog.debug("Difference:",match,capture=TRUE)
          if (stopOnDups) stop("Duplicate rule name",name(dup))
          flog.info("Keeping newer version.")
        }
      }
      flog.info("Adding rule %s.",name(rule))
      set$updateRule(rule)
      ruleset[[name(rule)]] <- rule
    } else {
      flog.info("Skipping %s, because failed test.",name(rule))
    }
  }
  invisible(set)
}





