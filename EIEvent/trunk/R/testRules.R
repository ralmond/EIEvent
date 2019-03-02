## Testing Scripts for Rules.

## A query test-script has the following fields:
## name -- used in reporting
## doc -- a documentation string
## state -- the initial state.
## event -- the event
## rule -- the rule being tested.
## result -- a logical value.

setClass("RuleTest",
         list("_id"="character",
              app="character",
              name="character",
              doc="character",
              initial="Status",
              event="Event",
              rule="Rule",
              queryResult="logical",
              final="Status"))

RuleTest <- function(name=paste("Test of Rule",rule),
                     doc="",app="default",initial,event,rule,queryResult,
                     final) {
  new("RuleTest","_id"=c(oid=NA_character_),app=app,name=name,
      doc=doc,initial=initial,event=event,rule=rule,
      queryResult=queryResult,
      final=final)
}


setMethod("name","RuleTest", function(x) x@name)
setMethod("doc","RuleTest", function(x) x@doc)
setMethod("initial","RuleTest", function(x) x@initial)
setMethod("event","RuleTest", function(x) x@event)
setMethod("rule","RuleTest", function(x) x@rule)
setMethod("queryResult","RuleTest", function(x) x@queryResult)
setMethod("final","RuleTest", function(x) x@final)


setMethod("toString","RuleTest", function(x, ...) {
  paste('RuleTest:{',x@name,'}')
})
setMethod("show","RuleTest",function(object) {
  cat(toString(object),"\n")
})

setMethod("as.jlist",c("RuleTest","list"), function(obj,ml,serialize=TRUE) {
  ml$"_id" <- NULL
  ml$class <-NULL

  ml$name <- unbox(ml$name)
  ml$doc <- unbox(ml$name)
  ml$queryResult <- unbox(ml$queryResult)

  ml$inital <- as.jlist(ml$initial,attributes(ml$initial),serialize)
  ml$event <- as.jlist(ml$event,attributes(ml$event),serialize)
  ml$rule <- as.jlist(ml$rule,attributes(ml$rule),serialize)
  ml$final <- as.jlist(ml$final,attributes(ml$initial),serialize)

  ml
})

parseRuleTest<- function (rec) {
  if (is.null(rec$"_id")) rec$"_id" <- NA_character_
  names(rec$"_id") <- "oid"
  if (is.null(rec$app)) rec$app <- "default"
  new("RuleTest","_id"=rec$"_id",
      app=as.vector(rec$app),
      name=as.vector(rec$name),
      doc=as.vector(rec$doc),
      initial=parseStatus(rec$initial),
      event=parseEvent(rec$event),
      rule=parseRule(rec$rule),
      queryResult=as.vector(rec$queryResult),
      final=parseStatus(rec$final))
}

########################################################################
###  Test Methods.

testQuery <- function (test) {
  context <- paste("Running Query test",name(test))
  flog.info(context)
  if (!is.null(doc(test))) flog.debug(doc(test))
  rule <- rule(test)
  if(is.null(rule)) stop("Missing rule in test",name(test))
  initial <- initial(test)
  if(is.null(initial)) stop("Missing initial state in test",name(test))
  event <- event(test)
  if(is.null(event)) stop("Missing event in test",name(test))
  expected <- queryResult(test)
  if(is.null(expected)) stop("Missing Query Result in test",name(test))
  actual <-withFlogging(checkCondition(condition(rule),initial,event),
                        context=context,rule=rule,
                        initial=initial,event=event)
  if (is(actual,"try-error")) {
    result <- NA
  } else {
    result <- actual==expected
  }
  flog.info("Test %s: %s.",name(test),
            ifelse(is.na(result),"Error",ifelse(result,"Passed","Failed")))
  if (!is.na(result) && !result) {
    flog.debug("Rule:",rule,capture=TRUE)
    flog.debug("Initial State:",initial(test),capture=TRUE)
    flog.debug("Event:",event,capture=TRUE)
    flog.debug("Expected Result:",expected,capture=TRUE)
  }
  return (result)
}

## queryTest <- function (test, quiet=FALSE, verbose=FALSE,
##                        name=test$name) {
##   if (verbose) {
##     cat("Running test ",name,"\n")
##     if (!is.null(test$doc)) print(test$doc)
##   }
##   if (is(test$state,"Status")) {
##     state <- test$state
##   } else {
##     state <- parseStatus(test$state)
##   }
##   if (!is(state,"Status")) {
##     stop("Problems parsing status for test ",name)
##   }
##   if (is(test$event,"Event")) {
##     event <- test$event
##   } else {
##     event <- parseEvent(test$event)
##   }
##   if (!is(event,"Event")) {
##     stop("Problems parsing event for test ",name)
##   }
##   if (is(test$rule,"Rule")) {
##     rule <- test$rule
##   } else {
##     rule <- parseRule(test$rule)
##   }
##   if (!is(rule,"Rule")) {
##     stop("Problems parsing rule for test ",name)
##   }
##   if (verbose) {
##     cat("Testing rule ",name(rule),"\n")
##   }
##   expected <- as.logical(test$result)
##   if (is.na(expected)) {
##     stop("Result must be true of false for test", name)
##   }
##   actual <-try(checkCondition(condition(rule),state,event))
##   if (is(actual,"try-error")) {
##     if (!quiet) {
##       cat("Error occurred while running test",name)
##     }
##     if (verbose) {
##       traceback(attr(actual,"condition"))
##     }
##     return (as.logical(NA))
##   }
##   result <- actual==expected
##   if (!quiet) {
##     cat("Test ",name,": ",ifelse(result,"Passed","Failed"),".\n")
##   }
##   return (result)
## }

testQueryScript <- function (filename,  suiteName=basename(filename)) {
  if (!file.exists(filename)) {
    stop("Cannot find file ",filename)
  }
  script <- fromJSON(filename,FALSE)
  N <- length(script)
  result <- rep(as.logical(NA),N)
  names(result) <- paste(suiteName,1:N)
  for (i in 1:N) {
    test <- withFlogging(parseRuleTest(script[[i]]),
                            context=paste("Parsing test ",i),
                            json=script[[i]])
    if (is(test,"try-error")) next
    if (is.null(name(test))) {
      test@name <- paste(suiteName,i)
    }
    names(result)[i] <- name(test)
    if (is.null(doc(test))) test@doc <- paste("Test",i,"in suite",suiteName,".")

    result[i] <- testQuery(test)
  }
  flog.info("Test suite %s: %d tests, %d passed, %d failed, %d errors.",
            suiteName,N, sum(result,na.rm=TRUE),
            sum(!result,na.rm=TRUE),
            sum(is.na(result)))
  result
}

testPredicate <- function (test) {
  if (!queryResult(test)) {
    flog.info("Skipping test %s as condition is false.",name(test))
    return (TRUE)
  }
  context <- paste("Running Predicate test",name(test))
  flog.info(context)
  if (!is.null(doc(test))) flog.debug(doc(test))
  rule <- rule(test)
  if(is.null(rule)) stop("Missing rule in test",name(test))
  state <- initial(test)
  if(is.null(state)) stop("Missing initial state in test",name(test))
  event <- event(test)
  if(is.null(event)) stop("Missing event in test",name(test))
  expected <- final(test)
  if(is.null(expected)) stop("Missing final state in test",name(test))

  actual <-withFlogging(executePredicate(predicate(rule),state,event),
                        context=context,rule=rule,
                        initial=state,event=event)
  if (is(actual,"try-error")) {
    flog.info("Test %s:  Error.",name(test))
    result <- NA
  } else {
    result <- withFlogging(all.equal(expected,actual),
                           context=paste(context,": Checking results"),
                           rule=rule,
                           initial=state,event=event)
    if (is(result,"try-error")) {
      flog.info("Test %s:  Error.",name(test))
      result <- NA
    } else  if (isTRUE(result)) {
      flog.info("Test %s:  Passed.",name(test))
      result <- TRUE
    } else {
      flog.info("Test %s:  Failed.",name(test))
      flog.info("Details:",result,capture=TRUE)
      result <- FALSE
    }
  }
  if (!is.na(result) && !result) {
    flog.debug("Rule:",rule,capture=TRUE)
    flog.debug("Initial State:",initial(test),capture=TRUE)
    flog.debug("Event:",event,capture=TRUE)
    flog.debug("Expected Result:",expected,capture=TRUE)
    flog.debug("Actual Result:",actual,capture=TRUE)
  }
  return (result)
}

testPredicateScript <- function (filename,  suiteName=basename(filename)) {
  if (!file.exists(filename)) {
    stop("Cannot find file ",filename)
  }
  script <- fromJSON(filename,FALSE)
  N <- length(script)
  result <- rep(as.logical(NA),N)
  names(result) <- paste(suiteName,1:N)
  run <- rep(TRUE,N)
  for (i in 1:N) {
    test <- withFlogging(parseRuleTest(script[[i]]),
                         context=paste("Parsing test ",i),
                         json=script[[i]])
    if (is(test,"try-error")) {
      next
    }
    if (is.null(name(test))) {
      test@name <- paste(suiteName,i)
    }
    names(result)[i] <- name(test)
    if (!queryResult(test)) {
      flog.info("Skipping test %s as condition is false.",name(test))
      run[i] <- FALSE
      next
    }
    if (is.null(doc(test))) test@doc <- paste("Test",i,"in suite",suiteName,".")
    result[i] <- testPredicate(test)
  }
  ## Remove Skiped tests.
  result <- result[run]
  flog.info("Test suite %s: %d tests, %d skipped, %d passed, %d failed, %d errors.",
            suiteName,N, sum(!run), sum(result,na.rm=TRUE),
            sum(!result,na.rm=TRUE),
            sum(is.na(result)))
  result
}


testRule <- function (test, contextSet=NULL) {
  context <- paste("Running test",name(test))
  flog.info(context)
  if (!is.null(doc(test))) flog.debug(doc(test))
  rule <- rule(test)
  if(is.null(rule)) stop("Missing rule in test",name(test))
  state <- initial(test)
  if(is.null(state)) stop("Missing initial state in test",name(state))
  event <- event(test)
  if(is.null(event)) stop("Missing event in test",name(test))
  expected <- final(test)
  if(is.null(expected)) stop("Missing final state in test",name(test))
  ### Check the verb, object and context
  actual <- state                       #Default is no change.
  verbmatch <- verb(rule)=="ALL" | verb(rule)==verb(event)
  objmatch <- object(rule)=="ALL" | object(rule)==object(event)
  if (is.null(contextSet)) {
    contextmatch <- NA
  } else {
    conSet <- withFlogging(applicableContexts(matchContext(context(state),
                                                           contextSet)),
                           context=paste(context,"Matching context"),
                           target=context(status))
    if (is(conSet,'try-error')) {
      flog.info("Test %s:  Error.",name(test))
      return (as.logical(NA))
    }
    contextmatch <- context(rule) %in% conSet
  }
  flog.info("Verb match: %s, Object match: %s, Context match:",
             verbmatch, objmatch,contextmatch)
  satisfied <- FALSE
  if (verbmatch && objmatch &&
      (is.na(contextmatch) || contextmatch)) {
    satisfied <- withFlogging(checkCondition(condition(rule),state,event),
                              context=paste(context,"Condition"),
                              rule=rule, initial=state,event=event)

    if (is(satisfied,"try-error")) {
      flog.info("Test %s:  Error.",name(test))
      satisfied <- FALSE
      actual <- NA
    }
  }
  if (satisfied) {
    actual <-withFlogging(executePredicate(predicate(rule),state,event),
                          context=paste(context,"Predicate"),rule=rule,
                          initial=state,event=event)
  } else {
    flog.info("Skipping predicate for %s, condition is false.",name(test))
    actual <- state
  }
  if (is(actual,"try-error")) {
    flog.info("Test %s:  Error.",name(test))
    result <- NA
  } else {
    result <- withFlogging(all.equal(expected,actual),
                           context=paste(context,": Checking results"),
                           rule=rule,
                           initial=state,event=event)
    if (is(result,"try-error")) {
      flog.info("Test %s:  Error.",name(test))
      result <- NA
    } else if  (isTRUE(result)) {
      flog.info("Test %s:  Passed.",name(test))
      result <- TRUE
    } else {
      flog.info("Test %s:  Failed.",name(test))
      flog.info("Details:",result,capture=TRUE)
      result <- FALSE
    }
  }
  if (!is.na(result) && !result) {
    flog.debug("Rule:",rule,capture=TRUE)
    flog.debug("Initial State:",initial(test),capture=TRUE)
    flog.debug("Event:",event,capture=TRUE)
    flog.debug("Expected Result:",expected,capture=TRUE)
    flog.debug("Actual Result:",actual,capture=TRUE)
  }
  return (result)
}

testRuleScript <- function (filename, suiteName=basename(filename),
                            contextSet=NULL) {
  if (!file.exists(filename)) {
    stop("Cannot find file ",filename)
  }
  script <- fromJSON(filename,FALSE)
  N <- length(script)
  result <- rep(as.logical(NA),N)
  names(result) <- paste(suiteName,1:N)
  for (i in 1:N) {
    test <- withFlogging(parseRuleTest(script[[i]]),
                         context=paste("Parsing test ",i),
                         json=script[[i]])
    if (is(test,"try-error")) next
    if (is.null(name(test))) {
      test@name <- paste(suiteName,i)
    }
    names(result)[i] <- name(test)
    if (is.null(doc(test))) test@doc <- paste("Test",i,"in suite",suiteName,".")

    result[i] <- testRule(test)
  }
  flog.info("Test suite %s: %d tests, %d passed, %d failed, %d errors.",
            suiteName,N, sum(result,na.rm=TRUE),
            sum(!result,na.rm=TRUE),
            sum(is.na(result)))
  result
}

#########################################
## Test Sets

TestSet <-
  setRefClass("TestSet",
              fields=c(app="character",
                       dbname="character",
                       dburi="character",
                       contexts="ContextSet",
                       rules="RuleTable",
                       db="MongoDB"),
              methods = list(
                  initialize =
                    function(app="default",
                             dbname="EIRecords",
                             dburi="mongo://localhost",
                             contexts=ContextSet$new(app,dburi),
                             rules=RuleTable$new(app,dbname,dburi),
                             db = NULL,
                             ...) {
                      callSuper(app=app,db=db,dbname=dbname,dburi=dburi,
                                contexts=contexts,rules=rules,...)
                    },
                  testdb = function () {
                    if (is.null(db)) {
                      db <<- mongo("RuleTests",dbname,dburi)
                    }
                    db
                  }

              ))
