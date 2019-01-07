## Testing Scripts for Rules.

## A query test-script has the following fields:
## name -- used in reporting
## doc -- a documentation string
## state -- the initial state.
## event -- the event
## rule -- the rule being tested.
## result -- a logical value.

queryTest <- function (test, quiet=FALSE, verbose=FALSE,
                       name=test$name) {
  if (verbose) {
    cat("Running test ",name,"\n")
    if (!is.null(test$doc)) print(test$doc)
  }
  if (is(test$state,"Status")) {
    state <- test$state
  } else {
    state <- parseStatus(test$state)
  }
  if (!is(state,"Status")) {
    stop("Problems parsing status for test ",name)
  }
  if (is(test$event,"Event")) {
    event <- test$event
  } else {
    event <- parseEvent(test$event)
  }
  if (!is(event,"Event")) {
    stop("Problems parsing event for test ",name)
  }
  if (is(test$rule,"Rule")) {
    rule <- test$rule
  } else {
    rule <- parseRule(test$rule)
  }
  if (!is(rule,"Rule")) {
    stop("Problems parsing rule for test ",name)
  }
  if (verbose) {
    cat("Testing rule ",name(rule),"\n")
  }
  expected <- as.logical(test$result)
  if (is.na(expected)) {
    stop("Result must be true of false for test", name)
  }
  actual <-try(checkCondition(condition(rule),state,event))
  if (is(actual,"try-error")) {
    if (!quiet) {
      cat("Error occurred while running test",name)
    }
    if (verbose) {
      traceback(attr(actual,"condition"))
    }
    return (as.logical(NA))
  }
  result <- actual==expected
  if (!quiet) {
    cat("Test ",name,": ",ifelse(result,"Passed","Failed"),".\n")
  }
  return (result)
}

queryTestScript <- function (filename,  quiet=FALSE, verbose=FALSE,
                             suiteName=basename(filename)) {
  script <- fromJSON(filename,FALSE)
  N <- length(script)
  result <- rep(as.logical(NA),N)
  names(result) <- paste(suiteName,1:N)
  for (i in 1:N) {
    test <- script[[i]]
    if (is.null(test$name)) {
      test$name <- paste(suiteName,i)
    } else {
      names(result)[i] <- test$name
    }
    if (is.null(test$doc)) test$doc <- paste("Test",i,"in suite",suiteName,".")
    res <- withJavaLogging(queryTest(test, quiet=quiet, verbose=verbose),
                           silentSuccess=!verbose,stopIsFatal=FALSE)
    ## if (!is(res,"try-error")) {
    ##   if (!quiet) {
    ##     cat("Error occurred while parsing test",test$name)
    ##   }
    ##   if (verbose) {
    ##     print(conditionCall(attr(res,"condition")))
    ##     traceback(attr(res,"condition"))
    ##   }
    ##   result[i] <- result
    ## }
    result[i] <- res
    if (verbose) cat("\n\n")

  }
  if (!quiet) {
    cat("Test suite",suiteName,": ",N,"tests, ",
        sum(result,na.rm=TRUE), "passed, ",
        sum(!result,na.rm=TRUE), "failed, ",
        sum(is.na(result)), "errors.\n")
  }
  result
}

predicateTest <- function (test, quiet=FALSE, verbose=FALSE,
                      name=test$name) {
  if (verbose) {
    cat("Running test ",name,"\n")
    if (!is.null(test$doc)) print(test$doc)
  }
  if (is(test$state,"Status")) {
    state <- test$state
  } else {
    state <- parseStatus(test$state)
  }
  if (!is(state,"Status")) {
    stop("Problems parsing status for test ",name)
  }
  if (is(test$event,"Event")) {
    event <- test$event
  } else {
    event <- parseEvent(test$event)
  }
  if (!is(event,"Event")) {
    stop("Problems parsing event for test ",name)
  }
  if (is(test$rule,"Rule")) {
    rule <- test$rule
  } else {
    rule <- parseRule(test$rule)
  }
  if (!is(rule,"Rule")) {
    stop("Problems parsing rule for test ",name)
  }
  if (is(test$result,"Status")) {
    expected <- test$result
  } else {
    expected <- parseStatus(test$result)
  }
  if (!is(expected,"Status")) {
    stop("Problems parsing result for test ",name)
  }
  if (verbose) {
    cat("Testing rule ",name(rule),"\n")
  }
  actual <-try(executePredicate(predicate(rule),state,event))
  if (is(actual,"try-error")) {
    if (!quiet) {
      cat("Error occurred while running test",name)
    }
    if (verbose) {
      traceback(attr(actual,"condition"))
    }
    return (as.logical(NA))
  }
  result <- all.equal(expected,actual)
  if (!quiet) {
    cat("Test ",name,": ",ifelse(isTRUE(result),"Passed","Failed"),".\n")
    if (!isTRUE(result)) print(result)
  }
  return (isTRUE(result))
}

predicateTestScript <- function (filename,  quiet=FALSE, verbose=FALSE,
                            suiteName=basename(filename)) {
  script <- fromJSON(filename,FALSE)
  N <- length(script)
  result <- rep(as.logical(NA),N)
  names(result) <- paste(suiteName,1:N)
  for (i in 1:N) {
    test <- script[[i]]
    if (is.null(test$name)) {
      test$name <- paste(suiteName,i)
    } else {
      names(result)[i] <- test$name
    }
    if (is.null(test$doc)) test$doc <-
                             paste("Test",i,"in suite",suiteName,".")
    res <- withJavaLogging(predicateTest(test, quiet=quiet,
                                         verbose=verbose),
                           silentSuccess=!verbose,stopIsFatal=FALSE)
    result[i] <- res
    if (verbose) cat("\n\n")

  }
  if (!quiet) {
    cat("Test suite",suiteName,": ",N,"tests, ",
        sum(result,na.rm=TRUE), "passed, ",
        sum(!result,na.rm=TRUE), "failed, ",
        sum(is.na(result)), "errors.\n")
  }
  result
}


ruleTest <- function (test, quiet=FALSE, verbose=FALSE,
                      contextSet=NULL,
                      name=test$name) {
  if (verbose) {
    cat("Running test ",name,"\n")
    if (!is.null(test$doc)) print(test$doc)
  }
  if (is(test$state,"Status")) {
    state <- test$state
  } else {
    state <- parseStatus(test$state)
  }
  if (!is(state,"Status")) {
    stop("Problems parsing status for test ",name)
  }
  if (is(test$event,"Event")) {
    event <- test$event
  } else {
    event <- parseEvent(test$event)
  }
  if (!is(event,"Event")) {
    stop("Problems parsing event for test ",name)
  }
  if (is(test$rule,"Rule")) {
    rule <- test$rule
  } else {
    rule <- parseRule(test$rule)
  }
  if (!is(rule,"Rule")) {
    stop("Problems parsing rule for test ",name)
  }
  if (is(test$result,"Status")) {
    expected <- test$result
  } else {
    expected <- parseStatus(test$result)
  }
  if (!is(expected,"Status")) {
    stop("Problems parsing result for test ",name)
  }
  if (verbose) {
    cat("Testing rule ",name(rule),"\n")
  }
  ### Check the verb, object and context
  actual <- state                       #Default is no change.
  verbmatch <- verb(rule)=="ALL" | verb(rule)==verb(event)
  objmatch <- object(rule)=="ALL" | object(rule)==object(event)
  if (is.null(contextSet)) {
    contextmatch <- NA
  } else {
    conSet <- applicableContexts(matchContext(context(status),contextSet))
    contextmatch <- context(rule) %in% conSet
  }
  if (verbose) {
    cat("Verb match: ",verbmatch," Object match: ",objmatch,
        "Context match: ",contextmatch)
  }
  if (verbmatch && objmatch &&
      (is.na(contextmatch) || contextmatch)) {
    satisfied <-try(checkCondition(condition(rule),state,event))
    if (is(satisfied,"try-error")) {
      if (!quiet) {
        cat("Error occurred while running condition check for test ",name)
      }
      if (verbose) {
        traceback(attr(satisfied,"condition"))
      }
      return (as.logical(NA))
    }
    if (satisfied) {
      ## Actually run the predicate
      actual <-try(executePredicate(predicate(rule),state,event))
      if (is(actual,"try-error")) {
        if (!quiet) {
          cat("Error occurred while running predicate for test",name)
        }
        if (verbose) {
          traceback(attr(actual,"condition"))
        }
        return (as.logical(NA))
      }
    }
  }
  result <- all.equal(expected,actual)
  if (!quiet) {
    cat("Test ",name,": ",ifelse(isTRUE(result),"Passed","Failed"),".\n")
    if (!isTRUE(result)) print(result)
  }
  return (isTRUE(result))
}

ruleTestScript <- function (filename,  quiet=FALSE, verbose=FALSE,
                            contextSet=NULL,
                            suiteName=basename(filename)) {
  script <- fromJSON(filename,FALSE)
  N <- length(script)
  result <- rep(as.logical(NA),N)
  names(result) <- paste(suiteName,1:N)
  for (i in 1:N) {
    test <- script[[i]]
    if (is.null(test$name)) {
      test$name <- paste(suiteName,i)
    } else {
      names(result)[i] <- test$name
    }
    if (is.null(test$doc)) test$doc <-
                             paste("Test",i,"in suite",suiteName,".")
    res <- withJavaLogging(ruleTest(test, quiet=quiet,
                                    verbose=verbose,
                                    contextSet=contextSet),
                           silentSuccess=!verbose,stopIsFatal=FALSE)
    result[i] <- res
    if (verbose) cat("\n\n")

  }
  if (!quiet) {
    cat("Test suite",suiteName,": ",N,"tests, ",
        sum(result,na.rm=TRUE), "passed, ",
        sum(!result,na.rm=TRUE), "failed, ",
        sum(is.na(result)), "errors.\n")
  }
  result
}
