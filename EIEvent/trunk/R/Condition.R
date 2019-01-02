## This file checks to see if the query part of a rule is satisified.

checkCondition <- function (conditions, state, event) {
  for (field in names(conditions)) {
    if (field == "?where") {
      return (do.call(conditions[[field]],list(state,event)))
    }
    target <- getJS(field,state,event)
    if (!checkOneCondition(conditions[[field]],target,state,event))
      return(FALSE)
  }
  return(TRUE)
}

checkOneCondition <- function(condition,target,state,event) {
  if ((is.vector(condition) || is.list(condition))
      && !is.null(names(condition))) {
    for (iop in 1:length(condition)) {
      argi <- condition[[iop]]
      op <- names(condition)[iop]
      if (!do.call(op,list(argi,target,state,event)))
        return(FALSE)
    }
    return (TRUE)
  } else {
    ## ?in and ?eq are equivalent if length(condition)==1
    return (do.call("?in",
                    list(condition,target,state,event)))
  }
}

"?eq" <- function (arg,target,state,event) {
  if (is.character(arg) &&
      all(grepl("^(state|event)\\.",arg) )) {
    arg <- lapply(arg, function (value)
      getJS(value,state,event))
  }
  if (is.double(arg) && is.double(target)) {
    abs(arg-target) < .0001
  } else {
    target == arg
  }
}
"?ne" <- function (arg,target,state,event) {
  if (is.character(arg) &&
      all(grepl("^(state|event)\\.",arg) )) {
    arg <- lapply(arg, function (value)
      getJS(value,state,event))
  }
  if (is.double(arg) && is.double(target)) {
    abs(arg-target) > .0001
  } else {
    target != arg
  }
}
"?gt" <- function (arg,target,state,event) {
  if (is.character(arg) &&
      all(grepl("^(state|event)\\.",arg) )) {
    arg <- lapply(arg, function (value)
      getJS(value,state,event))
  }
  target > arg
}
"?gte" <- function (arg,target,state,event) {
  if (is.character(arg) &&
      all(grepl("^(state|event)\\.",arg) )) {
    arg <- lapply(arg, function (value)
      getJS(value,state,event))
  }
  target >= arg
}
"?lt" <- function (arg,target,state,event) {
  if (is.character(arg) &&
      all(grepl("^(state|event)\\.",arg) )) {
    arg <- lapply(arg, function (value)
      getJS(value,state,event))
  }
  target < arg
}
"?lte" <- function (arg,target,state,event) {
  if (is.character(arg) &&
      all(grepl("^(state|event)\\.",arg) )) {
    arg <- lapply(arg, function (value)
      getJS(value,state,event))
  }
  target <= arg
}

"?in" <- function (arg,target,state,event) {
    if (is.character(arg) &&
      all(grepl("^(state|event)\\.",arg) )) {
    arg <- lapply(arg, function (value)
      getJS(value,state,event))
  }
    target %in% arg
}
"?nin" <- function (arg,target,state,event) {
  if (is.character(arg) &&
      all(grepl("^(state|event)\\.",arg) )) {
    arg <- lapply(arg, function (value)
      getJS(value,state,event))
  }
  !(target %in% arg)
}


"?exists" <- function (arg,target,state,event) {
  if (is.character(arg) &&
      all(grepl("^(state|event)\\.",arg) )) {
    arg <- lapply(arg, function (value)
      getJS(value,state,event))
  }
  !is.null(target) == arg
}
"?isnull" <- function (arg,target,state,event) {
  if (is.character(arg) &&
      all(grepl("^(state|event)\\.",arg) )) {
    arg <- lapply(arg, function (value)
      getJS(value,state,event))
  }
  is.null(target) == arg
}
"?isna" <- function (arg,target,state,event) {
  if (is.character(arg) &&
      all(grepl("^(state|event)\\.",arg) )) {
    arg <- lapply(arg, function (value)
      getJS(value,state,event))
  }
  is.na(target) == arg
}

"?regexp" <- function (arg,target,state,event) {
  if (is.character(arg) &&
      all(grepl("^(state|event)\\.",arg) )) {
    arg <- lapply(arg, function (value)
      getJS(value,state,event))
  }
  grepl(arg,target)
}


## These assume that target is a vector and apply the test repeatedly.
"?any" <- function (arg,target,state,event) {
  if (length(arg) >1L) stop("?any queries must have length 1.")
  subquery <- names(arg)[1]
  subarg <- arg[[1]]
  any(do.call(subquery,list(subarg,target,state,event)))
}
"?all" <- function (arg,target,state,event) {
  if (length(arg) >1L) stop("?all queries must have length 1.")
  subquery <- names(arg)[1]
  subarg <- arg[[1]]
  all(do.call(subquery,list(subarg,target,state,event)))
}


## These combine other tests.
"?not" <- function (arg,target,state,event) {
  if (length(arg) >1L) stop("?not queries must have length 1.")
  subquery <- names(arg)[1]
  subarg <- arg[[1]]
  !do.call(subquery,list(subarg,target,state,event))
}
"?and" <- function (arg,target,state,event) {
  for (iop in 1:length(arg)) {
    subquery <- names(arg)[iop]
    subarg <- arg[[iop]]
    if (!do.call(subquery,list(subarg,target,state,event)))
      return (FALSE)
  }
  return (TRUE)

}
"?or" <- function (arg,target,state,event) {
  for (iop in 1:length(arg)) {
    subquery <- names(arg)[iop]
    subarg <- arg[[iop]]
    if (do.call(subquery,list(subarg,target,state,event)))
      return (TRUE)
  }
  return (FALSE)
}

