## This file checks to see if the query part of a rule is satisified.

checkCondition <- function (conditions, state, event) {
  for (field in names(conditions)) {
    if (field == "?where") {
      if (!do.call(condition[[field]],list(state,event)))
        return (FALSE)
    }
    fieldexp <- strsplit(field,".")[[1]]
    if (fieldexp[1]=="state") target <- state
    if (fieldexp[1]=="event") target <- event
    else stop("Field name must start with 'state' or 'event':",field)
    for (i in 2:length(feildexp)) {
      target <- target[[field]]
    }
    if (!checkOneCondition(conditions[[field]],target))
      return(FALSE)
  }
  return TRUE
}

checkOneCondition <- function(condition,target) {
  if (is.list(condition) && !is.null(names(condition))) {
    for (iop in 1:length(condition)) {
      if (!do.call(names(condition)[iop],list(condition[[iop]],target)))
        return FALSE
    }
    return TRUE
  } else {
    if (is.double(condition) && is.double(target)) {
      any(abs(condition-target)<.0001)
    } else {
      any(condition==target)
    }
  }
}

"?eq" <- function (cond,target) { target == cond}
"?ne" <- function (cond,target) { target != cond}
"?gt" <- function (cond,target) { target > cond}
"?gte" <- function (cond,target) { target >= cond}
"?lt" <- function (cond,target) { target < cond}
"?le" <- function (cond,target) { target <= cond}

"?in" <- function (cond,target) { target %in% cond}
"?nin" <- function (cond,target) { !(target %in% cond)}


"?exists" <- function (cond,target) {!is.null(target) == cond}
"?isnull" <- function (cond,target) {is.null(target) == cond}
"?isna" <- function (cond,target) {is.na(target) == cond}

"?regexp" <- function (cond,target) {grepl(cond,target)}


## These assume that target is a vector and apply the test repeatedly.
"?any" <- function (cond,target) {
  if (length(cond) >1L) stop("?any queries must have length 1.")
  subquery <- names(cond)[1]
  subcond <- cond[[1]]
  any(do.call(subquery,list(subcond,target)))
}
"?all" <- function (cond,target) {
  if (length(cond) >1L) stop("?all queries must have length 1.")
  subquery <- names(cond)[1]
  subcond <- cond[[1]]
  all(do.call(subquery,list(subcond,target)))
}


## These combine other tests.
"?not" <- function (cond,target) {
  if (length(cond) >1L) stop("?all queries must have length 1.")
  subquery <- names(cond)[1]
  subcond <- cond[[1]]
  !do.call(subquery,list(subcond,target))
}
"?and" <- function (cond,target) {
  for (iop in 1:length(cond)) {
    subquery <- names(cond)[i]
    subcond <- cond[[i]]
    if (!do.call(subquery,list(subcond,target)))
      return FALSE
  }
  return TRUE

}
"?or" <- function (cond,target) {
  for (iop in 1:length(cond)) {
    subquery <- names(cond)[i]
    subcond <- cond[[i]]
    if (do.call(subquery,list(subcond,target)))
      return TRUE
  }
  return FALSE
}

