\name{matchContext}
\alias{matchContext}
\alias{matchContext,character,ContextSet-method}
\alias{matchContext,numeric,ContextSet-method}
\alias{matchContext,character,list-method}
\alias{matchContext,numeric,list-method}
\alias{updateContext}
\alias{updateContext,Context,ContextSet-method}
\alias{updateContext,Context,list-method}
\alias{clearContexts}
\alias{clearContexts,ContextSet-method}
\alias{clearContexts,list-method}
\title{Find or replace contexts in a context set}
\description{

  The function \code{matchContext} finds a context by string or numeric
  id.  The function \code{updateContext} adds a new context to a set or
  replaces an existing one (see details).  The function
  \code{clearContext} clears all contexts associated with this
  application. 
  
}
\usage{
matchContext(id, set)
\S4method{matchContext}{character,ContextSet}(id, set)
\S4method{matchContext}{numeric,ContextSet}(id, set)
\S4method{matchContext}{character,list}(id, set)
\S4method{matchContext}{numeric,list}(id, set)
updateContext(con, set)
\S4method{updateContext}{Context,ContextSet}(con, set)
\S4method{updateContext}{Context,list}(con, set)
clearContexts(set)
\S4method{clearContexts}{ContextSet}(set)
\S4method{clearContexts}{list}(set)
}
\arguments{
  \item{id}{This should be either a character or numeric scalar giving
    the \code{\link{cid}} or \code{\link{number}} of the desired
    context.} 
  \item{con}{This should be an object of type
    \code{\linkS4class{Context}} to be added or replaced.}
  \item{set}{This should either be an object of type
    \code{\linkS4class{ContextSet}} or a \code{list}.}
}
\details{

  The functions are for manipulating collections of contexts.  The
  robust way to implement a context set is to use the
  \code{\linkS4class{ContextSet}} class, which is an interface to
  database with dual indexes:  one on \code{\link{cid}} and
  \code{\link{app}} and one on \code{\link{number}} and \code{app}.
  Thus each application maintains its own set of contexts in the same
  database.

  A lightweight implementation can be made using a list with names
  corresponding to the \code{\link{cid}} and position in the list
  corresponding to \code{\link{number}}.  This representation is
  primarily intended for testing.

  The function \code{matchContext} is polymorphic on both its
  arguments.  If the first argument is character, it searches based on
  context id, if the first argument is numeric, it searches based on
  numeric id (position in the list if the second argument is a list).
  If no matching context is found, it returns \code{NULL}.  If the set
  is a \code{ContextSet} successes are logged at the DEBUG level, and
  failures at the INFO level.

  The function \code{updateContext} performs a database update
  operation, that is it replaces the context in the set if it already
  exists, or adds the context to the set if it does not.  If the
  \code{set} is a \code{\linkS4class{ContextSet}} it first checks for an
  existing context with that character and numeric id.  If such a
  context exists it is replaced; otherwise a new context is added.
  However, if the new character and numeric ids reference different
  contexts, then a warning is logged and the contexts are not replaced.
  If the \code{set} is a list, then no checking is done.  The context at
  the given numeric location is replaced and given the new name.  This
  may produce errors or an inconsistent state.

  The function \code{clearContexts} removes all of the contexts.  In the
  case of the list, it merely returns an empty list.  In the case of the
  \code{\linkS4class{ContextSet}} it removes all contexts with the
  application id of the set.  Other applications should remain
  untouched.

}
\value{

  The function \code{matchContext} returns an object of class
  \code{\linkS4class{Context}} if a class was found and \code{NULL}
  otherwise.

  The functions \code{updateContext} and \code{clearContexts} return the
  (modified) \code{set} argument.

}
\author{Russell Almond}
\note{

  Note that these functions obey functional or reference semantics
  depending on the \code{set} argument.  As
  \code{\linkS4class{ContextSet}} is a reference class, these functions
  modify the database and hence the results are propagated.  However, if
  \code{set} is a list, then normal functional semantics are used, and
  the resulting modified list needs to be stored in an environment to
  make the modification permanent.

}
\seealso{

  \code{\linkS4class{Context}}, \code{\linkS4class{ContextSet}},
  \code{\link{loadContexts}}
}
\examples{
conmat <- read.csv(file.path(library(help="EIEvent")$path,"testScripts",
                "SampleContextSheet.csv"))

conmat1 <- conmat
conmat1$Number <- 1:nrow(conmat) ## List style needs small integer indexes. 
setlist <- loadContexts(conmat1,list(),"ecd://epls.coe.fsu.edu/rga/test")
stopifnot(all.equal(names(setlist),as.character(conmat$CID)))

matchContext(3L,setlist)
matchContext("Stairs",setlist)


testSet <- ContextSet$new(app="ecd://epls.coe.fsu.edu/rga/test",
                      dbname="test",dburi="mongodb://localhost")

clearContexts(testSet)
loadContexts(conmat,testSet,"ecd://epls.coe.fsu.edu/rga/test")
matchContext(28L,testSet)
matchContext("Stairs",testSet)

}
\keyword{ manip }
\keyword{ interface }
