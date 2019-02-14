\name{loadContexts}
\alias{loadContexts}
\title{Loads a set of contexts from a matrix description}
\description{

  The easiest way to load a set of \code{\linkS4class{Context}} objects
  is from a matrix of cross references.  There are columns corresponding
  to the fixed fields of the context object (\code{cid},\code{number},
  \code{name} and \code{doc}).  The other columns are logical variables
  that refer to context sets, which take on a true value when the
  context represented by the row belongs to the set represented by the
  column. 

}
\usage{
loadContexts(conmat, set, app)
}
\arguments{
  \item{conmat}{A matrix representing a collection of contexts, see details.}
  \item{set}{Either a \code{\linkS4class{ContextSet}} object, or a list
    to which the new contexts will be added.}
  \item{app}{A character scalar giving the name of the application; used
    in constructing the contexts.}
}
\details{

  A context matrix is a \code{\link[base]{data.frame}} with the
  following columns: \code{cid} (character, required),
  \code{number} (numeric, required),  \code{name} (character, optional)
  and \code{doc} (character, required).  The \code{cid} and
  \code{number} fields should also be unique across all the entries as
  they serve as indexes for the \code{\linkS4class{ContextSet}} object.
  The \code{name} column if omitted defaults to the \code{cid} (the
  difference is that the name is designed to be more human readable).
  The \code{doc} if omitted produces an empty documentation string.  
  The remaining columns are logical and are indicators for set
  membership.

  The following is an example context matrix:
  \tabular{lllllrrr}{
       \tab \bold{cid} \tab \bold{number} \tab \bold{name} \tab
         \bold{doc} \tab \bold{Set1} \tab \bold{Subset1.1} \tab
         \bold{Set2} \cr
    [,1] \tab Set1 \tab -100 \tab "Set 1" \tab "A context set" \tab
         0 \tab 0 \tab 0 \cr
    [,2] \tab Subset1.1 \tab -110 \tab "Subet 1.1" \tab
         "A subset of Set 1" \tab 1 \tab 0 \tab 0 \cr
    [,3] \tab Set2 \tab -200 \tab "Set 2" \tab "Another context set" \tab
         0 \tab 0 \tab 0 \cr
    [,4] \tab Task1 \tab 100 \tab "First Task" \tab
         "A task beloning to Set 1" \tab 1 \tab 0 \tab 0 \cr
    [,5] \tab Task1a \tab 101 \tab "First Task Variant" \tab
         "A task beloning to Subet 1.1" \tab 1 \tab 1 \tab 0 \cr
    [,6] \tab Task2 \tab 200 \tab "Second Task" \tab
         "A task beloning to Set 2" \tab 0 \tab 0 \tab 1 \cr
       }
  
  A couple things to note about this matrix.  First, both actual
  contexts and context sets are represented (the are both represented
  internally by \code{\link{Context}} objects.  The names of the extra
  columns correspond to the names of the context sets.  If the database
  is used to store contexts, it recommended to give sets a negative
  number and actual contexts positive numbers.  If a list is used, then
  the numbers need to be list indexes, and so should be low numbers.

  Also, the \code{\link{belongsTo}} relationship is not transitive (or
  at least the transitive implications are not computed), so Task1a must
  have both set and subset memebership defined.

  The code loops through the rows of the table, and creates a new
  context for each row.  This is then added to the \code{set} argument.
  If an existing context of the same cid and number exists, it is
  replaced, otherwise a new one is added.  If the context is replacing
  an old one, the old one should have both the same name and number as
  the replacement.  The method for \code{\link{updateContext}} for a
  \code{\linkS4class{ContextSet}} object (called by
  \code{loadContexts}) checks for this inconsistency;  the list method
  does not.

  The matrix is to represent the entire context set, calling
  \code{\link{clearContexts}} before loading the matrix should eliminate
  conflicts. 

}
\value{

  The modified \code{set} argument is returned.  This is either a
  \code{list} or a \code{\linkS4class{ContextSet}}.

}
\references{

  The document \dQuote{Rules Of Evidence} gives extensive documentation
  for the context system:
  \url{https://pluto.coe.fsu.edu/Proc4/RulesOfEvidence.pdf}. 


}
\author{Russell Almond}
\note{

  The \code{\linkS4class{ContextSet}} class is a reference class.
  Therefore, this function destructively modifieds the database
  associated with the set.

  The \code{list} is an ordinary R functional class, and the function
  behaves in a functional manner when called with a list as a set.

}
\seealso{
  \code{\linkS4class{Context}} describes the context object, and
  \code{\linkS4class{ContextSet}} describes the context set object.

  The functions \code{\link{matchContext}}, \code{\link{updateContext}}
  and \code{\link{clearContexts}} are used to manipulate context sets.
  
}
\examples{

conmat <- read.csv(file.path(library(help="EIEvent")$path,"testScripts",
                "SampleContextSheet.csv"))

conmat1 <- conmat
conmat1$Number <- 1:nrow(conmat) ## List style needs small integer indexes. 
setlist <- loadContexts(conmat1,list(),"ecd://epls.coe.fsu.edu/rga/test")
stopifnot(all.equal(names(setlist),as.character(conmat$CID)))

testSet <- ContextSet$new(app="ecd://epls.coe.fsu.edu/rga/test",
                      dbname="test",dburi="mongodb://localhost")

clearContexts(testSet)
loadContexts(conmat,testSet,"ecd://epls.coe.fsu.edu/rga/test")
matchContext(28L,testSet)
matchContext("Stairs",testSet)


}
\keyword{ manip}
\keyword{ interface }
