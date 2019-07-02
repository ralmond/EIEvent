\name{loadRulesFromList}
\alias{loadRulesFromList}
\alias{testAndLoad}
\title{Functions for loading rules into database.}
\description{

  These functions load rules into a \code{\linkS4class{RuleTable}} (or
  the database for a \code{\linkS4class{EIEngine}}).  The function
  \code{testAndLoad} also runs the test suite as the rules are loaded.
  
}
\usage{
loadRulesFromList(set, rulelist, stopOnDups = TRUE)
testAndLoad(set, filename, stopOnDups = FALSE)
}
\arguments{
  \item{set}{An object of class \code{\linkS4class{RuleTable}} into
    which the rules will be loaded.}
  \item{rulelist}{A list of \code{\linkS4class{Rule}} objects.}
  \item{stopOnDups}{A logical value which controls how names with
    duplicate values will be treated.  If true, then an error will be
    signaled if the rule to be added has the same name as one in the
    database.  If false, the old rule will be replaced.}
  \item{filename}{A file which contains a number of
    \code{\linkS4class{RuleTest}} objects in JSON format (see
    \code{\link{testRule}}).  The tests will be run, and if successful
    the rules will be extracted and loaded.}
}
\details{

  The \code{loadRulesFromList} function is more often called from the
  \code{$loadRules()} method of the \code{\linkS4class{EIEngine}}
  class.  The easiest way to maintain the rules is by a series of one or
  more JSON files.  These can be read using:

  \code{rulelist <- lapply(fromJSON("rulefile.json",FALSE), parseRule)}

  With \code{stopOnDups==FALSE}, the functions will issue warnings when
  they are replacing rules, especially if the versions are different.
  However, it is probably easier to first clear out the old rules when
  replacing the rules.  This can be done with the \code{$clearAll()}
  method of the \code{\linkS4class{RuleTable}} class or the the
  \code{$clearAllRules()} method of the \code{\linkS4class{EIEngine}}
  class.

  The \code{testAndLoad} variant is not yet fully tested, but the idea
  is that the file would be a test script of the kind that could be
  tested with \code{\link{testRuleScript}}.  It will run the tests, and
  load the rules only if the tests pass.  (Note that as a rule could
  have multiple tests, it will be loaded if any of the tests pass.)

}
\value{

  Both of these function return the \code{set} argument invisibly.
}
\references{

  The document \dQuote{Rules Of Evidence} gives extensive documentation
  for the rule system.
  \url{https://pluto.coe.fsu.edu/Proc4/RulesOfEvidence.pdf}. 

  There is a \dQuote{quick start} document which describes how to set up the
  engine.  This is available at 
  \url{https://pluto.coe.fsu.edu/Proc4/EIQuickStart.pdf}. 

}
\author{Russell Almond}
\note{

  The \code{testAndLoad} variant is not fully tested.

}
\seealso{

  See \code{\linkS4class{Rule}} and \code{\link{parseRule}} for the rule
  object.  See \code{\linkS4class{RuleTable}} for the rule collection
  object and \code{\linkS4class{EIEngine}} for the full engine.

}
\examples{

\dontrun{
rls <- RuleTable$new("ecd://epls.coe.fsu.edu/P4test","test",
                     "mongodb://localhost")
sRules <- lapply(fromJSON(file.path(config.dir, "Rules",
                                    "CombinedRules.json"),FALSE),
                 parseRule)
rls$clearAll()
loadRulesFromList(rls,sRules)
}
}
\keyword{ utilities }
\keyword{ database }
