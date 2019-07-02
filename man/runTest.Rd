\name{runTest}
\alias{runTest}
\title{Runs a test case with a given set of rules}
\description{

  This function runs a test case in the context of a particular
  application.  In particular, it checks to make sure that the rules are
  behaving as expected.

}
\usage{
runTest(eng, test)
}
\arguments{
  \item{eng}{An object of class \code{\linkS4class{EIEngine}} which provides
    the testing environment.}
  \item{test}{An object of class \code{\linkS4class{EITest}} which describes
    the test.}
}
\details{

  An \code{\linkS4class{EITest}} object contains an
  \code{\link{initial}} \code{\linkS4class{Status}}, an
  \code{\linkS4class{Event}} and a result field which could either be a
  final \code{Status} or a \code{\link[Proc4]{P4Message}} that could be
  sent by a trigger rule.  The function \code{runTest} processes this
  event with the given initial status and checks the actual result
  agains the expected result.

}
\value{

  The function returns \code{TRUE} if the event was handled without
  error and the result matched the expected value, \code{FALSE} if the
  event was handled without error, but the result did not match, and
  \code{NA} if handling the event (or comparing the result to the
  expected result) produced an error.

  Note that errors are logged using \code{\link{withFlogging}} which
  will provide details about errors.

}
\references{
%% ~put references to the literature/web site here ~
}
\author{Russell Almond}
\note{

  This function is pretty much untested.  This is a planned expansion.
}
\seealso{

  See \code{\link{testRule}} for the testing functions that are
  currently implemented.

  See \code{\linkS4class{EITest}} for the test class.

}
\examples{
## The function is currently defined as
function (eng, test) 
{
    cl <- new("CaptureListener")
    eng$ListenerSet$addListener(name(test), cl)
    flog.info("Running Test \%s", name(test))
    flog.debug("Details:", doc(test), capture = TRUE)
    result <- NA
    withFlogging({
        actual <- eng$testRules(initial(test), event(test))
        if (is(final(test), "P4Message")) {
            actual <- cl$lastMessage()
        }
        else if (is(final(test), "list")) {
            actual <- cl@messages
        }
        result <- all.equal(final(test), actual)
        if (!isTRUE(result)) {
            flog.info("Test \%s failed.", name(test))
            flog.info("Details:", result, capture = TRUE)
            flog.debug("Actual Status/Message:", actual, capture = TRUE)
            result <- FALSE
        }
    }, context = paste("Running Test", name(test)), test = test)
    eng$ListenerSet$removeListener(name(test))
    result
  }
}
\keyword{ utilities }
\keyword{ programming }
