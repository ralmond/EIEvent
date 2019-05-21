\name{cid}
\alias{cid}
\alias{cid,Context-method}
\alias{number}
\alias{number,Context-method}
\alias{number<-}
\alias{number<-,Context-method}
\alias{app,Context-method}
\title{Accessor functions for context objects.}
\description{
  
  These functions access the corresponding fields of the
  \code{\linkS4class{Context}} class.

}
\usage{
cid(c)
\S4method{cid}{Context}(c)
number(c)
\S4method{number}{Context}(c)
number(c) <- value
\S4method{number}{Context}(c) <- value
\S4method{app}{Context}(x)
}
\arguments{
  \item{c}{A \code{\link{Context}} object.}
  \item{x}{A \code{\link{Context}} object.}
  \item{value}{An integer giving the new context number.}
}
\value{

  The function \code{cid} returns a unique string identifier for the
  context.  The function \code{number} returns a unique integer
  identifier.  The function \code{app} returns the application
  identifier.  The \code{cid} and \code{number} should be unique within
  the \code{app}.

}
\references{

  The document \dQuote{Rules Of Evidence} gives extensive documentation
  for the JSON layout of the Status/State objects.
  \url{https://pluto.coe.fsu.edu/Proc4/RulesOfEvidence.pdf}. 

  Almond, R. G., Steinberg, L. S., and Mislevy, R.J. (2002).  Enhancing
  the design and delivery of Assessment Systems:  A Four-Process
  Architecture.  \emph{Journal of Technology, Learning, and Assessment},
  \bold{1},
  \url{http://ejournals.bc.edu/ojs/index.php/jtla/article/view/1671}. 

  Almond, R. G., Shute, V. J., Tingir, S. and Rahimi,
  S. (2018). Identifying Observable Outcomes in Game-Based Assessments.
  Talk given at the \emph{2018 Maryland Assessment Research
  Conference}.  Slides:
  \url{https://education.umd.edu/file/11333/download?token=kmOIVIwi},
  Video: \url{https://pluto.coe.fsu.edu/Proc4/Almond-Marc18.mp4}.

  Betts, B, and Smith, R. (2018). The Leraning Technology Manager's Guid
  to {xAPI}, Second Edition.  HT2Labs Research Report:
  \url{https://www.ht2labs.com/resources/the-learning-technology-managers-guide-to-the-xapi/#gf_26}.

  HT2Labs (2018). Learning Locker Documentation.
  \url{https://docs.learninglocker.net/welcome/}.
}

\author{Russell Almond}
\seealso{

  \code{\linkS4class{Context}} describes the context object.  The
  function \code{\link{applicableContexts}} describes the context
  matching logic.

}
\examples{

ct <- Context("Level1","First Tutorial",1,
              belongsTo=c("tutorialLevels","easyLevels"),
              doc="First Introductory Level",
              app="ecd://epls.coe.fsu.edu/EITest")

stopifnot(cid(ct)=="Level1",basename(app(ct))=="EITest",
          number(ct)==1L)

number(ct) <- 0
stopifnot(number(ct)==0L)


}
\keyword{ manip }

