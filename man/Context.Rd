\name{Context}
\alias{Context}
\alias{parseContext}
\alias{as.jlist,Context,list-method}
\alias{show,Context-method}
\description{

  This is the constructor for the \code{\linkS4class{Context}} objects
  and context set objects (which are identical).  As Context objects are
  usually read from a database or other input stream, the
  \code{parseContext} function is recreates an event from a JSON list
  and \code{\link[Proc4]{as.jlist}} encodes them into a list to be saved
  as JSON.

}
\usage{
Context(cid, name, number, belongsTo = character(), doc = "", app="default")
parseContext(rec)
\S4method{as.jlist}{Context,list}(obj, ml, serialize = TRUE)
}
\arguments{
  \item{cid}{A character identifier for the context.  Should be unique
    within an application (\code{app}).}
  \item{name}{A human readable name, used in documentation.}
  \item{number}{A numeric identifier for the context.}
  \item{belongsTo}{A character vector describing context sets
    this context belongs to.}
  \item{doc}{A character vector providing a description of the context.}
  \item{app}{A character scalar providing a unique identifier for the
    application.}
  \item{rec}{A named list containing JSON data.}
  \item{obj}{An object of class \code{\linkS4class{Context}} to be
    encoded.}
  \item{ml}{A list of fields of \code{obj}.  Usually, this is created by
    using \code{\link[base]{attributes}(obj)}.}
  \item{serialize}{A logical flag. If true,
    \code{\link[jsonlite]{serializeJSON}} is used to protect the
    \code{data} field (and other objects which might contain complex R
    code).} 
}
\details{

  Most of the details about the \code{\linkS4class{Context}} object, and
  how it works is documented under \link{Context-class}.  Note that
  context sets and contexts are represented with the same object.
  
  The function \code{as.jlist} converts the \code{obj} into a named
  list. It is usually called from the function
  \code{\link[Proc4]{as.json}}.  

  The \code{parseContext} function is the inverse of \code{as.jlist}
  applied to a context object.  It is designed to be given as an argument
  to \code{\link[Proc4]{getOneRec}} and 
  \code{\link[Proc4]{getManyRecs}}.   

}
\value{

  The functions \code{Context} and \code{parseContext} return objects of
  class \code{\linkS4class{Context}}.  The function \code{as.jlist}
  produces a named list suitable for passing to
  \code{\link[jsonlite]{toJSON}}. 

}
\references{

  The document \dQuote{Rules Of Evidence} gives extensive documentation
  for the context system:
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

  \code{\linkS4class{Context}} describes the context object,  and
  \code{\linkS4class{ContextSet}} describes the context set object.

  \code{\link[Proc4]{parseMessage}} and \code{\link[Proc4]{as.json}}
  describe the JSON conversion system.  

  The functions \code{\link[Proc4]{getOneRec}} and 
  \code{\link[Proc4]{getManyRecs}} use \code{parseStatus} to extract
  events from a database.

}
\examples{
ct <- Context("Level1","First Tutorial",1,
              belongsTo=c("tutorialLevels","easyLevels"),
              doc="First Introductory Level",
              app="ecd://epls.coe.fsu.edu/EITest")

cta <- parseContext(as.jlist(ct,attributes(ct)))
stopifnot(all.equal(ct,cta))


}
\keyword{ interface }
\keyword{ database }

