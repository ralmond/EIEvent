\name{doLoad}
\alias{doLoad}
\title{Construct an Engine and Load the Rules.}
\description{

    This function reads a configuration file (config.json) and then
    loads the referenced rules into the database.  The assumption is
    that the `config.json` will be in git (or other SCCS) system along
    with the rules expressed in json notation.  The referenced rules
    are then loaded for the specified app.

}
\usage{
doLoad(app, EI.config, EIeng.local, config.dir, override = FALSE)
}
\arguments{
  \item{app}{A string giving the identifier for the application whose
      rules are loaded.}
  \item{EI.config}{A list giving details about the EI Event
      environment.  This is usually the content of the `config.json`
      file.  See details.}
  \item{EIeng.local}{A list giving local (machine specific)
    configuration information.}
  \item{config.dir}{A string giving the location of the configuation
    directory, where the rule files are located.}
  \item{override}{A logical flag.  If true, then the rules are loaded
    even if there currently exists a lock file to indicate that they are
    being used.}
}
\details{

  The \code{linkS4class{EIEngine}} runs against a collection of
  \code{\linkS4class{Rule}} objects.  In the current implementation, the
  rules are kept in a mongo database (`EIRecords.Rules`).  In addition
  to the rules, there is a table of \code{\link{Context}}s which
  describe collections of tasks (game levels) to which rules should
  apply. Finally, a default student record with proper initial values
  needs to be created for the application.

  Generally, the rules are coded using a `json` formatted text file,
  which is parsed and loaded into the database.  These are stored in the
  directory \code{file.path(config.dir,EI.config$ruledir)} (usually the
  \dQuote{Rules} directory in the configuration file).  The
  configuration value \code{EI.config$rules} is character vector giving
  the names of the rule files (without the \dQuote{json} suffix).  This
  is passed to the \code{\link{loadRulesFromList}} function. The
  configuration value \code{EI.config$rulesWithTests} gives the name of
  rule files that come with embedded tests.  These use the
  \code{\link{testAndLoad}} function instead.

  In addition to the rule files, the \code{\link{Context}}s must be
  specified.  These are loaded from tables (stored in CSV files) where
  the rows correspond to tasks and the columns correspond to higher
  level contexts that specific tasks might belong to.  This is used to
  create the collection of contexts in the database.  See
  \code{\link{loadContexts}} for details.  The value
  \code{EI.config$contextDescriptions} gives a character vector
  containing the name (minus the \code{.csv} extension) of the context
  description files.

  Finally, the loader creates a default student record.  This allows
  certain fields (which count across the whole assessment) to be
  initialized.  In Physics playground, the initial back balance is set
  to zero and the trophy list is initialized to the empty list.  The
  mechanism is that the \code{doLoad} function creates a new user record
  called \dQuote{*DEFAULT*} and sets its initial observables to the
  values in the named list \code{EI.config$defaultRecordData}.  This is
  then stored in the database.  In the future, new student records are
  created by cloning the \dQuote{*DEFAULT*} record.
  


}
\value{

  This function is called for its side effects, and the return value
  should not be used.

}
\references{
  The document \dQuote{Rules Of Evidence} gives extensive documentation
  for the rule system.
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

  MongoDB, Inc. (2018).  \emph{The MongoDB 4.0 Manual}.
  \url{https://docs.mongodb.com/manual/}.
}
\author{Russell Almond}
\note{

  The Github project \url{https://github.com/ralmond/PP-EI/} contains
  and example set of configuration files used for \emph{Physics
  Playground}. 

  This function is meant to be called by the \code{EILoad.R} script
  found in the config directory.
  (\code{file.path(help(package="EIEvent")$path,"conf","EILoad.R")})

  The shell script \code{EILoader} found in the same directory will run this
  script.  



}

\seealso{
  \code{\linkS4class{EIEngine}}, \code{\link{doRunrun}},
  \code{\link{loadContexts}}, \code{\link{loadRulesFromList}},
  \code{\link{testAndLoad}}
}
\examples{

## This script is available in the conf directory
file.path(help(package="EIEvent")$path,"conf","EILoad.R")

\dontrun{
library(utils)
library(EIEvent)

## It is assumed that EIini.R sets the value of
## config.dir, EIeng.local and Proc4.config
source("/usr/local/share/Proc4/EIini.R")

EI.config <- fromJSON(file.path(config.dir,"config.json"),FALSE)

## appStem is a short nickname for the app
## Proc4.config$apps contains the full names indexed by the nicknames.

appStem <- as.character(EI.config$appStem)

apps <- as.character(Proc4.config$apps[appStem])
if (length(apps)==0L || any(apps=="NULL")) {
  stop("Could not find apps for ",appStem)
}

ruledir <- ifelse(!is.null(EI.config$ruledir),
                  EI.config$ruledir,"Rules")

lockfile <- file.path(config.dir,ruledir,"ruleloader.lock")
file.create(lockfile)
logfile <- (file.path(logpath, sub("<app>","Loader",EI.config$logname)))
if (interactive()) {
  flog.appender(appender.tee(logfile))
} else {
  flog.appender(appender.file(logfile))
}
flog.threshold(EI.config$loglevel)


## Loop over apps
for (app in apps)
  doLoad(app, EI.config, EIeng.local, config.dir)
unlink(lockfile)
}

}
\keyword{ interface }
\keyword{ manip }

