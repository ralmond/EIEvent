\name{doRunrun}
\alias{doRunrun}
\title{Runs the EIEvent engine using the supplied configuration.}
\description{

  This is a system to run the evidence identifiation engine, taking most of
  the details from a configuration file.  It creates the
  \code{\linkS4class{EIEngine}} 
  instance then and then runs it in either scoring or rescoring mode.
  Configuration information in taken from the \code{EI.config} and
  \code{EIeng.local} parameters.
  
}
\usage{
doRunrun(app, EI.config, EIeng.local, config.dir, outdir = config.dir,
override = FALSE, logfile = "", noprep = FALSE)
}
\arguments{
  \item{app}{A character string giving the global unique identifier for
    the application being run.  This is normally formatted like a URL,
    and \code{basename(app)} is used as a short name.}
  \item{EI.config}{A named list containing the configuration details.  See the
    \sQuote{Configuration} section below.}
  \item{EIeng.local}{A named list containing additional parameters for the
    engine constructor.  The intention that these are local
    configuraiton paramete (e.g., database names and passwords) as
    opposed to more global information.  Note this must have an element
    named \dQuote{dburi} which gives the URI for the database.}
  \item{config.dir}{The pathname of the directory that contains the
    the rules subdirectories.}
  \item{outdir}{The pathname of the directory to which output files
    will be written.}
  \item{override}{A logical flag.  If true, the code will ignore locks
    and restart the run anyway.}
  \item{logfile}{Name for the file in which to do logging.}
  \item{noprep}{A logical flag.  If this value is true then the database
    preparation operations will be skipped.  This is intended for
    continuing a run which was interrupted for some reason.}
}
\details{

  The goal is to start a run for scoring (evidence identificaiton step)
  an assessment using the  
  \code{\linkS4class{EIEngine}} class.  This function takes care of many
  of the configuration details and preparatory steps and then calls
  \code{\link{mainLoop}} to do the major work.  In particular, the steps
  done by this system are as follows:

  \enumerate{
    \item{Configure the listeners.}
    \item{Configure the engine.}
    \item{Clean old scores from the database (optional depending on
      configuration.)}
    \item{Remove selected events from the collection. Import new
      evidence sets into the database and mark selected evidence as 
      unprocessed. (This step is skipped if the \code{noprep} flag is
      true.)}  
    \item{Launch engine using \code{\link{mainLoop}}.}
    \item{Build and register the obserables output file.}
  }

  Note that this will run in either rerun mode, where it will score an
  selection of existing events and stop, or in server mode where it
  will continue waiting for new messages until it gets a shut down signal.

  
}
\section{Configuration}{

  There are a large number of parameters which can be configured.  These
  are passed in through the \code{EI.config} argument, which is a list
  of parameters.  The intention is that this can be read in from a JSON
  file (using \code{\link[jsonlite]{fromJSON}}).  The \code{RunEIEvent.R}
  script loads these from a file called \code{config.json}.  A sample of
  this file is available on github
  \url{https://github.com/ralmond/PP-EI}.  The idea is that an entire
  configuration directory can be stored in a source code control system
  to manage the configuration process.


  The following fields are available:

  \describe{

    \item{ConfigName}{An identifier for the configuration. Default value
    \code{"PP-main"}.  Documentation only, not used by \code{doRunrun}.} 
    \item{Branch}{The branch name for the git branch for this
      configuraiton.  Default value \code{"PP-main"}. Documentation
      only, not used by \code{doRunrun}. }
    \item{Version}{A version number for the configuration. Documentation
      only, not used by \code{doRunrun}. }
    \item{Date}{A edit date for the configuration. Documentation only,
      not used by \code{doRunrun}. }
    \item{appStem}{A charcter vector of app stems that will be affected.  Sample
      value \code{["P4Test"]}.  This should be the result of applying
    \code{\link[base]{basename}} to the longer application IDs.} 
    \item{logLevel}{This controls the
      \code{\link[futile.logger]{flog.threshold}}. Default value
      \code{"INFO"}.  Note that \code{doRunrun} does not set the log
      value, that should be done in the calling script.}
    \item{logname}{This is the name of the file to which logs should be
      sent. Example value \code{"EA_<app>0.log"}. Note that
      \code{doRunrun} does not set the log 
      file, that should be done in the calling script.}
    \item{sender}{The sender field on output messages.  Example value
      \code{"EA_<app>"}.} 
    \item{ruledir}{The directory in which rule files are found.  This is
      only read at load time; see \code{\link{doLoad}}.}
    \item{contextDescriptions}{A character vector giving the names of
      context description files (minus the \code{.csv} extension) in the
      \code{ruledir} directory.  see \code{\link{doLoad}} and
      \code{\link{loadContexts}}.}
    \item{rules}{A character vector giving the names of
      rule files (minus the \code{.json} extension) in the
      \code{ruledir} directory.  See \code{\link{doLoad}} and
      \code{\link{loadRulesFromList}}.}
    \item{rulesWithTests}{A character vector giving the names of
      rule files (minus the \code{.json} extension) in the
      \code{ruledir} directory.  See \code{\link{doLoad}} and
      \code{\link{testAndLoad}}.}
    \item{defaultRecordData}{A named list giving the initial values of
      observables in the default (initial) student record.}
    \item{lscolname}{The name of the column to which the listener set
      should log messages. Example value \code{"Messages"}.}
    \item{listeners}{This is a list of listener descriptions. See the
      section \sQuote{Listener Configuration} below.}
    \item{listenerReset}{Which listeners should be reset before running.
     This should be a character scalar or vector.  The values should be
     names of listeners.  The special value \dQuote{Self} refers to the
     ListenerSet object, and the special value \dQuote{ALL} resets all
     listeners.  See \code{\link[Proc4]{resetListeners}}.  Example value
     \code{"ALL"}.} 
    \item{EIEngine}{A complex object describing engine parameters. See
      the section \sQuote{Engine Configuration} below.}
    \item{filter}{A complex object describing how to prefilter the
      database.  See the section \sQuote{Database Filters} below.}
    \item{extensions}{This should be a list of paths (relative to
      \code{config.dir}) containing additional R code to load.  This is
      not used by \code{doRunrun}, but is supplied for use in scripts
      that might use \code{doRunrun}.}
    \item{mode}{This is a documentation field.}
    \item{SRreset}{A logical flag.  If true old student records for the
      designated application will be cleared before running.}
    \item{limitNN}{An integer: how many events should be processed.
      Two special string values are also accepted.  \dQuote{ALL} will
      process all records currently in the database and stop.
      \dQuote{Inf} will cause the process to run in server mode until it
      is shut down.}
  }

  A number of these values do \dQuote{<app>} substitution, that is they
  will substitute the string \dQuote{<app>} for the short name of the
  application. 
  
}
\section{Listener Configruation}{

  The listeners consist of a \code{\link[Proc4]{ListenerSet}} and a
  collection of \code{\link[Proc4]{Listener}} objects.  The listener
  objects are made by using the information from the \dQuote{listners}
  element of the \code{EA.config} argument.  This should be a list of
  specifications (each specification itself is a list).  These are
  passed to \code{\link[Proc4]{buildListener}}, which provides some
  examples.

  The listener set is controlled by the \code{EAeng.local$dburi} value
  and the \dQuote{lscolname} field.  If \code{dbuir} is a name of a
  database, then the \code{\link[Proc4]{ListenerSet}} is logged into the
  \dQuote{lscolname} collection.  If \code{dburi} is null or an empty
  string, then the listener set will not do logging.

}
\section{Engine Configruation}{

  The arguments to the appropriate constructor are found between the
  \code{EIeng.local} and \code{EI.config$EIEngine} collections.  The
  intent is for the former to include details (e.g., database user names
  and passwords) which are local to the server on which \code{EIEvent} is
  running, and for \code{EI.config$EIEngine} to include more public
  details which are local to a particular run.

  See \code{\link{EIEvent}} for the expected fields.  Note that the
  \dQuote{processN} field is taken care of separately after the database
  operations (next section). 

}
\section{Database Filtering}{

  The \code{EI.config$filter} field controls the database filtering
  process.  There are four steps:

  \describe{
    \item{Remove}{old records from the database.}
    \item{Import}{new records into the database.}
    \item{Purge}{unused records from the database.}
    \item{Reprocess}{Reset the processed flag to ensure records get
      reprocessed.}
  }

  These are controlled by the following elements in the
  \code{EI.config$filter} list:
  \describe{
    \item{doRemove}{Logical, should records be removed before import.}
    \item{remove}{Filter to use for removal. The value \code{\{\}}
      will remove all records for the given \code{app}.}
    \item{importFile}{A list of filenames (in the \code{config.dir})
      which contain evidence sets to be imported before scoring.}
    \item{doPurge}{Logical, should records be removed after
      import.}
    \item{purge}{Filter for the purging (after import removal).
      Leaving this empty will probably not be satisfactory.}
    \item{doReprocess}{Logical, should existing records have the processed
      flag cleared?  Typically \code{TRUE} for rerun mode and
      \code{FALSE} for server mode.}
    \item{reprocess}{Filter for the selected records to be marked
      for reprocessing.  The value \code{\{\}} will mark all records
      (for this \code{app}) for reprocessing.}
  }

  If the \code{doprep} argument is false, the database preprocessing will be
  skipped regardless of the values of the filter field.

}
\section{Locking}{

  Locking is done via the administrative fields of the database.  In
  particular, if the EIEvent process is marked as running in the
  database, then the engine will not start.  The \code{override} switch
  will force a start anyway.

}
\section{Data Files}{

  If any of the listeners is a \code{\link[Proc4]{TableListener}}, then
  an output file corresponding to the table will be produced when the
  run finishes.  The name of that output file is determined by the field
  in the lister specificaitons.  The datafiles are registered using the
  \code{\link[Proc4]{ListenerSet}$registerOutput} method.

}
\section{Logging}{

  Logging is done through the \code{\link{futile.logger}{flog.logger}}
  mechanism. This allows logs to be save to a file.

  The \dQuote{logLevel} and \dQuote{logname} fields are put in the
  configuration specification to assist scripts in configuring the
  logging system.

  Both the log file is registered using the
  \code{\link[Proc4]{ListenerSet}$registerOutput} method.

}
  
\value{

  This returns the engine invisibly, in case the calling program wants
  to do something with it.
  
}
\references{

  The Bobs (1983) Psychokiller.  \emph{My I'm Large.}  Rhino Records.
  \url{https://www.youtube.com/watch?v=-Gu4PKnCLDg}.  (Reference is
  about 2:30 minutes into song.)

}
\author{Russell Almond}
\note{

  This function is meant to be called by the \code{RunEIEvent.R} script
  found in the config directory.
  (\code{file.path(help(package="EIEvent")$path,"conf","RunEIEvent.R")})

  The shell script \code{EIEvent} found in the same directory will run this
  script.  

}
\seealso{

  \code{\link{doLoad}}

  \code{\linkS4class{EIEngine}}, \code{\link{mainLoop}},
  \code{\link[Proc4]{ListenerSet}}


}
\examples{
## This example is in:
file.path(help(package="EIEvent")$path,"conf","RunEIEvent.R")
\dontrun{
library(R.utils)
library(EIEvent)

if (interactive()) {
  ## Edit these for the local application
  appStem <- "P4test"
  loglevel <- ""
  noprep <- FALSE
  override <- FALSE
} else {
  appStem <- cmdArg("app",NULL)
  if (is.null(app) || !grepl("^ecd://",app))
    stop("No app specified, use '--args app=ecd://...'")
  loglevel <- cmdArg("level","")
  noprep <- as.logical(cmdArg("noprep",FALSE))
  override <- as.logical(cmdArg("override",FALSE))
}

source("/usr/local/share/Proc4/EIini.R")

EI.config <- jsonlite::fromJSON(file.path(config.dir,"config.json"),FALSE)


app <- as.character(Proc4.config$apps[appStem])
if (length(app)==0L || any(app=="NULL")) {
  stop("Could not find app for ",appStem)
}
if (!(appStem \%in\% EI.config$appStem)) {
  stop("Configuration not set for app ",appStem)
}


logfile <- (file.path(logpath, sub("<app>",appStem,EI.config$logname)))
## Let command line override configuration.
if (nchar(loglevel)==OL) loglevel <- EI.config$logLevel

if (interactive()) {
  flog.appender(appender.tee(logfile))
} else {
  flog.appender(appender.file(logfile))
}
flog.threshold(EI.config$loglevel)

## Load extensions.
for (ext in EI.config$extensions) {
  if (is.character(ext) && nchar(ext) > 0L) {
    if (file.exists(file.path(config.dir,ext))) {
      source(file.path(config.dir,ext))
    } else {
      flog.error(paste("Can't find extension file", ext))
    }
  }
}

eng <- doRunrun(app,EI.config,EIeng.local,config.dir,outdir,
                logfile=logfile, override=override, noprep=noprep)

}
}
\keyword{ interface }
\keyword{ graphs }
\keyword{ database }

