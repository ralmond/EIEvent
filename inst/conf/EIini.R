Proc4.config <- fromJSON("/usr/local/share/Proc4/Proc4.json",FALSE)

dbhost <- "localhost"
dbuser <- "" # "EAP"
dbport <- "" # "27018"

dburi <-Proc4::makeDBuri(dbuser,Proc4.config$pwds[[dbuser]],
                         dbhost,dbport)
config.dir <- "/home/ralmond/Projects/PP-EI"
outdir <- config.dir

logpath <- "/usr/local/share/Proc4/logs"


## These are application generic parameters
EIeng.local <- list(dburi=dburi,sslops=mongolite::ssl_options(),
                    dbname="EIRecords",admindbname="Proc4",
                    lscolName="Messages",registrycol="OutputFiles",
                    aacol="AuthorizedApps", eventcol="Events",
                    rulecol="Rules", urcol="States",
                    contextcol="Contexts",testcol="Tests",
                    mongoverbose=FALSE)

trophy2json <- function(dat) {
  thall <- ""
  if (length(dat$trophyHall) > 0L) {
    thall <- paste(
        paste('{"',names(dat$trophyHall),'":"',dat$trophyHall,'"}',
              sep=""), collapse=", ")
  }
  paste('{', '"trophyHall"', ':','[', thall, '],',
        '"bankBalance"', ':', dat$bankBalance, '}')
}

### Reset rules should set the field "state.flags.applicable" to the
### names of the appliable sliders for this level.  Note, can't use
### the "state.observables" prefix.

allSliders <- paste("state","observables",
                    c("massManip", "gravityManip", "airManip"),
                    sep=".")

## Applicable Adjustment must run first
applicableAdjustments <- function (name, state, event) {
  adj <- 0
  appl <- getJS("state.flags.applicable",state,event)
  if (is.null(appl) || is.null(appl) || length(appl) == 0L) {
    return ("NA")                      #Nothing applicable, do nothing.
  }
  appl <- paste("state","observables",appl,sep=".")
  for (slider in appl) {
    sval <- as.numeric(getJS(slider,state,event))
    flog.trace("Slider %s, adj=%f, sval=%f.",slider,adj,sval)
    adj <- adj + sval
  }
  adj
}

mostlyApplicable <- function (name, state, event) {
  total <- 0
  appl <- getJS("state.observables.SliderAdjust",state,event)
  if (is.null(appl) || is.na(appl)) {
    appl <- 0
  }
  appl <- as.numeric(appl)
  if (is.na(appl) || length(appl) == 0L) {
    appl <- 0
  }
  for (slider in allSliders) {
    total <- total + as.numeric(getJS(slider,state,event))
  }
  inapp <- total - appl

  val <- "NA"
  if (total == 0) {
    ## Nothing yet, leave value at NA.
  } else if (inapp < 2) {
    val <- "almostAlways"
  } else if (appl < 2) {
    val <- "almostNever"
  } else if (inapp < appl) {
    val <- "mosltyNot"
  } else if (appl <= inapp) {
    val <- "mostly"
  } else {
    flog.warn("Weird State:  applicable=%d, inapplicable=%d",appl,inapp)
  }
  val
}
