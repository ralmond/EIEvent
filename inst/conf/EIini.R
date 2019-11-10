## These are application generic parameters
EIeng.common <- list(host="localhost",username="EI",password="secret",
                     dbname="EIRecords",P4dbname="Proc4",waittime=.25)

appstem <- basename(app)
## These are for application specific parameters
EIeng.params <- list(app=app)

config.dir <- "/home/ralmond/ownCloud/Projects/NSFCyberlearning/EvidenceID"

logfile <- file.path("/usr/local/share/Proc4/logs",
                     paste("EI_",appstem,"0.log",sep=""))

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

EI.listenerSpecs <-
  list("InjectionListener"=list(sender=paste("EI",appstem,sep="_"),
            dbname="EARecords",dburi="mongodb://localhost",
            colname="EvidenceSets",messSet="New Observables"),
       "UpdateListener"=list(dbname="Proc4",dburi="mongodb://localhost",
            colname="Players",targetField="data",
            messSet=c("Money Earned","Money Spent"),
            jsonEncoder="trophy2json"))

### Special Applicable Slider Functions

### Reset rules should set the field "state.flags.applicable" to the
### names of the appliable sliders for this level.  Note, can't use
### the "state.fobservables" prefix.

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

