## These are application generic parameters
EIeng.common <- list(host="localhost",username="EI",password="secret",
                     dbname="EIRecords",P4dbname="Proc4",waittime=.25)
appstem <- basename(app)

## These are for application specific parameters
EIeng.params <- list(app=app)


logfile <- file.path("/usr/local/share/Proc4/logs",
                     paste("EI_",appstem,"0.log",sep=""))

trophy2json <- function(dat) {
  paste('{', '"trophyHall"', ':','[',
        paste(
            paste('{"',names(dat$trophyHall),'":"',dat$trophyHall,'"}',
                  sep=""), collapse=", "), '],',
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

