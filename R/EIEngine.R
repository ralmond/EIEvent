EIEngine <-
   setRefClass("EIEngine",
               fields=c(app="character",
                        dburi="character",
                        dbname="character",
                        userRecords="UserRecordSet",
                        rules="RuleTable",
                        contexts="ContextSet",
                        events="MongoDB",
                        ruleTests="TestSet",
                        listenerSet="ListenerSet"),
               methods = list(
                   initialize =
                     function(app="default",listeners=list(),
                              username="",password="",host="localhost",
                              port="",dbname="EIRecrods",
                              ...) {
                       security <- ""
                       if (nchar(username) > 0L) {
                         if (nchar(password) > 0L)
                           security <- paste(username,password,sep=":")
                         else
                           security <- username
                       }
                       if (nchar(port) > 0L)
                         host <- paste(host,port,sep=":")
                       else
                         host <- host
                       if (nchar(security) > 0L)
                         host <- paste(security,host,sep="@")
                       dburi <- paste("mongo:/",host,sep="/")
                       flog.info("Connecting to database %s/%s\n",dburi,dbname)
                       events <- NULL # mongo("Events",dbname,dburi)
                       listenerSet <- ListenerSet(sender=
                                                    paste("EIEngine[",app,"]"),
                                                  dbname=dbname,
                                                  dburi=dburi,
                                                  listners=listeners,
                                                  colname="Messages",
                                                  ...)
                       rules <- RuleTable$new(app,dbname,dburi)
                       userRecords <- UserRecordSet$new(app,dbname,dburi)
                       contexts <- ContextSet$new(app,dbname,dburi)
                       ruleTests <- TestSet$new(app,dbname,dburi)
                       callSuper(app=app,dbname=dbname,dburi=dburi,
                                 listenerSet=listenerSet,
                                 events=events,
                                 rule=rules,userRecords=userRecords,
                                 contexts=contexts,ruleTests=ruleTests,
                                 ...)
               }))


## Listener notification.
setMethod("notifyListeners","EIEngine",
           function(sender,mess) {
             sender$listnerSet$notifyListeners(mess)
           })

## Student Record Methods
EIEngine$methods(
             newStudent = function (uid) {
              state <- userRecords$newStudent(uid)
              state
             })


## Rule Methods
EIEngine$methods(
         )

## Context Methods
EIEngine$methods(
          )

## Event Methods
EIEngine$methods(
             eventdb = function() {
               if (is.null(events)) {
                 events <<- mongo("Events",dbname,dburi)
               }
              events
            }
        )


## Handle Event
EIEngine$methods(
              handleEvent = function (event) {

              })

## Rule Tests
EIEngine$methods(
              testRules = function (event) {

              })

