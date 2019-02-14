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
                       dburl <- paste("mongo:/",host,sep="/")
                       flog.info("Connecting to database %s/%s\n",dburl,dbname)
                       evnts <- NULL # mongo("Events",dbname,dburi)
                       ls <- ListenerSet(sender= paste("EIEngine[",app,"]"),
                                         dbname=dbname,
                                         dburi=dburl,
                                         listners=listeners,
                                         colname="Messages",
                                         ...)
                       rls <- RuleTable$new(app,dbname,dburl)
                       urecs <- UserRecordSet$new(app,dbname,dburl)
                       ctxts <- ContextSet$new(app,dbname,dburl)
                       rTests <- TestSet$new(app,dbname,dburl)
                       callSuper(app=app,dbname=dbname,dburi=dburl,
                                 listenerSet=ls, events=evnts,
                                 rule=rls,userRecords=urecs,
                                 contexts=ctexts,ruleTests=rTests,
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

