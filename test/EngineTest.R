library(EIEvent)
flog.threshold(DEBUG)
cl <- new("CaptureListener")


### initialize
eng <- EIEngine(app="ecd://epls.coe.fsu.edu/EItest",listeners=list(capture=cl))


### notifyListeners

notifyListeners(eng,P4Message("Test","*INITIALIZE*","EI","Testing started"))
rec <- cl$lastMessage()
stopifnot(context(rec)=="*INITIALIZE*")

### newStudent

eng$userRecords$clearAll(TRUE)
strp <- eng$newStudent("Porgy")
stopifnot(uid(strp)=="Porgy",app(strp)==eng$app,
          context(strp)=="*INITIAL*",
          length(strp@flags)==0L)

defstatus <- Status("*DEFAULT*","*INITIAL*",flags=list(learningSupports=0),
                    app=eng$app)
eng$saveStatus(defstatus)

strb <- eng$newStudent("Bess")
stopifnot(uid(strb)=="Bess",app(strb)==eng$app,
          context(strb)=="*INITIAL*",
          flag(strb,"learningSupports")==0L)

### getStatus

str1 <- eng$getStatus("Porgy")
stopifnot(all.equal(strp,str1))

str0 <- eng$getStatus("Crown")
stopifnot(uid(str0)=="Crown",app(str0)==eng$app,
          context(str0)=="*INITIAL*",
          flag(str0,"learningSupports")==0L)


### saveStatus

obs(str1,"tweaked") <-TRUE

str1a <- eng$saveStatus(str1)
str2 <- eng$getStatus(uid(str1))
stopifnot(all.equal(str1,str2))

### getContext

conmat <- read.csv(file.path(library(help="EIEvent")$path,"testScripts",
                "SampleContextSheet.csv"))
conmat1 <- conmat
conmat1$Number <- 1:nrow(conmat) ## List style needs small integer indexes.

eng$clearContexts()
eng$addContexts(conmat)
c28 <- eng$getContext(28L)
cStairs <- eng$getContext("Stairs")
stopifnot(all.equal(c28,cStairs))

### findRules

sRules <- lapply(fromJSON(file.path(library(help="EIEvent")$path,"testScripts",
                                    "SampleRules.json"),FALSE),parseRule)
eng$rules$clearAll()
eng$loadRules(sRules)

r1s <- eng$findRules("initialized","game level","Stairs","Status")
stopifnot(length(r1s)==1L)
r1s <- r1s[[1]]
stopifnot(name(r1s)=="Start Timer")

r2o <- eng$findRules("identified","game object","Stairs","Observable")
stopifnot(length(r2o)==1L)
r2o <- r2o[[1]]
stopifnot(name(r2o)=="Update agent used.")

r3c <- eng$findRules("initialized","game level","Stairs","Context")
stopifnot(length(r3c)==1L)
r3c <- r3c[[1]]
stopifnot(name(r3c)=="Start Level")

r4t <- eng$findRules("satisfied","game level","BlockedByBlocks","Trigger")
stopifnot(length(r4t)==1L)
r4t <- r4t[[1]]
stopifnot(name(r4t)=="Satisfied Trigger")

r5r <- eng$findRules("play","params","Balance","Reset")
stopifnot(length(r5r)==1L)
r5r <- r5r[[1]]
stopifnot(name(r5r)=="Reset Blower Flags and Counters")


### runRule

st0 <- Status(uid="Test0",context="Stairs",
             timestamp=as.POSIXct("2018-09-25 12:12:28 EDT"),
             observables=list(agentsUsed=list(),
                              lastAgent=NA),
             app=eng$app)
evnt1 <- Event(app=eng$app,uid="Test0",
               verb="identified",object="game object",
               context="Stairs",
               timestamp=as.POSIXct("2018-09-25 12:12:29 EDT"),
               details= list("gemeObjectType"="Lever"))
st1exp <- Status(uid="Test0",context="Stairs",
             timestamp=as.POSIXct("2018-09-25 12:12:28 EDT"),
             observables=list(agentsUsed=list("Lever"),
                              lastAgent="Lever"),
             app=eng$app)
st1act <- runRule(st0,evnt1,r2o,"Observable")
stopifnot(all.equal(st1exp,st1act))



### runTRule
st2 <- Status(uid="Test0",context="Stairs",
             timestamp=as.POSIXct("2018-09-25 12:13:30 EDT"),
             observables=list(agentsUsed=list("Lever"),
                              lastAgent="Lever",
                              badge="silver"),
             app=eng$app)

evnt2 <- Event(app=eng$app,uid="Test0",
               verb="satisfied",object="game level",
               context="Stairs",
               timestamp=as.POSIXct("2018-09-25 12:13:30 EDT"),
               details= list("badge"="silver"))

mess1 <- buildMessages(predicate(r4t),st2,evnt2)[[1]]


runTRule(st2,evnt2,r4t,cl)
mess1a <- cl$lastMessage()
stopifnot(all.equal(mess1a,mess1,check_ids=FALSE))

### runStatusRules

st3 <- Status(uid="Test0",context="Balance",
             timestamp=as.POSIXct("2018-09-25 12:12:28 EDT"),
             flags = list(blowerUsed=FALSE),
             observables=list(blowerManip=0),
             app=eng$app)
evnt4 <- Event(app=eng$app,uid="Test0",
               verb="adjusted",object="control",
               context="Balance",
               timestamp=as.POSIXct("2018-09-25 12:12:40 EDT"),
               details= list("gemeObjectType"="blower",
                          oldValue=0,newValue=5,
                          method="slider"))
st4exp <- Status(uid="Test0",context="Balance",
             timestamp=as.POSIXct("2018-09-25 12:12:40 EDT"),
             flags = list(blowerUsed=TRUE),
             observables=list(blowerManip=1),
             app=eng$app)

st4act <- runStatusRules(eng,st3,evnt4)
stopifnot(all.equal(st4exp,st4act))

### runObservableRules

st1act <- runObservableRules(eng,st0,evnt1)
stopifnot(all.equal(st1exp,st1act))


### runResetRules

st5 <- Status("uid"="Test0", "context"="Balance",
             app=eng$app,
            "timestamp"=as.POSIXct("2018-09-25 12:12:28 EDT"),
            "flags"=list("blowerUsed"=TRUE),
            "observables"=list("blowerManip"=3))
evnt6 <-Event(app=eng$app, "uid"= "Test0",
            "verb"= "play", "object"= "params",
            "context"= "Balance",
            "timestamp"=as.POSIXct("2018-09-25 12:13:28 EDT"),
            "details"=list())
st6exp <- Status("uid"="Test0", "context"="Balance",
                app=eng$app,
                "timestamp"=as.POSIXct("2018-09-25 12:12:28 EDT"),
                "flags"=list("blowerUsed"=FALSE),
                "observables"=list("blowerManip"=0))

st666 <- runRule(st5,evnt6,r5r,"Reset")
stopifnot(all.equal(st666,st6exp,checkTimeStamp=FALSE))
##executePredicate(predicate(r5r),st5,evnt6)

st6act <- runResetRules(eng,st5,evnt6)
stopifnot(all.equal(st6exp,st6act))


### runContextRules

st7 <- Status("uid"="Test0", "context"="*INITIAL*",
             app=eng$app,
             "timestamp"=as.POSIXct("2018-09-25 12:12:28 EDT"))
evnt8 <- Event(uid="Test0",verb="initialized",object="game level",
                "timestamp"=as.POSIXct("2018-09-25 12:12:29 EDT"),
                details=list(gameLevel="Stairs"),
                app=eng$app)
st8exp <- Status("uid"="Test0", context="Stairs",
                app=eng$app,
                "timestamp"=as.POSIXct("2018-09-25 12:12:29 EDT"))
st8exp@oldContext <- "*INITIAL*"

st8act <- runContextRules(eng,st7,evnt8)
stopifnot(all.equal(st8exp,st8act))

### runTriggerRules

st9 <- Status(uid="Test0",context="SpiderWeb",
             timestamp=as.POSIXct("2018-09-25 12:13:30 EDT"),
             observables=list(agentsUsed=list("Pendulum"),
                              lastAgent="Pendulum",
                              badge="gold"),
             app=eng$app)

evnt10 <- Event(app=eng$app,uid="Test0",
               verb="satisfied",object="game level",
               context="SpiderWeb",
               timestamp=as.POSIXct("2018-09-25 12:13:30 EDT"),
               details= list("badge"="gold"))

mess2 <- buildMessages(predicate(r4t),st9,evnt10)[[1]]

runTriggerRules(eng,st9,evnt10)
stopifnot(all.equal(cl$lastMessage(),mess2,check_ids=FALSE))

### handleEvent
stpee <- st1exp
timestamp(stpee) <- timestamp(evnt1)
stpe <- processEvent(eng,st0,evnt1)
stopifnot(all.equal(stpe,stpee))


evnt8b <- Event(uid="Bess",verb="initialized",object="game level",
                "timestamp"=as.POSIXct("2019-04-15 12:12:29 EDT"),
                details=list(gameLevel="Stairs"),
                app=eng$app)
handleEvent(eng, evnt8b)
bout <- eng$getStatus("Bess")
stopifnot(context(bout)=="Stairs",
          timerTime(bout,"levelTime",timestamp(evnt8b))
          == as.difftime(0,units="secs"),
          timestamp(bout)==timestamp(evnt8b))


