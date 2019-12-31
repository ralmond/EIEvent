###
## These are R functions for loading the contexts and rules into the
## Eng

## This file can be run with the command
## R --slave EILoader.R

library(EIEvent)
flog.threshold(INFO)
cl <- new("CaptureListener")
app <- "ecd://epls.coe.fsu.edu/P4test"
## Edit this to reflect local preferences
config.dir <- "/home/ralmond/ownCloud/Projects/NSFCyberlearning/EvidenceID"

## Create an engine to open connection to the database.  Only the
## engTest code is necessary except for the full server setup.
engTest <- EIEngine(app="ecd://epls.coe.fsu.edu/P4test",
                    listeners=list(capture=cl))
engUser <- EIEngine(app="ecd://epls.coe.fsu.edu/PhysicsPlayground/Sp2019/userControl",
                    listeners=list(capture=cl))
engLin <- EIEngine(app="ecd://epls.coe.fsu.edu/PhysicsPlayground/Sp2019/linear",
                    listeners=list(capture=cl))
engAdapt <- EIEngine(app="ecd://epls.coe.fsu.edu/PhysicsPlayground/Sp2019/adaptive",
                    listeners=list(capture=cl))

## Load the context tables
sketchCon <- read.csv(file.path(config.dir,"ContextSketching.csv"))
manipCon <- read.csv(file.path(config.dir,"ContextManipulation.csv"))
## Special initial context table is required.
initCon <- data.frame(CID="*INITIAL*",Name="*INITIAL*",Number=0)

## Load the context tables into the database
engTest$clearContexts() ## Clears the database
engTest$addContexts(initCon)
engTest$addContexts(sketchCon)
engTest$addContexts(manipCon)

engLin$clearContexts()
engLin$addContexts(initCon)
engLin$addContexts(sketchCon)
engLin$addContexts(manipCon)

engUser$clearContexts()
engUser$addContexts(initCon)
engUser$addContexts(sketchCon)
engUser$addContexts(manipCon)

engAdapt$clearContexts()
engAdapt$addContexts(initCon)
engAdapt$addContexts(sketchCon)
engAdapt$addContexts(manipCon)


## Load and parse the rule files.
sRules <- lapply(fromJSON(file.path(config.dir, "Rules",
                                    "CombinedRules.json"),FALSE),
                 parseRule)
## Note the CorrectDuration rules replace the CombinedRules; edit this
## as appropriate
sRules <- lapply(fromJSON(file.path(config.dir, "Rules",
                                    "CorrectDuration.json"),FALSE),
                 parseRule)
tRules <- lapply(fromJSON(file.path(config.dir, "Rules",
                                    "TrophyHallRules.json"),FALSE),
                 parseRule)

## Save the rules in the database, changing the app field to match
## app(eng)
engTest$rules$clearAll()                # Clear old rules
engTest$loadRules(sRules)
engTest$loadRules(tRules)

engUser$rules$clearAll()
engUser$loadRules(sRules)
engUser$loadRules(tRules)

engLin$rules$clearAll()
engLin$loadRules(sRules)
engLin$loadRules(tRules)

engAdapt$rules$clearAll()
engAdapt$loadRules(sRules)
engAdapt$loadRules(tRules)

## Load default student records.
engTest$userRecords$clearAll(TRUE)   #Clear default records
engUser$userRecords$clearAll(TRUE)   #Clear default records
engLin$userRecords$clearAll(TRUE)   #Clear default records
engAdapt$userRecords$clearAll(TRUE)   #Clear default records
## Note:  the defaultSR.json file has default student records for all
## four applications.
## Alternative method using external file.
## system2("mongoimport",args=sprintf("-d EIRecords -c States --jsonArray %s",
##                                    file.path(config.dir,"defaultSR.json")))

defaultRec <- engTest$newUser("*DEFAULT*")
obs(defaultRec,"bankBalance") <- 0
obs(defaultRec,"trophyHall") <- list()
engTest$saveStatus(defaultRec)

defaultRec <- engUser$newUser("*DEFAULT*")
obs(defaultRec,"bankBalance") <- 0
obs(defaultRec,"trophyHall") <- list()
engUser$saveStatus(defaultRec)

defaultRec <- engLin$newUser("*DEFAULT*")
obs(defaultRec,"bankBalance") <- 0
obs(defaultRec,"trophyHall") <- list()
engLin$saveStatus(defaultRec)

defaultRec <- engAdapt$newUser("*DEFAULT*")
obs(defaultRec,"bankBalance") <- 0
obs(defaultRec,"trophyHall") <- list()
engAdapt$saveStatus(defaultRec)
