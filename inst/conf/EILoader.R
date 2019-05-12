###
## These are R functions for loading the contexts and rules into the Eng

library(EIEvent)
flog.threshold(INFO)
cl <- new("CaptureListener")

config.dir <- "/home/ralmond/ownCloud/Projects/NSFCyberlearning/EvidenceID"

engTest <- EIEngine(app="ecd://epls.coe.fsu.edu/P4test",
                    listeners=list(capture=cl))
engUser <- EIEngine(app="ecd://epls.coe.fsu.edu/PhysicsPlayground/Sp2019/userControl",
                    listeners=list(capture=cl))
engLin <- EIEngine(app="ecd://epls.coe.fsu.edu/PhysicsPlayground/Sp2019/linear",
                    listeners=list(capture=cl))
engAdapt <- EIEngine(app="ecd://epls.coe.fsu.edu/PhysicsPlayground/Sp2019/adaptive",
                    listeners=list(capture=cl))

sketchCon <- read.csv(file.path(config.dir,"ContextSketching.csv"))
manipCon <- read.csv(file.path(config.dir,"ContextManipulation.csv"))
initCon <- data.frame(CID="*INITIAL*",Name="*INITIAL*",Number=0)



engTest$clearContexts()
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

sRules <- lapply(fromJSON(file.path(config.dir, "Rules",
                                    "CombinedRules.json"),FALSE),
                 parseRule)
tRules <- lapply(fromJSON(file.path(config.dir, "Rules",
                                    "TrophyHallRules.json"),FALSE),
                 parseRule)

engTest$rules$clearAll()
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
system2("mongoimport",args=sprintf("-d EIRecords -c States --jsonArray %s",
                                   file.path(config.dir,"defaultSR.json")))

