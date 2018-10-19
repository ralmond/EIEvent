####
## This coverts from Learning Locker format events to Proc4 format events.

## "statement.actor.account.name":  -> uid
## "statement.verb.display.en-US": -> verb
## "statement.object.definition.name.en-US": -> object
## "statement.object.definition.extensions": -> data
## "statement.context.extensions.level": -> context
## "active":1,
## timestamp -> timestamp
## app = "https://coe.fsu.edu/epls/PhysicsPlayground"
## sender = "Unity Engine"
## mess = "Event" + Verb + Object

## Needto fix usernames.
LLcol <- mongo("Messages",
                 url="mongodb://test:secret@127.0.0.1:27017/test")


db.statements.find('{"statement.actor.account.name":name,
                    timestamp:{"$gte":after},timestamp:{"$lte":before}}',
                   '{"statement.actor.account.name": 1,
                    "statement.verb.display.en-US":1,
                    "statement.object.definition.name.en-US":1,
                    "statement.object.definition.extensions":1,
                    "statement.context.extensions":1,
                    "active":1, timestamp:1}')

