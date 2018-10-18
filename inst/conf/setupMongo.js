// This file contains javascript to set up the collections expected by
// the EI process

// Before running this script, create passwords for the Proc4
// processes you will use.
// You should put these in a file called ".Proc4.js" in your home
// directory.  It should look like:
// var pwds = [
//     {"user":"EAP","pwd":"Secret1"},
//     {"user":"ASP","pwd":"Secret2"},
//     {"user":"EIP","pwd":"Secret3"},
//     {"user":"C4","pwd":"Secret4"},
// ];
// Then load that file.  Change the next line
// To reflect the name of that path.
load("/home/ralmond/.Proc4.js")

eipUser = pwds.filter(function(u) {return u.user == "EIP";})[0];

con = new Mongo();
db=con.getDB("EIRecords");                 
db.auth(eipUser.user,eipUser.pwd);
db.createCollection("Events", {
    validator: {
        $jsonSchema: {
            bsonType: "object",
            required: ["app","uid","timestamp"],
            properties: {
                _id: {
                    bsonType: "string",
                    description: "app@uid@seqno",
                },
                app: {
                    bsonType: "string",
                    description: "Application ID (string)"
                },
                uid: {
                    bsonType: "string",
                    description: "User (studnet) ID (string)"
                },
                context: {
                    bsonType: "string",
                    description: "Context (task) ID (string)"
                },
                timestamp: {
                    bsonType: "date",
                    description: "Timestamp"
                },
                mess: {
                    bsonType: "string",
                    description: "Instructions to EAP"
                },
                verb: {
                    bsonType: "string",
                    description: "Verb Characterizing Event"
                },
                object: {
                    bsonType: "string",
                    description: "Predicate for Verb"
                },
                data: {
                    bsonType: "object",
                    description: "Named list of evidence."
                }
                }
            }
        }
    },
    validationAction: "warn"
})
db.EvidenceSets.createIndex( { app:1, uid: 1, timestamp:-1});
db.createCollection("Rules", {
    validator: {
        $jsonSchema: {
            bsonType: "object",
            required: ["app","context","verb","object","priority","ruleType"],
            properties: {
                _id: {
                    bsonType: "string",
                    description: "app@uid@seqno",
                },
                app: {
                    bsonType: "string",
                    description: "Application ID (string)"
                },
                name: {
                    bsonType: "string",
                    description: "Short Description of the Rule"
                },
                doc: {
                    bsonType: "string",
                    description: "Long Description of the Rule"
                },
                context: {
                    bsonType: "string",
                    description: "Context (task) ID (string)"
                },
                verb: {
                    bsonType: "string",
                    description: "Verb in which this rule is applicable."
                },
                object: {
                    bsonType: "string",
                    description: "Object for which this rule is applicable."
                },
                ruleType: {
                    bsonType: "string",
                    description: "Type of the rule."
                },
                condition: {
                    bsonType: "object",
                    description: "Condition Query"
                },
                predicate: {
                    bsonType: "object",
                    description: "Actions taken when rule is triggered."
                },
                priority: {
                    bsonType: "int",
                    description: "When is this rule run (lower numbers are run first)."
                }
            }
        }
    },
    validationAction: "warn"
})
db.Rules.createIndex( { app:1, context: 1, verb: 1, object: 1, ruleType:1, priority:-1});
db.createCollection("States", {
    validator: {
        $jsonSchema: {
            bsonType: "object",
            required: ["app","uid","context","timestamp"],
            properties: {
                _id: {
                    bsonType: "string",
                    description: "app@uid@seqno",
                },
                app: {
                    bsonType: "string",
                    description: "Application ID (string)"
                },
                uid: {
                    bsonType: "string",
                    description: "User (studnet) ID (string)"
                },
                context: {
                    bsonType: "string",
                    description: "Context (task) ID (string)"
                },
                timers: {
                    bsonType: "array",
                    description: "List of timers"
                },
                flags: {
                    bsonType: "array",
                    description: "List of timers"
                },
                observables: {
                    bsonType: "array",
                    description: "List of timers"
                },
                timestamp: {
                    bsonType: "date",
                    description: "Timestamp"
                }
            }
        }
    },
    validationAction: "warn"
})
db.States.createIndex( { app:1, context: 1, timestamp:-1});

