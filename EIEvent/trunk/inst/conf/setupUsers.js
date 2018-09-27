// This file contains javascript to set up the collections expected by
// the EA process

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

eiUser = pwds.filter(function(u) {return u.user == "EIP";})[0];
eapUser.roles = [{role:"readWrite", db:"EIRecords"},
                 {role:"read", db:"gameLRS"},
                 {role:"dbAdmin", db:"EIRecords"}];
db.getSiblingDB("EIRecords");
db.createUser(eipUser);
