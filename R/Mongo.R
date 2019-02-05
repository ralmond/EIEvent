
## Need to figure out how we initialize the user name and password.
## For the momemnt, we will leave them as null.

EAPUSERNAME <- ""
EAPPASSWORD <- ""
EAPDATABASEHOST <- "localhost"
EAPDATABASEPORT <- "27017"
EAPDATABASEDB <- "EIRecords"

mongoCol <- function(collectionName) {
  security <- ""
  if (nchar(EAPUSERNAME) > 0L) {
    if (nchar(EAPPASSWORD) > 0L)
      security <- paste(EAPUSERNAME,EAPPASSWORD,sep=":")
    else
      security <- EAPUSERNAME
  }
  if (nchar(EAPDATABASEPORT) > 0L)
    host <- paste(EAPDATABASEHOST,EAPDATABASEPORT,sep=":")
  else
    host <- EAPDATABASEHOST
  if (nchar(security) > 0L)
    host <- paste(security,host,sep="@")
  url <- paste("mongodb:/",host,EAPDATABASENAME,sep="/")
  mongo(collectionName,url=url)
}


