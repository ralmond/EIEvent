\name{RuleTable-class}
\Rdversion{1.1}
\docType{class}
\alias{RuleTable-class}

\title{Class \code{"RuleTable"}}
\description{

This is a containiner for a set of rules for an application.  

}
\section{Extends}{

All reference classes extend and inherit methods from
\code{"\linkS4class{envRefClass}"}.

This class extends by containment the \code{\linkS4class{MongoDB}} class
where the actual rules are stored.

}
\references{
  The document \dQuote{Rules Of Evidence} gives extensive documentation
  for the rule system.
  \url{https://pluto.coe.fsu.edu/Proc4/RulesOfEvidence.pdf}. 
}
\author{Russell Almond}
\note{

  A different set of rules is associated with each application ID.

}
\section{Tracing}{

  The EIEngine uses the \code{\link[futile.logger]{flog.logger}}
  system.  In particular, setting the threshold for the \dQuote{EIEvent}
  logger, will change the amount of information sent to the log file.

  In particular, DEBUG level logging will cause the findRules() method
  to tell how many rules were returned, and the TRACE level will list
  them. 

}
\seealso{

  The \code{\linkS4class{Rule}} class object describes rules.  The function
  \code{\link{loadRulesFromList}} and \code{\link{testAndLoad}} can be
  used to load rules into the rule table.

  The primary classes in the EIEvent system are:
  \code{\linkS4class{EIEngine}}, \code{\linkS4class{Context}},
  \code{\linkS4class{Status}}, \code{\linkS4class{Event}},
  \code{\linkS4class{Rule}}.


  The \code{\linkS4class{EIEngine}} class is a container for the
  following classes: 
  \code{\linkS4class{UserRecordSet}}, \code{\linkS4class{RuleTable}},
  \code{\linkS4class{ContextSet}},  \code{\linkS4class{TestSet}} and
  \code{\link[Proc4]{ListenerSet}}, 

  \code{\link[futile.logger]{flog.logger}}

}
\examples{
showClass("RuleTable")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{app}:}{Object of class \code{character} providing applicaiton
      ID of the application. }
    \item{\code{dbname}:}{Object of class \code{character} giving the
      name of the database used. }
    \item{\code{dburi}:}{Object of class \code{character} giving the URI
      of the database used.}
    \item{\code{db}:}{Object of class \code{MongoDB} giving the actual
      mongo collection.  This should be accessed using the
      \code{ruledb()} method to make sure that it has been properly
      updated.   }
    \item{\code{stoponduplicate}:}{Object of class \code{logical}.  If
      true, an error will be signaled if a rule is added with a
      duplicate name.  If false, it will be quietly replaced. }
  }
}
\section{Methods}{
  \describe{
    \item{\code{updateRule(rule)}:}{This command will add or replace a
      rule in the database.  If the rule has the same name as an
      existing rule, the the behavior will depend on the value of
      \code{stoponduplicate}.  The that flag is FALSE, then the rule will be
      replaces, if true and error will be logged and the rule not
      replaced. Note that if the rule
      has a database ID, and it does not match the ID of the rule it is
      replacing, an error will be logged and the rule not replaced.}
    \item{\code{findRuleByName(name)}:}{ This searchers for a rule by
      the name filed. }
    \item{\code{findRules(verb, object, context, phase)}:}{This is the
      key function that finds rules based on \code{\link{Event}} and
      \code{\link{Status}}.  This will return a (possibly empty) list of
      all matching rules.} 
    \item{\code{skipDuplicate(newval)}:}{If called with no arguments,
      gives the rucrrent value of \code{stoponduplicate}.  If a logical
      value is given, then the value of the flag is set.}
    \item{\code{ruledb()}:}{ This returns the database collection  If
      the database colleciton is currently null, it is inialized.  This
      method should be used rather than raw calls to the \code{db} field.} 
    \item{\code{initialize(app, dbname, dburi, db, stoponduplicate,
        ...)}:}{ This initialization method. }
    \item{\code{clearAll()}:}{ This removes all rules for this
      application from the database.}
  }
}
