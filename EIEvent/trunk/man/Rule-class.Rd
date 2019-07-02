\name{Rule-class}
\Rdversion{1.1}
\docType{class}
\alias{Rule-class}
\alias{show,Rule-method}
\alias{toString,Rule-method}
\alias{app,Rule-method}
\alias{all.equal.Rule}
\title{Class \code{"Rule"}}
\description{

  A \code{Rule} is a declarative predicate for processing events.  It
  has two parts, a \code{condition} and a \code{predicate}.  If the
  current \code{\linkS4class{Status}} and \code{\linkS4class{Event}}
  statisfy the condition, then the \code{Status} is updated according to
  the instructions in the \code{Predicate}.

}
\section{Objects from the Class}{
  Objects can be created by calls of the function \code{\link{Rule}}, or
  read from JSON using \code{\link{parseRule}}.
  Generally rules are stored in a \code{\linkS4class{RuleTable}}.

  When the \code{\linkS4class{EIEngine}} gets a new
  \code{\linkS4class{Event}}, then the applicable rules from the
  appropriate rule table are fetched.  For each rule, if the condition
  is matched then the predicate is executed.

}
\section{Slots}{
  \describe{
    \item{\code{_id}:}{Object of class \code{"character"} which provides
      the Mongo database ID of the rule.  This generally should not be
      modified.}
    \item{\code{app}:}{Object of class \code{"character"} giving the
      name of the application.  This should match the
      \code{\linkS4class{RuleTable}} the rule belongs to.}
    \item{\code{name}:}{Object of class \code{"character"} giving a human
      readable identifier for the rule; used in error reporting and rule
      management.}
    \item{\code{doc}:}{Object of class \code{"character"} giving a human
      readable description of the intent of the rule.}
    \item{\code{context}:}{Object of class \code{"character"} giving the
      character ID of of a \code{\linkS4class{Context}} or context group
      to which the rule applies.  The special value \dQuote{ANY} can be
      used to indicate that the rule applies to all contexts.}
    \item{\code{verb}:}{Object of class \code{"character"} giving the
      value of the \code{verb(\linkS4class{Event})} to which the rule
      applies.  The special value \dQuote{ANY} is used to indicate that
      the rule applies to all verbs.}
    \item{\code{object}:}{Object of class \code{"character"} giving the
      value of the \code{object(\linkS4class{Event})} to which the rule
      applies.  The special value \dQuote{ANY} is used to indicate that
      the rule applies to all verbs.}
    \item{\code{ruleType}:}{Object of class \code{"character"} giving
      the type of the rule.  See the \sQuote{Rule Types} section. }
    \item{\code{priority}:}{Object of class \code{"numeric"} providing a
      partial ordering on the execution sequence of rules.  See
      \sQuote{Rule Applicability and Sequencing} section.}
    \item{\code{condition}:}{Object of class \code{"list"} which
      provides a test for when the rule runs.  This should be expressed
      as a set of restrictions on the fields of the
      \code{\linkS4class{Status}} and \code{\linkS4class{Event}}
      classes.  The syntax for this field is described in the
      \sQuote{Rule Condition} section.}
    \item{\code{predicate}:}{Object of class \code{"list"} which
      describes the action to be performed modifying the state of the
      \code{\linkS4class{Status}} object if the condition is satisfied.
      The syntax for this field is described in the
      \sQuote{Rule Condition} section.}
  }
}
\section{Rule Types}{

  There are five types of rules, which are run in the following
  sequence:
  \enumerate{
    \item{\dQuote{Status} Rules.  These rules should have predicates
    which set flag variables and manipulate timers.  These rules are
    run first.}
    \item{\dQuote{Observable} Rules.  These rules should have predicates
    that set observable values, they are run immediately after the
    state rules.}
    \item{\dQuote{Context} Rules.  These rules return a new value for
    the \emph{context} field, if this needs to be changed.  These
    are run until either the set of context rules is exhausted or one
    of the rules returns a value other than the current context.}
    \item{\dQuote{Trigger} Rules.  These rules have a special predicate
    which sends a message to a process listening to the EIP.  These
    rules are given both the old and new context values as often they
    will trigger when the context changes.}
    \item{\dQuote{Reset} Rules.  These rules run only if the context
    changes.  They are used to reset values of various timers and
    flags that should be reset for the new context.}
  }

  Note that the \code{ruleType} field should be one of the five keywords
  \dQuote{Status}, \dQuote{Observable}, \dQuote{Context},
  \dQuote{Trigger} or \dQuote{Reset}. 

  The \code{\linkS4class{EIEngine}} runs these rules in sequence.

  First the status rules are run followed by the observable rules.  Although
  it is suggested that status rules be used to change flags and timers
  and observable rules to change observables (see
  \code{\linkS4class{Status}}), these rules are not strictly enforced.
  It is however, guarenteed that status rules will be run before
  observable rules so that the status rule can be used to calculate
  intermediate variables which are used in calculating the final
  observables. 

  After the observables have been updated, the context rules are run to
  find out if the context is changed.  The context rules are run until
  the value of \code{context(\linkS4class{Status})} is not equal to the
  value of \code{oldContext(\linkS4class{Status})}.

  Next the trigger rules are run.  These cause the
  \code{\linkS4class{EIEngine}} to send a \code{\link[Proc4]{P4Message}}
  to registered listeners.  The primary use is to inform the evidence
  accumulation engine that new observables are available.

  Finally, if \code{oldContext(\linkS4class{Status})} is not equal to
  \code{context(\linkS4class{Status})}, the reset rules are run to reset
  the values of timers and counters for the new context type.  After this
  time, the value of \code{oldContext(\linkS4class{Status})} is set to
  \code{context(\linkS4class{Status})} and the \code{EIEngine} is ready
  to process the next event.
    
}
\section{Rule Selection and Sequencing}{

  The \code{\linkS4class{EIEngine}} applies the rules in five rounds
  according to the rule types.  In each round, the
  \code{\linkS4class{RuleTable}} is searched to find all applicable
  rules.  A rule is applicable if all of the following conditions are
  met: 
  \enumerate{
    \item{The value of \code{ruleType(Rule)} matches the current round.}
    \item{The value of \code{verb(Rule)} is equal to the value of
      \code{verb(Event)} or to the special value \dQuote{ANY}.}
    \item{The value of \code{object(Rule)} is equal to the value of
      \code{object(Event)} or to the special value \dQuote{ANY}.}
    \item{The value of \code{object(Rule)} is equal to the value of
      \code{context(Status)}, is equal to the name of a group context to
      which \code{context(Status)} belongs (see
      \code{\link{applicableContexts}}), or is equal to the special value
      \dQuote{ANY}.}
  }

  Any rule which satisfies these four conditions is consider
  applicable.  The \code{\linkS4class{EIEngine}} checks the
  \code{condition} of all applicable rules, and if the condition is
  true runs the \code{predicate}.  The exception is the context rule
  round, in which the rules are run until the first time the condition
  is satisfied (or more precisely until the value of
  \code{context(\linkS4class{Status})} changes).

  Note that the sequence of rules within a round is arbitrary.  In some
  cases, there will be order dependencies among rules run during the
  same round.  The \code{priority} field of the rules is used to resolve
  potential conflicts of this sort.  The \code{\linkS4class{EIEngine}}
  sorts the rules by priority before checking them.  Rules with lower
  priority are always run before rules with higher priority, but the
  sequence of rules with the same priority is arbitrary.

}
\section{Referencing fields of the \code{Status} and \code{Event}
  objects.}{

  Both conditions and predicates need to reference fields in the
  \code{\linkS4class{Status}} and \code{\linkS4class{Event}}
  objects.  This is done using strings which use the dot notation
  (similar to javascript referencing in JSON documents) to reference
  fields in the two objects.  Thus, \code{event.data.}\emph{field}
  references a field named \dQuote{field} in the
  \code{details(\linkS4class{Event})}.  The following table gives useful
  dot notation references:
  \itemize{
    \item{\code{state.context} The current context that the state
      object is in.}
    \item{\code{state.oldContext} The the context of the state at
      the end of the previous event.  In particular, this can be
      compared to the context to check if the context has changed as a
      result of the event.} 
    \item{\code{state.observables.}\emph{field} The value of the
      observable named \emph{field}.}
    \item{\code{state.timers.}\emph{field} The the timer named
      \emph{field}.  Note that
      \code{state.timers.}\emph{field}\code{.time} or \code{.value} refers
      to the current elapsed time of the timer, and
      \code{state.timers.}\emph{field}\code{.run} or \code{.running} is
      a logical value which refers to whether or not the timer is running.}
    \item{\code{state.flags.}\emph{field} The value of the
      observable named \emph{field}.}
    \item{\code{event.verb} The verb associated with the current event.}
    \item{\code{event.object} The object associated with the current event.}
    \item{\code{event.timestamp} The time at which the event
      occurred.}
    \item{\code{event.data.}\emph{field} The value of the
      extra data field named \emph{field}.}
  }

  In each of these cases, \emph{field} refers to the name of a field in
  one of the collections in the \code{\linkS4class{Event}} or
  \code{\linkS4class{Status}} object being processed.  If the referenced
  field is a list, then the if the field reference is of the form
  \emph{field}\code{.}\emph{component} then the named component of the
  list is referenced.  If the list structure itself contains lists as
  elements, then multiple \sQuote{.}s can be used to reference the
  nested fields.  In this respect, the \sQuote{.} operator performs
  similarly to the S \sQuote{$} operator.

  If the field references a character or numeric vector, then the
  \sQuote{[]} operator can be used to reference elements of that
  vector.  Thus \code{status.flags.agents[3]} references the third
  element of a vector called \sQuote{agents} found in the flags
  collection of the status.

  The functions \code{\link{getJS}} and \code{\link{setJS}} are used to
  access the fields, and the help for those functions contains a number
  of examples.

}
\section{Rule Conditions}{

  The syntax for the condition part of the rule resembles the query
  lanugage used in the Mongo database (MongoDB, 2018).  There are two
  minor differences:  first the syntax uses R lists and vectors rather
  than JSON objects and second the \sQuote{$} operators are replaced
  with \sQuote{?} operators.

  In general, each element of the list should have the form
  \emph{field}\code{=c(}\emph{?op}\code{=}\emph{arg}\code{)}.  In this
  expression, \emph{field} references a field of either the
  \code{\linkS4class{Status}} or \code{\linkS4class{EIEngine}} (see
  sQuote{Dot Notation} section above), \emph{?op} is one of the test
  operators below, and the argument \emph{arg} is a litteral value
  (which could be a list) or a character string in dot notation
  referencing a field of either the \code{Status} or \code{Event}.  If
  \emph{?op} is omitted, it is taken as equals if \emph{arg} is a scalar
  and \emph{?in} if value is a vector.  For more complex queries where
  \code{arg} is a more complex expresison, the \code{c()} function is
  replaced with \code{list()}.

  See \link{Conditions} for a list of supported condition operators.

}
\section{Rule Predicates}{

  The syntax for the predicate of the rule resembles the database
  update operations used in the Mongo database (MongoDB, 2018).  There are
  two minor differences:  first the syntax uses R lists and vectors rather
  than JSON objects and second the \sQuote{$} operators are replaced
  with \sQuote{!} operators.

  The general form of a predicate expression is
  \emph{!op}\code{=list(}\emph{field}\code{=}\emph{arg}\code{)}.  Here
  \emph{!op} is one of the operations described below, \emph{field} is
  the name of a field in the \code{\linkS4class{Status}} object, and the
  argument \emph{arg} is either a literal value or a character scalar giving
  the name of a field of either the \code{\linkS4class{Status}} or
  \code{\linkS4class{Event}} in dot notation.

  See \link{Predicates} a list of supported operations and  more
  information about predicate handling. 

  
}
\section{Rule Testing}{

  The functions \code{\link{testQuery}} and
  \code{\link{testQueryScript}} can be used to test that rule conditions
  function properly.

  The functions \code{\link{testPredicate}} and
  \code{\link{testPredicateScript}} can be used to test that rule conditions
  function properly.

  The functions \code{\link{testRule}} and \code{\link{testRuleScript}}
  can be used to test that rule conditions and predicates function
  properly together.

  
}
\section{Methods}{
  \describe{
    \item{as.jlist}{\code{signature(obj = "Rule", ml = "list")}: helper
      function for coverting the rule into a JSON object, see
      \code{\link[Proc4]{as.json}}.} 
    \item{condition}{\code{signature(x = "Rule")}: Returns a list giving
      the conditions for the rule.  }
    \item{app}{\code{signature(x = "Rule")}: Returns a character
      scalar giving the ID of the application this rule belongs to.}
    \item{context}{\code{signature(x = "Rule")}: Returns a character
      scalar giving the ID of the context or context group to which the
      rule is applicable. } 
    \item{object}{\code{signature(x = "Rule")}: Returns a character
      scalar giving the object to which the rule is applicable. }
    \item{predicate}{\code{signature(x = "Rule")}: Returns a list giving
      the predicate for the rule.}
    \item{ruleType}{\code{signature(x = "Rule")}: Returns a character
      scalar giving the type of the rule.}
    \item{predicate}{\code{signature(x = "Rule")}: Returns a number giving
      the priority for the rule; lower numbers are higher priority.}
    \item{verb}{\code{signature(x = "Rule")}: Returns a character
      scalar giving the verb to which the rule is applicable. }
    \item{show}{\code{signature(object = "Rule")}: Prints rule object. }
    \item{toString}{\code{signature(x = "Rule")}: Produces <>
      representation of object. } 
    \item{all.equal.Rule}{\code{(target, current, ..., checkTimestamp=FALSE,
        check_ids=TRUE)}:  (S3 method) Checks for equality.}
  }
}
\references{

  The document \dQuote{Rules Of Evidence} gives extensive documentation
  for the rule system.
  \url{https://pluto.coe.fsu.edu/Proc4/RulesOfEvidence.pdf}. 

  Almond, R. G., Steinberg, L. S., and Mislevy, R.J. (2002).  Enhancing
  the design and delivery of Assessment Systems:  A Four-Process
  Architecture.  \emph{Journal of Technology, Learning, and Assessment},
  \bold{1},
  \url{http://ejournals.bc.edu/ojs/index.php/jtla/article/view/1671}. 

  Almond, R. G., Shute, V. J., Tingir, S. and Rahimi,
  S. (2018). Identifying Observable Outcomes in Game-Based Assessments.
  Talk given at the \emph{2018 Maryland Assessment Research
  Conference}.  Slides:
  \url{https://education.umd.edu/file/11333/download?token=kmOIVIwi},
  Video: \url{https://pluto.coe.fsu.edu/Proc4/Almond-Marc18.mp4}.

  MongoDB, Inc. (2018).  \emph{The MongoDB 4.0 Manual}.
  \url{https://docs.mongodb.com/manual/}.
}
\author{Russell Almond}
\seealso{

  \link{Conditions} and \link{Predicates} each have detailed
  descriptions. The functions \code{\link{checkCondition}} and
  \code{\link{executePredicate}} run the condition and predicate parts
  of the rule.  The functions \code{\link{runRule}} and
  \code{\link{runTRule}} run the indivual rules, and the functions
  \code{\link{runTriggerRules}}, \code{\link{runStatusRules}},
  \code{\link{runObservableRules}}, \code{\link{runResetRules}},
  and \code{\link{runContextRules}} run sets of rules.

  The functions \code{\link{testQuery}} and \code{\link{testQueryScript}}
  can be used to test that rule conditions function properly.
  The functions \code{\link{testPredicate}} and
  \code{\link{testPredicateScript}} 
  can be used to test that predicates function properly.
  The functions \code{\link{testRule}} and \code{\link{testRuleScript}}
  can be used to test that rule conditions and predicates function
  properly together.  The class \code{\linkS4class{RuleTest}} stores a
  rule test. 
  
  Other classes in the EIEvent system:
  \code{\linkS4class{EIEngine}}, \code{\linkS4class{Context}},
  \code{\linkS4class{Status}}, \code{\linkS4class{Event}},
  \code{\linkS4class{RuleTable}}.

  Methods for working with Rules:
  \code{\link{Rule}}, \code{\link{parseRule}}, \code{\link{ruleType}},
  \code{\link{priority}}, \code{\link{condition}},
  \code{\link{predicate}}, \code{\link{verb}}, \code{\link{object}},
  \code{\link{context}}, \code{\link{name}}, \code{\link{doc}}

}
\examples{
showClass("Rule")
}
\keyword{classes}
