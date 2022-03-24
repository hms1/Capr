# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of Capr
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


# Circe Classes Description----------------

# These are the base circe classes that come from circe.
# Despite being a circe class, concept sets are placed in a different section
#(LowLevelConceptSetClasses.R) because they have a whole host of actions and
# properties beyond the cohort definition.
# See https://github.com/OHDSI/circe-be for details
#

# Circe classes are as follows:

# 1) Query - mechanism to locate observational events in the CDM
# 2) Count - mechanism to count instances of a query across a period of time
# 3) Group - provide the mechanism to apply boolean logic to counts


# Within these basic classes there are further subclasses:
# 1) Attributes - modifiers to queries that distinguish subsections of observational events. From
# here there are 6 types of attributes:
#   a) DateOp - modifier of query based on a range specified by a date
#   b) NumericOp - modifier of query based on a range specified by a value
#   c) SourceConcept - modifier of query that uses source concepts rather than standard
#   d) Logic - modifier of query based on presence or absence (TRUE OR FALSE)
#   e) Concept - modifier of query based on a selection of concepts
#   f) Correlated - modifier of query based on a group
# 2) Window - a period of time from an index where the instances of a query is counted
# 2a) ObservationWindow - special class for the index event
# 3) Timeline - parent class of a window that provides the full specification of time of observation
# 4) Occurrence - the number of instances a query occurs
# 5) Expression Type - boolean logic of counts

# Attributes are described in a separate file. Here we distinguish the remaining subclasses.

## Circe Helpers -----------------------------------------

#' An S4 class for a Window
#'
#' A window class provides details on the end points of the timeline
#'
#' @slot Event a character string either EventStarts or EventEnds. Identifies the point of reference for the window
#' @slot Start a list containing the days and coefficient for the start of the window
#' @slot End A list containing the days and coefficient for the end of the window
#' @slot Index A character string either IndexStartDate or IndexEndDate Identifies where the index is relative to the window
setClass("Window",
         slots = c(Event = "character",
                   Start = "list",
                   End = "list",
                   Index = "character"))

setValidity("Window", function(object) {
  # TODO Create Window validation rules
  TRUE
})

#' Show statements of capr objects
#'
#' These functions print the capr object to console in a readable format
#'
#' @param object the object to show
#' @return a console print of the object
#' @rdname show-method
#' @aliases show
#' @aliases show,Window-method
setMethod("show", "Window", function(object) {
  cat(object@Event, object@Start$Days, "Days", object@Start$Coeff, "and",
      object@End$Days, "Days", object@End$Coeff, object@Index)
})

#' An S4 class for Timeline
#'
#'The timeline class provides context to when the criteria must be observed in a person timeline to pretain to the expression
#'
#' @slot StartWindow a window class object identifying the start window
#' @slot EndWindow a window class object ifentifying the end window (optional)
#' @slot RestrictVisit a logic toggle where TRUE restricts to the same visit
#' @slot IgnoreObservationPeriod a logic toggle where TRUE allows events outside the observation period
setClass("Timeline",
         slots = c(StartWindow = "Window",
                   EndWindow = "Window",
                   RestrictVisit = "logical",
                   IgnoreObservationPeriod = "logical"))

setValidity("Timeline", function(object) {
  # TODO create Timeline validation rules
  TRUE
})

#' @rdname show-method
#' @aliases show,Timeline-method
setMethod("show", "Timeline", function(object) {
  cat("Timeline", "\n")
  show(object@StartWindow)
  cat("\n")
  if (length(object@EndWindow@Start) == 2){
    show(object@EndWindow)
    cat("\n")
  }
  if(object@RestrictVisit) {
    cat("at the same visit as cohort entry")
    cat("\n")
  }
  if (object@IgnoreObservationPeriod) {
    cat("allow events outside observation period")
    cat("\n")
  }
})

#' An S4 class for Occurrence
#'
#' The Occurrence class provides logic on the number of criterias that most be true in a person for them to be contained in
#' the expression
#'
#' @slot Type a character string of either at most, at least, or exactly providing context to the number of occurrences
#' @slot Count an integer value that provides the number of occurrences
#' @slot isDistinct a logic toggle where if TRUE only counts distinct occurrences
setClass("Occurrence", #a class counting the number of occurrences of the event
         slots = c(Type = "character",
                   Count = "integer",
                   isDistinct = "logical")
         # TODO add prototype
)

setValidity("Occurrence", function(object) {
  # TODO Occurrence validity checks
  TRUE
})

#' @rdname show-method
#' @aliases show,Occurrence-method
setMethod("show", "Occurrence", function(object) {
  cat(utils::str(object))
})

#' An S4 class for Expression type
#'
#' An expression type quantifies the number of criteria's needed to set as restriction. Types include:
#' All, Any, at least and at most. If the expression type is at least or at most a count is required
#' to express the type
#'
#' @slot Type boolean operator for the number of items in group to include. all, any, at most and at least
#' @slot Count the number of criteria's needed for restriction. If Type is ALL or ANY this value is NA
setClass("ExpressionType",
         slots = c(Type = "character",
                   Count = "integer"),
         prototype = list(Type = "ALL",
                          Count = NA_integer_)
)

setValidity("ExpressionType", function(object) {
  # TODO ExpressionType validity checks
  TRUE
})

#' An S4 class for ObservationWindow
#'
#' A class designating an amount of time necessary for an initial event to be recorded
#'
#' @slot PriorDays minimal amount of time before event for it to be recorded
#' @slot PostDays minimal amount of time after an event for it to be recorded
setClass("ObservationWindow",
         slots = c(PriorDays = "integer",
                   PostDays = "integer"))

setValidity("ObservationWindow", function(object) {
  # TODO create ObservationWindow validation rules
  TRUE
})

#' @rdname show-method
#' @aliases show,ObservationWindow-method
setMethod("show", "ObservationWindow", function(object) {
  cat("Observation Window:", "\n")
  cat("Continous observation of at least", object@PriorDays, "days prior and",
      object@PostDays, "days after event index date")
})


# Circe Classes ------------------------------

#' An S4 class for a Query
#'
#' A query is a mechanism to locate observational events in the OMOP CDM. The query finds observational
#' data from the clinical tables of the cdm: observation period, visit occurrence, condition occurrence,
#' drug exposure, procedure occurrence, device, measurement, observation, death, and specimen. You can also
#' create queries based on derived elements such as drug era, dose era, and condition era.
#'
#' There are three aspects to a query: a domain, a codeset id and attributes. The domain specifies the
#' clinical table where we look for concepts. The codeset id links the query to a concept set that
#' specifies all concepts used to specify the clinical construct. In Capr the codeset id is a guid that uniquely
#' identifies a concept set expression in a component. Attributes are optional to a query as they modify the query
#' to distinguish a subselection of observational events.
#'
#' To create a query class, use the query<Domain> commands and apply a concept set expression expression
#' to attach to the query. You may also attach a list of attributes to add to the query.
#'
#' @slot Domain the domain where the concepts can be found
#' @slot CodesetId the id that matches the concept set expression
#' @slot Attributes a list of attributes that modify the query with more information
setClass("Query",
         slots = c(Domain = "character",
                   CodesetId = "character",
                   Attributes = "list"),
         prototype = list(Domain = NA_character_,
                          CodesetId = NA_character_,
                          Attributes = list())
)

setValidity("Query", function(object) {
  # TODO Query validation rules
  TRUE
})

#' @rdname show-method
#' @aliases show,Query-method
setMethod("show", "Query", function(object) {
  cat("Query", "\n")
  cat("Domain:", object@Domain, "\n")
  cat("CodesetId:", object@CodesetId, "\n")
  cat("Attributes:", "\n")
  if (length(object@Attributes) > 0) {
    for (i in seq_along(object@Attributes)) {
      cat("\t",paste0(i, ") "))
      show(object@Attributes[[i]])
      cat("\n")
    }
  } else {
    cat("None", "\n")
  }
})



#' An S4 class for a Count
#'
#' A count class provides a number of occurrences of the query and the timeline that it happens
#'
#' @slot Criteria a query class object
#' @slot Timeline a timeline class object
#' @slot Occurrence an occurrence class object
setClass(
  "Count",
  slots = c(Criteria = "Query",
            Timeline = "Timeline",
            Occurrence = "Occurrence"))

setValidity("Count", function(object) {
  # TODO Count Validity checks
  TRUE
})

#' @rdname show-method
#' @aliases show,Count-method
setMethod("show", "Count", function(object) {
  cat("Count", object@Occurrence@Type, object@Occurrence@Count)
  if (object@Occurrence@isDistinct) {
    cat(" distinct occurrence(s) of")
  } else {
    cat(" occurrence(s) of")
  }
  lineBreak(4)
  show(object@Criteria)
  lineBreak(4)
  show(object@Timeline)
})

#' An S4 class for Group
#'
#' TODO clarify the description of a group.
#' A group that bundles criteria together identifying an event
#'
#' @slot Type a expression type class Boolean for the number of items to make the group count
#' @slot CriteriaList a list of items (counts and queries) that would identify a medical event
#' @slot DemographicCriteriaList a list of demographic attributes that could identify a population
#' @slot Groups a list of other groups that are contained within a group
setClass("Group",
         slots = c(Type = 'ExpressionType',
                   CriteriaList = 'list',
                   DemographicCriteriaList = 'list',
                   Groups = 'list'))

setValidity("Group", function(object) {
  # TODO Group validity checks
  TRUE
})

#' @rdname show-method
#' @aliases show,Group-method
setMethod("show", "Group", function(object) {
  ty <- object@Type@Type
  if (ty == "AT_LEAST" | ty == "AT_MOST") {
    cat("Having",tolower(ty), object@Type@Count, "of the following criteria")
  } else{
    cat("Having",tolower(ty), "of the following criteria")
  }
  lineBreak(1)
  if (length(object@CriteriaList) > 0) {
    cat("Criteria List")
    lineBreak(3)
    for (i in seq_along(object@CriteriaList)) {
      show(object@CriteriaList[[i]])
    }
  }
  if (length(object@DemographicCriteriaList) > 0) {
    cat("Demographic Criteria List")
    lineBreak(3)
    for (i in seq_along(object@DemographicCriteriaList)) {
      show(object@DemographicCriteriaList[[i]])
    }
  }
  if (length(object@Groups) > 0) {
    cat("Groups List")
    lineBreak(3)
    for (i in seq_along(object@Groups)) {
      show(object@Groups[[i]])
    }
  }
})
