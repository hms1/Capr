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

#Attributes are described in a separate file. Here we distinguish the remaining subclasses.

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
