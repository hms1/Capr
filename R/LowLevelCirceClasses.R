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

#Despite specifying the subclasses in this section. They are elaborated in LowLevelSubClasses.R



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
