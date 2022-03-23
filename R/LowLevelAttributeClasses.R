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

#' An S4 class for an Op Date Attribute
#'
#' An operator attribute meaning it has some value with a boolean operator
#'
#' @slot Name the name of the attribute
#' @slot Op the operator gt,lt,gte,lte,eq,neq,bt,!bt
#' @slot Contents the contents of the attribute as a list. includes the value and the extent
setClass("OpDateAttribute",
         slots = c(Name = "character",
                   Op = "character",
                   Contents = "list"))

setValidity("OpAttribute", function(object) {
  # TODO Create OpAttribute validity checks
  TRUE
})

#' @rdname show-method
#' @aliases show,OpAttribute-method
setMethod("show", "OpAttribute", function(object) {
  cat(paste0(methods::is(object), ":"),object@Name, "==>")
  op <- object@Op
  if (op == "bt" | op == "!bt") {
    cat("", op, object@Contents$Value, "and", object@Contents$Extent)
  } else{
    cat("", op, object@Contents$Value)
  }
})

#' An S4 class for an Op Numeric Attribute
#'
#' An operator attribute meaning it has some value with a boolean operator
#'
#' @slot Name the name of the attribute
#' @slot Op the operator gt,lt,gte,lte,eq,neq,bt,!bt
#' @slot Contents the contents of the attribute as a list. includes the value and the extent
setClass("OpNumericAttribute",
         slots = c(Name = "character",
                   Op = "character",
                   Contents = "list"))

setValidity("OpAttribute", function(object) {
  # TODO Create OpAttribute validity checks
  TRUE
})

#' @rdname show-method
#' @aliases show,OpAttribute-method
setMethod("show", "OpAttribute", function(object) {
  cat(paste0(methods::is(object), ":"),object@Name, "==>")
  op <- object@Op
  if (op == "bt" | op == "!bt") {
    cat("", op, object@Contents$Value, "and", object@Contents$Extent)
  } else{
    cat("", op, object@Contents$Value)
  }
})

#' An S4 class for SourceConceptAttribute
#'
#' An attribute that looks at utilizing the source concepts instead of standard concepts
#'
#' @slot Name name of the attribute
#' @slot SourceCodesetId a source concept id, connection to concept set expression
setClass("SourceConceptAttribute",
         slots = c(Name = "character",
                   SourceCodesetId = "character"))

# TODO add validity check and show method

#' An S4 class for Concept Attribute
#'
#' A concept attribute, using concepts to identify the attribute like a gender or race etc
#'
#' @slot Name the name of the attribute
#' @slot Concepts a list containing the concepts used to identify the attribute
setClass("ConceptAttribute",
         slots = c(Name = "character",
                   Concepts = "list"))

# TODO add validity check and show method

#' An S4 class for CorrelatedCriteriaAttribute
#'
#' A group attribute that is nested within a query.
#'
#' @slot Name name of the attribute
#' @slot Group a group class object for the attribute
setClass("CorrelatedCriteriaAttribute",#
         slots = c(Name = "character",#
                   Group = "Group"))#

# TODO add validity check and show method

#' An S4 class for Logic Attribute
#'
#' This class creates a logic attribute which says either true or false if the name of the attribute
#' is maintained
#'
#' @slot Name a name of the attribute
#' @slot Logic TRUE or FALSE for this attribute
setClass("LogicAttribute",
         slots = c(Name = "character",
                   Logic = "logical"))#

# TODO add validity check and show method
