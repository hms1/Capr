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

# Component Class ----------------------------


#' An S4 class for Limit
#'
#' A class designating a limit of events per person Types include: all first last
#'
#' @slot Type how to limit events per person: all, first, or last
setClass("Limit",
         slots = c(Type = "character"),
         prototype = list(Type = "first"))

setValidity("Limit", function(object) {
  if(length(object@Type) == 1 && object@Type %in% c("all", "first", "last")) {
    "Limit Type must be one of 'all', 'fist', last'"
  } else {
    TRUE
  }
})

#' @rdname show-method
#' @aliases show,Limit-method
setMethod("show", "Limit", function(object) {
  event <- ifelse(object@Type == "all", "events", "event")
  cat(paste("Limit to", object@Type, event, "per person."))
})



#' An S4 class for Component MetaData
#' TODO confirm possible values for ComponentType. Should Index be included as a slot?
#' @slot ComponentType name of component class (this is formally defined) Possible values are...
#' @slot Name name for component customized by user
#' @slot Description description of the component
#' @slot Index A character string either IndexStartDate or IndexEndDate Identifies where the index is relative to the window
setClass("MetaData",
         slots = c(ComponentType = "character",
                   Name = "character",
                   Description = "character")
         # TODO add prototype
)

setValidity("MetaData", function(object) {
  validTypes <- c("PrimaryCriteria", "AdditionalCriteria", "InclusionRules", "EndStrategy", "CensoringCriteria", "CohortEra",
                  "Query", "Count", "Group", "ConceptSetExpression", "Attribute", "Empty")
  if (!(object@ComponentType %in% validTypes)) {
    paste0("ComponentType must be one of '", paste(validTypes, collapse = ", "), "'", ", not '", object@ComponentType, "'")
    # TODO add additional checks. Should we check length of name and Description. Should we use the argument checking package?
  } else {
    TRUE
  }
})

#' @rdname show-method
#' @aliases show,MetaData-method
setMethod("show", "MetaData", function(object) {
  cat(utils::str(object))
})

# Capr Component ----------------------------------------------------------

#' An S4 class for a cohort definition component
#'
#' This class is an flexible container used to store the component parts of cohort definition allowing us to maintain information
#' in smaller parts that remain relevant in isolation. The structure of a Circe cohort definition relies on a concept set
#' table that stores information for queries. In each cohort component an internal reference id is used to maintain
#' consistency between the expression of the cohort criteria and the actionable concepts. The component container
#' bundles the concept set expression and the criteria expression into one object that is saveable and inheritable.
#' Smaller classes are stored within the container and when they are converted into a superior class the component container
#' is modified but the previous information is kept in tact. A component consists of 4 parts: MetaData stores
#' the name, description and the ComponentType. The ComponentType identifies what kind of component one is using. Next
#' the criteriaExpression stores any information about the deployment of the medical concept. This includes queries, counts,
#' groups, attributes and other structures that detail the information of the specific component class. The limit
#' is a slot that specifies the limit of entry for person events, e.g. the first event, all events, or last event for
#' the criteriaExpression. Finally the ConceptSetExpression slot holds the concepts relevant
#' to the criteria expression and their unique identifies. A Component object can be saved as a json file or loaded back into its s4 class.
#' In some cases components can be nested inside other components
#' TODO Explain the possible nesting structures that can exist. Question: why does metaData get its own class but other slots do not?
#'
#' @slot MetaData meta information about the object
#' @slot CriteriaExpression a list of criteria that is in the object
#' @slot Limit a list containing any limits
#' @slot ConceptSetExpression a list containing any concept sets
setClass("Component",
         slots = c(MetaData = "MetaData",
                   CriteriaExpression = "list",
                   Limit = "list",
                   ConceptSetExpression = "list"),
         # TODO improve prototype
         prototype = list(MetaData = new("MetaData"),
                          CriteriaExpression = list(),
                          Limit = list(),
                          ConceptSetExpression = list())
)

setValidity("Component", function(object) {
  # TODO fill in component validity logic
  TRUE
})

#' @rdname show-method
#' @aliases show,Component-method
setMethod("show", "Component", function(object) {

  name <- ifelse(nchar(object@MetaData@Name) > 0, paste(":", object@MetaData@Name), "")

  cli::cat_line(cli::rule(paste0(object@MetaData@ComponentType, name)))
  cli::cat_line("Critera Expression")
  cli::cat_line(utils::str(object@CriteriaExpression, max.level = 2))

  print(object@Limit)
  cli::cat_line("Concept Set Expression")
  cli::cat_line(utils::str(object@ConceptSetExpression, max.level = 2))
})


# TODO combine print methods for component into a single method
#' Show Contents of a Component
#'
#' This function prints the contents of a component. Note 1/27/21 attributes and some other s4 classes
#' need to be implemented
#'
#' param showFullConceptSetExpressions T/F options to include full details of concept expressions
# printComponent <- function(x, showFullConceptSetExpressions = FALSE){
#   lineBreak(1)
#   if(methods::is(x) != "Component"){
#     stop("The object is not a component")
#   }
#   cat("Component Type:", x@MetaData@ComponentType, "\n")
#   cat("Name:", x@MetaData@Name, "\n")
#   cat("Description:", x@MetaData@Description)
#   lineBreak(2)
#   if (length(x@CriteriaExpression) > 0) {
#     if (componentType(x) == "PrimaryCriteria"){
#       cat("Criteria List")
#       for (i in seq_along(x@CriteriaExpression$CriteriaList)) {
#         lineBreak(3)
#         cat(paste0(i, ") "))
#         printCapr(x@CriteriaExpression$CriteriaList[[i]])
#       }
#       lineBreak(2)
#       printCapr(x@CriteriaExpression$ObservationWindow)
#     } else if (componentType(x) == "InclusionRules") {
#       for (i in seq_along(x@CriteriaExpression)) {
#         cat(paste0(i, ") "))
#         printComponent(x@CriteriaExpression[[i]], showFullConceptSetExpressions = showFullConceptSetExpressions)
#       }
#     } else {
#       for (i in seq_along(x@CriteriaExpression)) {
#         cat(paste0("Criteria ", i, ")"), "\n")
#         printCapr(x@CriteriaExpression[[i]])
#       }
#     }
#   }
#   lineBreak(2)
#   if (length(x@Limit) > 0) {
#     tt <- paste0(names(x@Limit),":")
#     lim <- x@Limit[[1]]@Type
#     cat(tt, lim)
#     lineBreak(2)
#   }
#   cat("Concept Set Expressions")
#   if (length(x@ConceptSetExpression) > 0) {
#     for (i in seq_along(x@ConceptSetExpression)) {
#       lineBreak(3)
#       cat(paste0(i, ") "))
#       if (showFullConceptSetExpressions) {
#         printCapr(x@ConceptSetExpression[[i]])
#       } else {
#         cat(x@ConceptSetExpression[[i]]@Name, "\n")
#         cat("CodesetId:", x@ConceptSetExpression[[i]]@id)
#       }
#     }
#   } else {
#     cat("None")
#   }
#   lineBreak(1)
# }

# TODO add unit tests for validity and print methods
