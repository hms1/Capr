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


# Cohort Definition -------------------------------------------------------

#' An S4 class providing metadata for a CohortDefinition
#'
#' The cohort details do not affect the cohort definition and are for improving
#' human readability only.
#'
#' @slot Name a name for the cohort
#' @slot Description a text field providing an information on the cohort and what it is intended
#' @slot Author who created the cohort
#' @slot cdmVersionRange the range of cdm versions
setClass("CohortDetails",
         slots = c(Name = "character",
                   Description = "character",
                   Author = "character",
                   cdmVersionRange = "character"),
         prototype = list(Name = "New Cohort Definition",
                          Description = "",
                          Author = "",
                          cdmVersionRange = ""))


setValidity("CohortDetails", function(object) {
  if (length(object@Name) != 1) {
    paste("Name must be a character vector of length 1, not", length(object@Name))
  } else if (length(object@Description) != 1) {
    paste("Description must be a character vector of length 1, not", length(object@Description))
  } else if (length(object@Author) != 1) {
    paste("Description must be a character vector of length 1, not", length(object@Author))
  } else if (length(object@cdmVersionRange) != 1) {
    paste("Description must be a character vector of length 1, not", length(object@cdmVersionRange))
  } else if (nchar(object@Name) == 0) {
    "Name cannot be an empty string"
  } else {
    TRUE
  }
})

#' @rdname show-method
#' @aliases show,CohortDetails-method
setMethod("show", "CohortDetails", function(object) {
  nm <- stringr::str_trunc(object@Name, 40)
  aut <- ifelse(nchar(object@Author) > 0, paste0("[", object@Author, "]"), "")
  ver <- ifelse(nchar(object@cdmVersionRange) > 0, paste0("(", object@cdmVersionRange, ")"), "")
  cat(nm, aut, ver)
})

# TODO add unit tests for validity checks
# (object <- new("CohortDetails", Name = letters[1:3]))
# (object <- new("CohortDetails"))
# (object <- new("CohortDetails", Name = "blah"))

#' An S4 class for a Circe Cohort Definition
#'
#' A cohort definition contains information about how to quantify a clinical phenotype.
#' The ultimate purpose of Capr is to allow the creation and manipulation of Circe cohort
#' definitions in R making CohortDefinition its most important class.
#'
#' @slot CohortDetails a cohortDetails object providing meta information about the cohort
#' @slot PrimaryCriteria a component class containing the primary criteria
#' @slot AdditionalCriteria a component class containing the additional criteria
#' @slot InclusionRules a component class containing the Inclusion Rules
#' @slot EndStrategy a component class containing the End Strategy
#' @slot CensoringCriteria a component class containing the censoring criteria
#' @slot CohortEra a component class containing the cohort era
setClass("CohortDefinition",
         slots = c(CohortDetails = "CohortDetails",
                   PrimaryCriteria = "Component",
                   AdditionalCriteria = "Component",
                   InclusionRules = "Component",
                   EndStrategy = "Component",
                   CensoringCriteria ="Component",
                   CohortEra = "Component")
         # TODO add prototype
)

setValidity("CohortDefinition", function(object) {
  # Type checking is automatically enforced
  # TODO implement addtitional validity checks
})

#' @rdname show-method
#' @aliases show,CohortDefinition-method
setMethod("show", "CohortDefinition", function(object) {
  cat(utils::str(object, maxlevel = 3))
  # TODO write a better print method for CohortDefinition
})
