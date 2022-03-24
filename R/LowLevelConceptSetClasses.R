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

# Concept Sets  ------------------------------------------

#' An S4 class for a ConceptSet
#'
#' A concept class contains all the information about the concept from the OMOP voabulary
#'
#' @slot CONCEPT_ID the id of the concept
#' @slot CONCEPT_NAME the name of the concept
#' @slot STANDARD_CONCEPT whether the cncept is standard, single letter
#' @slot STANDARD_CONCEPT_CAPTION whether the concept is standard full phrase
#' @slot INVALID_REASON Whether the concept is invalid single letter
#' @slot INVALID_REASON_CAPTION whether the concept is invalid standard phrase
#' @slot CONCEPT_CODE the original code of the concept from its vocabulary
#' @slot DOMAIN_ID the domain of the concept
#' @slot VOCABULARY_ID the name of the vocabulary
#' @slot CONCEPT_CLASS_ID type of concept class
setClass("Concept",
         slots = c(CONCEPT_ID = "integer",
                   CONCEPT_NAME = "character",
                   STANDARD_CONCEPT = "character",
                   STANDARD_CONCEPT_CAPTION = "character",
                   INVALID_REASON = "character",
                   INVALID_REASON_CAPTION = "character",
                   CONCEPT_CODE = "character",
                   DOMAIN_ID = "character",
                   VOCABULARY_ID = "character",
                   CONCEPT_CLASS_ID = "character")
         # TODO write prototype
)

setValidity("Concept", function(object) {
  # TODO
})

#' @rdname show-method
#' @aliases show,Concept-method
setMethod("show", "Concept", function(object) {
  nm <- methods::slotNames(methods::is(object))
  concept <- unname(sapply(nm, slot, object = object))
  cid <- paste("conceptId:", concept[1])
  cname <- paste("conceptName:", concept[2])
  cstd <- paste("standardConcept:", concept[3])
  cdom <- paste("domainId:", concept[8])
  cat("",cid, "\n", cname, "\n", cstd,"\n", cdom,"\n")
})

#' An S4 class for ConceptSetItem
#'
#' A class that provides information on the mapping of the concept
#'
#' @slot Concept a concept class object
#' @slot isExcluded toggle if want to exclude the concept
#' @slot includeDescendants toggle if want to include descendants
#' @slot includeMapped toggle if want to include map
setClass("ConceptSetItem",
         slots = c(Concept = "Concept",
                   isExcluded = "logical",
                   includeDescendants = "logical",
                   includeMapped = "logical"),
         prototype = list(Concept = new("Concept"),
                          isExcluded = FALSE,
                          includeDescendants = TRUE,
                          includeMapped = FALSE)
)

setValidity("ConceptSetItem", function(object) {
  # TODO fill in validation logic
  TRUE
})


# print a checkmark
# stringi::stri_unescape_unicode("\\u2714")
#' @rdname show-method
#' @aliases show,ConceptSetItem-method
#' @export
setMethod("show", "ConceptSetItem", function(object) {
  domains <- paste(unique(vapply(object, function(x) x@Concept@DOMAIN_ID, character(1))), collapse = ", ")
  cli::cat_line(paste(domains, "Concept Set"))
  purrr::walk(object, function(x) {
    x <- x@Concept
    smry <- glue::glue("{x@CONCEPT_ID}: {x@CONCEPT_NAME} ({x@STANDARD_CONCEPT})")
    ie <- ifelse(x@isExcluded, "Excluded", "")
    id <- ifelse(x@includeDescendants, "+Descendants", "")
    im <- ifelse(x@includeMapped, "Mapped", "")
    cli::cat_line(paste(smry, paste(ie,id,im, collapse = ", ")))
  })
})

# This is the method definition from UserPrintFn
# TODO combine two show methods for ConceptSetItem into a single method
# setMethod("show", "ConceptSetItem", function(object){
#   printCapr(object@Concept)
#   ie <- paste("isExcluded:", object@isExcluded)
#   id <- paste("includeDescendants:", object@includeDescendants)
#   im <- paste("includeMapped:", object@includeMapped)
#   cat(paste(" Mapping ==>",ie,id,im), "\n")
# })

#' An S4 class for ConceptSetExpresion
#'
#' A class for the concept set expressions bundles multiple concepts with mapping
#'
#' @slot id an id for the concept set expression to identify within a component
#' @slot Name the name of the concept set expression
#' @slot Expression a list containing expressions. expressions include multiple conceptSetItem objects
setClass("ConceptSetExpression",
         slots = c(id = "character",
                   Name = "character",
                   Expression = "list"),
         prototype = list(id = uuid::UUIDgenerate(),
                          Name = NA_character_,
                          Expression = list()))

setValidity("ConceptSetExpression", function(object) {
  # TODO create validation rules for ConceptSetExpression
  TRUE
})

#' @rdname show-method
#' @aliases show,ConceptSetExpression-method
setMethod("show", "ConceptSetExpression", function(object) {
  cat(object@Name, "\n")
  cat("CodesetId:", object@id,"\n")
  cat("Expression:")
  for (i in seq_along(object@Expression)){
    lineBreak(3)
    cat(paste0("ConceptItem",i), "\n")
    show(object@Expression[[i]])
  }
})
