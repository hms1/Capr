

# make a concept Set from a data frame of concepts
setGeneric('conceptSet',function(x){standardGeneric("conceptSet")})
setMethod("conceptSet", "data.frame",
          function(x) {

            toConcept <- function(ll) {
              ll$INVALID_REASON_CAPTION <- ifelse(is.na(ll$invalidReason),"Valid", "Invalid")
              ll$INVALID_REASON <- ifelse(is.na(ll$invalidReason),"V", ll$invalidReason)
              ll$STANDARD_CONCEPT_CAPTION <- ifelse(is.na(ll$standardConcept),"Non-Standard",
                                                    ifelse(ll$standardConcept =="C", "Classification", "Standard"))
              ll$STANDARD_CONCEPT <- ifelse(is.na(ll$standardConcept), "N", ll$standardConcept)

              new("Concept",
                  CONCEPT_ID = as.integer(ll$conceptId),
                  CONCEPT_NAME = as.character(ll$conceptName),
                  STANDARD_CONCEPT = as.character(ll$STANDARD_CONCEPT),
                  STANDARD_CONCEPT_CAPTION = as.character(ll$STANDARD_CONCEPT_CAPTION),
                  INVALID_REASON = as.character(ll$INVALID_REASON),
                  INVALID_REASON_CAPTION = as.character(ll$INVALID_REASON_CAPTION),
                  CONCEPT_CODE = as.character(ll$conceptCode),
                  DOMAIN_ID =as.character(ll$domainId),
                  VOCABULARY_ID = as.character(ll$vocabularyId),
                  CONCEPT_CLASS_ID = as.character(ll$conceptClassId))
            }

            tt <- apply(x, 1, function(x) toConcept(as.list(x)))
            return(tt)

})

setGeneric('conceptSetItem',function(ConceptSet,
                                     includeDescendants = NULL,
                                     isExcluded = NULL,
                                     includeMapped = NULL){
  standardGeneric("conceptSetItem")
  })
setMethod("conceptSetItem", "list",
          function(ConceptSet, includeDescendants = NULL, isExcluded = NULL, includeMapped = NULL) {

            #check if no logicals entered
            if (all(is.null(includeDescendants), is.null(isExcluded), is.null(includeMapped))) {
              ll <- rep(FALSE, length(ConceptSet))
              #make all null entries false
              includeDescendants <- ll
              isExcluded <- ll
              includeMapped <- ll
            }

            #check if only logic specified for includeDescendants
            if (length(includeDescendants) == length(ConceptSet) & (is.null(isExcluded) & is.null(includeMapped))) {
              ll <- rep(FALSE, length(ConceptSet))
              #make all null entries false
              isExcluded <- ll
              includeMapped <- ll
            }
            #check if only logic specified for isExcluded
            if (length(isExcluded) == length(ConceptSet) & (is.null(includeDescendants) & is.null(includeMapped))) {
              ll <- rep(FALSE, length(ConceptSet))
              #make all null entries false
              includeDescendants <- ll
              includeMapped <- ll
            }
            #check if only logic specified for includeMapped
            if (length(includeMapped) == length(ConceptSet) & (is.null(isExcluded) & is.null(includeDescendants))) {
              ll <- rep(FALSE, length(ConceptSet))
              isExcluded <- ll
              includeDescendants <- ll
            }

            if (length(includeMapped) != length(ConceptSet) | length(isExcluded) != length(ConceptSet) | length(includeDescendants) != length(ConceptSet)) {
              stop("The mapping items must match the number of concepts in the concept set list")
            }

            ll <- list(a = ConceptSet, b = includeDescendants, c = isExcluded, d = includeMapped)
            csi <- purrr::pmap(ll, function(a, b, c, d) {
              new("ConceptSetItem",
                  Concept= a,
                  includeDescendants = b,
                  isExcluded = c,
                  includeMapped = d)
            })

            return(csi)

          })

setGeneric('conceptSetExpression',function(ConceptSet, Name = NULL){
  standardGeneric("conceptSetExpression")
})
setMethod("conceptSetExpression", "list",
          function(ConceptSet, Name) {

            if (is.null(Name)) {
              Name <- "Unnamed Concept Set Expression"
            }

            cse <- new("ConceptSetExpression", Name = Name, Expression = ConceptSet)
            return(cse)
          })



tst <- conceptSet(df) %>%
  conceptSetItem(includeDescendants = c(T,F)) %>%
  conceptSetExpression()
